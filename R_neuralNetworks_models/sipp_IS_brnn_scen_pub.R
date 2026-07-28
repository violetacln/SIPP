###############################################################################
### SIPP: Stochastic Integrated Population Projections                      ###
### Using brnn, Bootstrapping, and Statistics Iceland Open Data (pxweb)     ###
###############################################################################


#############********************************##############
################### works very well! ######################
#############********************************##############


# ---------------------------------------------------------
# 0. Dependencies
# ---------------------------------------------------------
library(pxweb)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(tibble)
library(brnn)
library(scales)

set.seed(42)

# --- GLOBAL PROJECTION PARAMETERS ---
TRAIN_END  <- 2025   # Last full year of historical flow data
PROJ_START <- 2026   # First year of projection
PROJ_END   <- 2076   # 50-year horizon
PROJ_YEARS <- PROJ_START:PROJ_END
Y_LEN      <- length(PROJ_YEARS)

S_sims <- 1000  ## projection simulations
B <- 50        ## bootstrapping the brnn (data and models for uncertainty) 

###############################################################################
### PART 1: DATA INGESTION & CLEANING                                       ###
###############################################################################

# ---------------------------------------------------------
# 1A. Births
# ---------------------------------------------------------
cat("1/4: Fetching Births Data...\n")
url_births <- "https://px.hagstofa.is:443/pxen/api/v1/en/Ibuar/Faeddirdanir/faeddir/faedingar/MAN05101.px"
df_births <- pxweb_get(url = url_births, query = list("Eining"="0", "Aldur"="*", "Ár"="*"), verbose = FALSE) %>%
  as.data.frame(column.name.type = "text", variable.value.type = "text") %>%
  rename(Births = 4) %>%
  filter(Age != "Total") %>%
  mutate(Year = as.numeric(as.character(Year)), Age = parse_number(sub('On', '0', Age))) %>%
  select(Year, Age, Births)

# ---------------------------------------------------------
# 1B. Deaths 
# ---------------------------------------------------------
cat("2/4: Fetching Deaths Data...\n")
url_deaths <- "https://px.hagstofa.is:443/pxen/api/v1/en/Ibuar/Faeddirdanir/danir/danir/MAN05221.px"
df_deaths <- pxweb_get(url = url_deaths, query = list("Aldur"="*", "Ár"="*", "Kyn"="*", "Sveitarfélag"="00"), verbose = FALSE) %>%
  as.data.frame(column.name.type = "text", variable.value.type = "text") %>%
  rename(Deaths = 5) %>%
  filter(Age != "Total", Sex != "Total", Municipality == "Total") %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Is_Male = ifelse(Sex == "Males", 1, 0),
    Age = parse_number(sub('On', '0', Age))
  ) %>%
  select(Year, Age, Gender = Sex, Is_Male, Deaths)

# ---------------------------------------------------------
# 1C. Migration 
# ---------------------------------------------------------
cat("3/4: Fetching Migration Data...\n")
url_migration <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/buferlaflutningar/buferlaflmillilanda/MAN01401.px"

fetch_migration <- function(citizenship, flow_code, flow_label) {
  query <- list("Kyn"=c("1","2"), "Ríkisfang"=c(citizenship), "Tegund flutnings"=c(flow_code), "Aldur"=c("*"), "Ár"=c("*"))
  pxweb_get(url = url_migration, query = query, verbose = FALSE) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>%
    rename(Count = 6) %>%
    mutate(
      Year = as.numeric(as.character(Ár)),
      Age = ifelse(grepl("Á 1. ári", Aldur, ignore.case = TRUE), 0, as.numeric(str_extract(Aldur, "\\d+"))),
      Count = as.numeric(Count),
      Is_Male = ifelse(Kyn == "Karlar", 1, 0),
      Flow = flow_label,
      Is_Immigrant = ifelse(flow_label == "Immigrant", 1, 0)
    ) %>%
    filter(!is.na(Age)) %>% select(Year, Age, Is_Male, Is_Immigrant, Count)
}

df_mig_tot <- bind_rows(
  fetch_migration("2", "1", "Immigrant"), fetch_migration("2", "2", "Emigrant"),
  fetch_migration("1", "1", "Immigrant"), fetch_migration("1", "2", "Emigrant")
) %>%
  group_by(Year, Age, Is_Male, Is_Immigrant) %>%
  summarize(Count = sum(Count, na.rm = TRUE), .groups = "drop")


# ---------------------------------------------------------
# 1D. Baseline Population & Historical Series
# ---------------------------------------------------------
cat("4/4: Fetching Population Data...\n")
url_pop <- "https://px.hagstofa.is:443/pxen/api/v1/en/Ibuar/mannfjoldi/1_yfirlit/yfirlit_mannfjolda/MAN00101.px"

# Step 1: Save the full dataframe for ALL years
df_pop_clean <- pxweb_get(url = url_pop, query = list("Kyn"=c("1","2"), "Aldur"="*", "Ár"="*"), verbose = FALSE) %>%
  as.data.frame(column.name.type = "text", variable.value.type = "text") %>%
  filter(Age != "Total") %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Age_Num = ifelse(grepl("Under", Age), 0, as.numeric(str_extract(Age, "\\d+"))),
    Gender_Str = ifelse(Sex == "Males", "Male", "Female"),
    Count = as.numeric(`Population 1841-2026`)
  ) %>%
  select(Year, Age = Age_Num, Gender = Gender_Str, Count) %>%
  filter(Year >= 1990)

# Step 2: Extract baseline population for PROJ_START (Jan 1st) to properly align stocks and flows
base_yr <- ifelse(PROJ_START %in% df_pop_clean$Year, PROJ_START, TRAIN_END)
Baseline_Pop_Recreated <- df_pop_clean %>%
  filter(Year == base_yr, Age <= 100) %>% 
  complete(Age = 0:100, Gender = c("Male", "Female"), fill = list(Count = 0)) %>%
  pivot_wider(names_from = Gender, values_from = Count) %>%
  select(Age, Male, Female) %>% 
  arrange(Age) %>% 
  column_to_rownames(var = "Age") %>% 
  as.matrix()


###############################################################################
### PART 2: MODELING & BOOTSTRAPPING (brnn)                                 ###
###############################################################################

# ---------------------------------------------------------
# 2A. Births Modeling (LINEAR ASFR)
# ---------------------------------------------------------
cat("\nTraining Births Model (Linear ASFR)...\n")

hist_fem_pop <- df_pop_clean %>% filter(Gender == "Female") %>% select(Year, Age, Fem_Count = Count)

df_train_b <- expand.grid(Year = 1990:TRAIN_END, Age = 15:45) %>%
  left_join(df_births, by = c("Year", "Age")) %>%
  left_join(hist_fem_pop, by = c("Year", "Age")) %>%
  mutate(
    Births = replace_na(Births, 0),
    Fem_Count = pmax(replace_na(Fem_Count, 1), 1),
    ASFR = Births / Fem_Count,
    Target = ASFR * 1000  # Scale target natively for BRNN convergence
  )

df_test_b <- expand.grid(Year = PROJ_YEARS, Age = 15:45)
df_anchor_b <- df_test_b
df_anchor_b$Year <- PROJ_START
df_test_b$Weight <- 1 - exp(-(df_test_b$Year - PROJ_START) / 15)

base_model_b <- brnn(Target ~ Year + Age, data = df_train_b, neurons = 3, verbose = FALSE)    

df_train_b$Resid <- df_train_b$Target - predict(base_model_b, newdata = df_train_b)
sd_age_b <- df_train_b %>% group_by(Age) %>% summarize(sd_lin = sd(Resid), .groups = "drop") %>% mutate(sd_lin = pmax(replace_na(sd_lin, 0.01), 0.01))
df_train_b <- df_train_b %>% left_join(sd_age_b, by = "Age")
df_test_b  <- df_test_b %>% left_join(sd_age_b, by = "Age")

Boot_ASFR_Raw <- matrix(NA, nrow = nrow(df_test_b), ncol = B)

# 10-year "Recent Regime" grid to build a stable anchor
df_anchor_grid_b <- expand.grid(Year = (TRAIN_END - 9):TRAIN_END, Age = 15:45)

for(i in 1:B) {
  boot_train <- df_train_b
  boot_train$Target <- predict(base_model_b, newdata = df_train_b) + rnorm(nrow(df_train_b), 0, df_train_b$sd_lin)
  
  boot_model <- brnn(Target ~ Year + Age, data = boot_train, neurons = 3, verbose = FALSE)    
  
  raw_pred <- predict(boot_model, newdata = df_test_b)
  
  # Calculate 10-year regime anchor dynamically for this bootstrap
  df_anchor_grid_b$Pred <- predict(boot_model, newdata = df_anchor_grid_b)
  anchor_df <- df_anchor_grid_b %>% group_by(Age) %>% summarize(Anchor = mean(Pred), .groups = "drop")
  anchor_pred <- left_join(df_test_b, anchor_df, by = "Age") %>% pull(Anchor)
  
  damped_pred <- (1 - df_test_b$Weight) * raw_pred + df_test_b$Weight * anchor_pred
  
  noisy_pred <- damped_pred + rnorm(nrow(df_test_b), 0, df_test_b$sd_lin)
  Boot_ASFR_Raw[, i] <- pmax(noisy_pred / 1000, 0)
}

Boot_ASFR_Rates <- array(0, dim = c(Y_LEN, length(15:45), B), dimnames = list(PROJ_YEARS, 15:45, 1:B))

for(b in 1:B) {
  temp_df <- df_test_b %>%
    mutate(Pred = Boot_ASFR_Raw[, b]) %>%
    select(Year, Age, Pred) %>% 
    pivot_wider(names_from = Age, values_from = Pred) %>%
    select(-Year) %>%
    as.matrix()
  
  Boot_ASFR_Rates[, , b] <- temp_df
}


# ---------------------------------------------------------
# 2B. Deaths Modeling (LOG-SPACE ASMR - Gompertz Law)
# ---------------------------------------------------------
cat("Training Deaths Model (Log-Space ASMR)...\n")

hist_pop_d <- df_pop_clean %>%
  mutate(Is_Male = ifelse(Gender == "Male", 1, 0)) %>%
  select(Year, Age, Is_Male, Pop_Count = Count)

df_train_d <- expand.grid(Year = 1990:TRAIN_END, Age = 0:100, Is_Male = c(1, 0)) %>%
  left_join(df_deaths, by = c("Year", "Age", "Is_Male")) %>%
  left_join(hist_pop_d, by = c("Year", "Age", "Is_Male")) %>%
  mutate(
    Deaths = replace_na(Deaths, 0),
    Pop_Count = pmax(replace_na(Pop_Count, 1), 1),
    Mort_Rate = Deaths / Pop_Count,
    Target_Log = log(Mort_Rate * 100000 + 1)
  )

df_test_d <- expand.grid(Year = PROJ_YEARS, Age = 0:100, Is_Male = c(1, 0))

base_model_d <- brnn(Target_Log ~ Year + Age + Is_Male, data = df_train_d, neurons = 3, verbose = FALSE)

df_train_d$Resid_Log <- df_train_d$Target_Log - predict(base_model_d, newdata = df_train_d)
df_train_d$Linear_Pred <- (exp(predict(base_model_d, newdata = df_train_d)) - 1) / 100000
df_train_d$Resid_Lin <- df_train_d$Mort_Rate - df_train_d$Linear_Pred

sd_age_d <- df_train_d %>% group_by(Age, Is_Male) %>% 
  summarize(sd_log = sd(Resid_Log), sd_lin = sd(Resid_Lin), .groups = "drop") %>% 
  mutate(sd_log = pmax(replace_na(sd_log, 0.01), 0.01), sd_lin = pmax(replace_na(sd_lin, 0.000001), 0.000001))

df_train_d <- df_train_d %>% left_join(sd_age_d, by = c("Age", "Is_Male"))
df_test_d <- df_test_d %>% left_join(sd_age_d %>% select(Age, Is_Male, sd_lin), by = c("Age", "Is_Male"))

Boot_ASMR_Raw <- matrix(NA, nrow = nrow(df_test_d), ncol = B)

for(i in 1:B) {
  boot_train <- df_train_d
  boot_train$Target_Log <- predict(base_model_d, newdata = df_train_d) + rnorm(nrow(df_train_d), 0, df_train_d$sd_log)
  
  boot_model <- brnn(Target_Log ~ Year + Age + Is_Male, data = boot_train, neurons = 3, verbose = FALSE)
  
  raw_pred <- predict(boot_model, newdata = df_test_d)
  
  # NO DAMPING FOR MORTALITY: Allow continuous historical improvement (Lee-Carter style drift)
  clean_rate <- (exp(raw_pred) - 1) / 100000
  Boot_ASMR_Raw[, i] <- pmax(clean_rate + rnorm(nrow(df_test_d), 0, df_test_d$sd_lin), 0)
}

Boot_Mort_Rates <- array(Boot_ASMR_Raw, dim = c(Y_LEN, 101, 2, B))


# ---------------------------------------------------------
# 2C. Migration Modeling (LINEAR Rates)
# ---------------------------------------------------------
cat("Training Migration Model (Linear Rates)...\n")

hist_pop_m <- hist_pop_d

df_train_m <- expand.grid(Year = 2000:TRAIN_END, Age = 0:100, Is_Male = c(1, 0), Is_Immigrant = c(1, 0)) %>%
  left_join(df_mig_tot, by = c("Year", "Age", "Is_Male", "Is_Immigrant")) %>%
  left_join(hist_pop_m, by = c("Year", "Age", "Is_Male")) %>%
  mutate(
    Count = replace_na(Count, 0),
    Pop_Count = pmax(replace_na(Pop_Count, 1), 1),
    Mig_Rate = Count / Pop_Count,
    Target = Mig_Rate * 1000 # Scale target natively
  )

df_test_m <- expand.grid(Year = PROJ_YEARS, Age = 0:100, Is_Male = c(1, 0), Is_Immigrant = c(1, 0))
df_test_m$Weight <- 1 - exp(-(df_test_m$Year - PROJ_START) / 15)

base_model_m <- brnn(Target ~ Year + Age + Is_Male + Is_Immigrant, data = df_train_m, neurons = 3, verbose = FALSE)

df_train_m$Resid <- df_train_m$Target - predict(base_model_m, newdata = df_train_m)
sd_age_m <- df_train_m %>% group_by(Age, Is_Male, Is_Immigrant) %>% summarize(sd_lin = sd(Resid), .groups = "drop") %>% mutate(sd_lin = pmax(replace_na(sd_lin, 0.01), 0.01))
df_train_m <- df_train_m %>% left_join(sd_age_m, by = c("Age", "Is_Male", "Is_Immigrant"))
df_test_m <- df_test_m %>% left_join(sd_age_m, by = c("Age", "Is_Male", "Is_Immigrant"))

Boot_Mig_Raw <- matrix(NA, nrow = nrow(df_test_m), ncol = B)

# 10-year "Recent Regime" grid to build a stable anchor
df_anchor_grid_m <- expand.grid(Year = (TRAIN_END - 9):TRAIN_END, Age = 0:100, Is_Male = c(1, 0), Is_Immigrant = c(1, 0))

for(i in 1:B) {
  boot_train <- df_train_m
  boot_train$Target <- predict(base_model_m, newdata = df_train_m) + rnorm(nrow(df_train_m), 0, df_train_m$sd_lin)
  
  boot_model <- brnn(Target ~ Year + Age + Is_Male + Is_Immigrant, data = boot_train, neurons = 3, verbose = FALSE)
  
  raw_pred <- predict(boot_model, newdata = df_test_m)
  
  # Calculate 10-year regime anchor dynamically for this bootstrap
  df_anchor_grid_m$Pred <- predict(boot_model, newdata = df_anchor_grid_m)
  anchor_df <- df_anchor_grid_m %>% group_by(Age, Is_Male, Is_Immigrant) %>% summarize(Anchor = mean(Pred), .groups = "drop")
  anchor_pred <- left_join(df_test_m, anchor_df, by = c("Age", "Is_Male", "Is_Immigrant")) %>% pull(Anchor)
  
  damped_pred <- (1 - df_test_m$Weight) * raw_pred + df_test_m$Weight * anchor_pred
  
  # NEW: Safely add linear Aleatoric noise
  noisy_pred <- damped_pred + rnorm(nrow(df_test_m), 0, df_test_m$sd_lin)
  Boot_Mig_Raw[, i] <- pmax(noisy_pred / 1000, 0)
}

idx_imm <- which(df_test_m$Is_Immigrant == 1)
idx_emi <- which(df_test_m$Is_Immigrant == 0)

Boot_Imm_Rates <- array(Boot_Mig_Raw[idx_imm, ], dim = c(Y_LEN, 101, 2, B))
Boot_Emi_Rates <- array(Boot_Mig_Raw[idx_emi, ], dim = c(Y_LEN, 101, 2, B))


#####################################################################################
##### PART 3: THE SCENARIO ENGINE ###################################################
#####################################################################################

# ---------------------------------------------------------
# Mathematical Helper: Period Life Expectancy (e0)
# ---------------------------------------------------------
calculate_e0 <- function(mx) {
  ax <- rep(0.5, length(mx)); ax[1] <- 0.1 
  qx <- mx / (1 + (1 - ax) * mx)
  qx <- pmin(qx, 1); qx[length(qx)] <- 1 
  px <- 1 - qx
  lx <- c(100000, 100000 * cumprod(px)[-length(px)])
  dx <- lx * qx
  Lx <- lx - (1 - ax) * dx
  m_term <- max(mx[length(mx)], 0.1) 
  Lx[length(Lx)] <- lx[length(lx)] / m_term
  Tx <- rev(cumsum(rev(Lx)))
  return(Tx[1] / lx[1])
}

# ---------------------------------------------------------
# The Core Projection Wrapper
# ---------------------------------------------------------
run_scenario <- function(scenario_name, arr_births_asfr, arr_deaths_rates, arr_imm_rates, arr_emi_rates, n_sims = S_sims) {
  
  cat(sprintf("\nRunning Scenario: %s (%d Sims)...\n", scenario_name, n_sims))
  
  Pop_Proj <- array(0, dim = c(Y_LEN, 101, 2, n_sims), dimnames = list(PROJ_YEARS, 0:100, c("Male", "Female"), 1:n_sims))
  e0_Proj  <- array(0, dim = c(Y_LEN, 2, n_sims), dimnames = list(PROJ_YEARS, c("Male", "Female"), 1:n_sims))
  TFR_Proj <- matrix(0, nrow = Y_LEN, ncol = n_sims, dimnames = list(PROJ_YEARS, 1:n_sims))
  Births_Proj <- matrix(0, nrow = Y_LEN, ncol = n_sims, dimnames = list(PROJ_YEARS, 1:n_sims))
  Deaths_Proj <- matrix(0, nrow = Y_LEN, ncol = n_sims, dimnames = list(PROJ_YEARS, 1:n_sims))
  Imm_Proj <- matrix(0, nrow = Y_LEN, ncol = n_sims, dimnames = list(PROJ_YEARS, 1:n_sims)) 
  Emi_Proj <- matrix(0, nrow = Y_LEN, ncol = n_sims, dimnames = list(PROJ_YEARS, 1:n_sims)) 
  
  for(sim in 1:n_sims) {
    idx_b <- sample(1:B, 1); idx_d <- sample(1:B, 1); idx_i <- sample(1:B, 1); idx_e <- sample(1:B, 1)  
    Current_Pop <- Baseline_Pop_Recreated 
    
    for(y_idx in 1:Y_LEN) {
      
      # Record Jan 1st Population BEFORE flow events
      Pop_Proj[y_idx, , , sim] <- Current_Pop
      
      Next_Pop <- matrix(0, nrow = 101, ncol = 2) 
      total_deaths_this_year <- 0
      total_imm_this_year <- 0
      total_emi_this_year <- 0
      
      e0_Proj[y_idx, 1, sim] <- calculate_e0(arr_deaths_rates[y_idx, , 1, idx_d])
      e0_Proj[y_idx, 2, sim] <- calculate_e0(arr_deaths_rates[y_idx, , 2, idx_d])
      
      fem_pop_repro <- Current_Pop[16:46, 2] 
      asfr <- arr_births_asfr[y_idx, , idx_b]
      
      TFR_Proj[y_idx, sim] <- sum(asfr)
      sim_births_repro <- asfr * fem_pop_repro 
      
      for(age in 0:100) {
        for(gender in 1:2) {
          # DYNAMIC MIGRATION: Rate * Population
          imm_sim <- arr_imm_rates[y_idx, age + 1, gender, idx_i] * Current_Pop[age + 1, gender]
          emi_sim <- arr_emi_rates[y_idx, age + 1, gender, idx_e] * Current_Pop[age + 1, gender]
          
          if (age == 100) {
            deaths_sim <- Current_Pop[age + 1, gender] * 0.45
            survivors <- Current_Pop[age + 1, gender] - deaths_sim
          } else {
            deaths_sim <- arr_deaths_rates[y_idx, age + 1, gender, idx_d] * Current_Pop[age + 1, gender]
            survivors <- Current_Pop[age + 1, gender] - deaths_sim + imm_sim - emi_sim
          }
          
          total_deaths_this_year <- total_deaths_this_year + deaths_sim
          total_imm_this_year <- total_imm_this_year + imm_sim
          total_emi_this_year <- total_emi_this_year + emi_sim
          
          target_age <- min(age + 1, 100) 
          Next_Pop[target_age + 1, gender] <- Next_Pop[target_age + 1, gender] + max(survivors, 0)
        }
      }
      
      total_births <- sum(sim_births_repro) 
      Births_Proj[y_idx, sim] <- total_births
      Deaths_Proj[y_idx, sim] <- total_deaths_this_year
      Imm_Proj[y_idx, sim] <- total_imm_this_year
      Emi_Proj[y_idx, sim] <- total_emi_this_year
      
      Next_Pop[1, 1] <- total_births * 0.5122  
      Next_Pop[1, 2] <- total_births * 0.4878  
      
      # Population for Jan 1st of the NEXT year becomes Current_Pop for the next iteration
      Current_Pop <- Next_Pop
    }
  }
  
  return(list(Name = scenario_name, Pop = Pop_Proj, e0 = e0_Proj, TFR = TFR_Proj, Births = Births_Proj, Deaths = Deaths_Proj, Imm = Imm_Proj, Emi = Emi_Proj))
}

# ---------------------------------------------------------
# Define Scenario Configurations
# ---------------------------------------------------------
scenario_defs <- list(
  "Baseline" = list(imm = 1.0, emi = 1.0, fert = 1.0),
  "High Growth" = list(imm = 1.30, emi = 0.90, fert = 1.05),
  "Dem Winter"  = list(imm = 0.80, emi = 1.20, fert = 0.85)
)

# ---------------------------------------------------------
# Execute Projection Scenarios
# ---------------------------------------------------------
cat("Running Scenario Engine natively with smoothly damped NN Arrays...\n")

scenario_results <- lapply(names(scenario_defs), function(s_name) {
  params <- scenario_defs[[s_name]]
  
  scen_births_asfr <- Boot_ASFR_Rates * params$fert
  scen_imm_rates   <- Boot_Imm_Rates * params$imm
  scen_emi_rates   <- Boot_Emi_Rates * params$emi
  
  run_scenario(s_name, scen_births_asfr, Boot_Mort_Rates, scen_imm_rates, scen_emi_rates, n_sims = S_sims)
})
names(scenario_results) <- names(scenario_defs)


###############################################################################
### PART 4: VISUALIZATIONS                                                  ###
###############################################################################
cat("Generating Visualizations...\n")

build_fan_df <- function(sim_matrix, baseline_val) {
  df <- data.frame(
    Year = PROJ_YEARS, Median = apply(sim_matrix, 1, median),
    Q02.5 = apply(sim_matrix, 1, quantile, probs=0.025), Q97.5 = apply(sim_matrix, 1, quantile, probs=0.975),
    Q10 = apply(sim_matrix, 1, quantile, probs=0.10), Q90 = apply(sim_matrix, 1, quantile, probs=0.90),
    Q25 = apply(sim_matrix, 1, quantile, probs=0.25), Q75 = apply(sim_matrix, 1, quantile, probs=0.75)
  )
  base_df <- data.frame(Year=TRAIN_END, Median=baseline_val, Q02.5=baseline_val, Q97.5=baseline_val, Q10=baseline_val, Q90=baseline_val, Q25=baseline_val, Q75=baseline_val)
  bind_rows(base_df, df)
}

plot_fan <- function(df, title, y_label, color_hex) {
  ggplot(df, aes(x = Year)) +
    geom_ribbon(aes(ymin = Q02.5, ymax = Q97.5), fill = color_hex, alpha = 0.2) +
    geom_ribbon(aes(ymin = Q10, ymax = Q90), fill = color_hex, alpha = 0.4) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75), fill = color_hex, alpha = 0.6) +
    geom_line(aes(y = Median), color = color_hex, linewidth = 1.2) +
    geom_point(aes(y = Median), color = color_hex, size = 2) +
    scale_y_continuous(labels = comma) +
    ggtitle(title) + ylab(y_label) + xlab("Year") + theme_minimal() + theme(plot.title = element_text(face = "bold"))
}

# --- EXTRACT GLOBALS FOR LEGACY PLOTS ---
Pop_Proj    <- scenario_results[["Baseline"]]$Pop
e0_Proj     <- scenario_results[["Baseline"]]$e0
Births_Proj <- scenario_results[["Baseline"]]$Births
Deaths_Proj <- scenario_results[["Baseline"]]$Deaths
Imm_Proj    <- scenario_results[["Baseline"]]$Imm
Emi_Proj    <- scenario_results[["Baseline"]]$Emi

# Calculate exact historical anchors to prevent data-leakage jumps
anchor_pop    <- df_pop_clean %>% filter(Year == TRAIN_END) %>% pull(Count) %>% sum()
anchor_births <- df_births %>% filter(Year == TRAIN_END) %>% pull(Births) %>% sum()
anchor_deaths <- df_deaths %>% filter(Year == TRAIN_END) %>% pull(Deaths) %>% sum()
anchor_imm    <- df_mig_tot %>% filter(Year == TRAIN_END, Is_Immigrant == 1, Age <= 100) %>% pull(Count) %>% sum()
anchor_emi    <- df_mig_tot %>% filter(Year == TRAIN_END, Is_Immigrant == 0, Age <= 100) %>% pull(Count) %>% sum()

# 1. Pop (Baseline)
print(plot_fan(build_fan_df(apply(Pop_Proj, c(1, 4), sum), anchor_pop), "Total Projected Population (Baseline)", "Individuals", "#03396c"))

# 2. Births (Dynamic)
print(plot_fan(build_fan_df(Births_Proj, anchor_births), "Total Projected Births (Baseline Dynamic)", "Live Births", "#4a148c"))

# 3. Deaths (Dynamic)
print(plot_fan(build_fan_df(Deaths_Proj, anchor_deaths), "Total Projected Deaths (Baseline Dynamic)", "Deaths", "#b71c1c"))

# 4 & 5. Migration (Dynamic Tracking)
print(plot_fan(build_fan_df(Imm_Proj, anchor_imm), "Total Projected Immigration (Baseline Dynamic)", "Immigrants", "#004d40"))
print(plot_fan(build_fan_df(Emi_Proj, anchor_emi), "Total Projected Emigration (Baseline Dynamic)", "Emigrants", "#e65100"))

# 6. Net Migration (Baseline)
Net_Mig_Sims <- Imm_Proj - Emi_Proj
print(plot_fan(build_fan_df(Net_Mig_Sims, anchor_imm - anchor_emi), "Total Projected Net Migration", "Net Migrants", "#00838f"))


# ---------------------------------------------------------
#  8. Age-Specific Component Facets
# ---------------------------------------------------------
cat("Calculating age-specific facets for Births, Deaths, and Migration...\n")

b_quantiles <- t(apply(Boot_ASFR_Raw, 1, quantile, probs = c(0.025, 0.10, 0.5, 0.90, 0.975)))
df_test_b_plot <- df_test_b %>% mutate(Q02.5 = b_quantiles[, 1], Q10 = b_quantiles[, 2], Median = b_quantiles[, 3], Q90 = b_quantiles[, 4], Q97.5 = b_quantiles[, 5])
df_train_b_plot <- df_train_b %>% mutate(Fitted = pmax(predict(base_model_b, newdata = df_train_b) / 1000, 0))
target_ages_b <- c(20, 25, 30, 35)

plot_births_facet <- ggplot() +
  geom_point(data = filter(df_train_b_plot, Age %in% target_ages_b), aes(x = Year, y = ASFR), color = "black", alpha = 0.5) +
  geom_line(data = filter(df_train_b_plot, Age %in% target_ages_b), aes(x = Year, y = Fitted), color = "#4a148c", linetype = "dashed") +
  geom_ribbon(data = filter(df_test_b_plot, Age %in% target_ages_b), aes(x = Year, ymin = Q10, ymax = Q90), fill = "#4a148c", alpha = 0.3) +
  geom_line(data = filter(df_test_b_plot, Age %in% target_ages_b), aes(x = Year, y = Median), color = "#4a148c", linewidth = 1) +
  facet_wrap(~ Age, labeller = label_both, scales = "free_y") +
  ggtitle("NN Forecast of ASFR by Mother's Age") + ylab("Fertility Rate (ASFR)") + xlab("Year") + theme_bw()

print(plot_births_facet)


d_quantiles <- t(apply(Boot_ASMR_Raw, 1, quantile, probs = c(0.025, 0.10, 0.5, 0.90, 0.975)))
df_test_d_plot <- df_test_d %>% mutate(Gender = ifelse(Is_Male == 1, "Male", "Female"), Q02.5 = d_quantiles[, 1], Q10 = d_quantiles[, 2], Median = d_quantiles[, 3], Q90 = d_quantiles[, 4], Q97.5 = d_quantiles[, 5])

# Apply Smearing Estimator (+ sd_log^2 / 2) to correct for Jensen's Inequality in the historical plot
df_train_d_plot <- df_train_d %>% mutate(Gender = ifelse(Is_Male == 1, "Male", "Female"), Fitted = pmax((exp(predict(base_model_d, newdata = df_train_d) + (sd_log^2)/2) - 1) / 100000, 0))
target_ages_d <- c(0, 40, 80)

plot_deaths_facet <- ggplot() +
  geom_point(data = filter(df_train_d_plot, Age %in% target_ages_d), aes(x = Year, y = Mort_Rate), color = "black", alpha = 0.4) +
  geom_line(data = filter(df_train_d_plot, Age %in% target_ages_d), aes(x = Year, y = Fitted), color = "#b71c1c", linetype = "dashed") +
  geom_ribbon(data = filter(df_test_d_plot, Age %in% target_ages_d), aes(x = Year, ymin = Q10, ymax = Q90), fill = "#b71c1c", alpha = 0.3) +
  geom_line(data = filter(df_test_d_plot, Age %in% target_ages_d), aes(x = Year, y = Median), color = "#b71c1c", linewidth = 1) +
  facet_grid(Gender ~ Age, scales = "free_y", labeller = label_both) +
  ggtitle("NN Forecast of ASMR by Age and Gender") + ylab("Mortality Rate (ASMR)") + xlab("Year") + theme_bw() +
  theme(strip.background = element_rect(fill = "lightgray"))

print(plot_deaths_facet)


m_quantiles <- t(apply(Boot_Mig_Raw, 1, quantile, probs = c(0.025, 0.10, 0.5, 0.90, 0.975)))
df_test_m_plot <- df_test_m %>% mutate(Gender = ifelse(Is_Male == 1, "Male", "Female"), Flow = ifelse(Is_Immigrant == 1, "Immigrant", "Emigrant"), Q02.5 = m_quantiles[, 1], Q10 = m_quantiles[, 2], Median = m_quantiles[, 3], Q90 = m_quantiles[, 4], Q97.5 = m_quantiles[, 5])
df_train_m_plot <- df_train_m %>% mutate(Gender = ifelse(Is_Male == 1, "Male", "Female"), Flow = ifelse(Is_Immigrant == 1, "Immigrant", "Emigrant"), Fitted = pmax(predict(base_model_m, newdata = df_train_m) / 1000, 0))
target_ages_m <- c(25, 30, 35, 40)

plot_mig_facet <- ggplot() +
  geom_point(data = filter(df_train_m_plot, Age %in% target_ages_m), aes(x = Year, y = Mig_Rate, color = Flow), alpha = 0.3, size = 1) +
  geom_line(data = filter(df_train_m_plot, Age %in% target_ages_m), aes(x = Year, y = Fitted, color = Flow), linetype = "dashed") +
  geom_ribbon(data = filter(df_test_m_plot, Age %in% target_ages_m), aes(x = Year, ymin = Q10, ymax = Q90, fill = Flow), alpha = 0.3) +
  geom_line(data = filter(df_test_m_plot, Age %in% target_ages_m), aes(x = Year, y = Median, color = Flow), linewidth = 1) +
  facet_grid(Gender ~ Age, scales = "free_y", labeller = label_both) +
  scale_color_manual(values = c("Immigrant" = "#004d40", "Emigrant" = "#e65100")) + scale_fill_manual(values = c("Immigrant" = "#004d40", "Emigrant" = "#e65100")) +
  ggtitle("NN Forecast of Migration Rates by Age and Gender") + ylab("Migration Rate") + xlab("Year") + theme_bw() + theme(strip.background = element_rect(fill = "lightgray"), legend.position = "bottom")

print(plot_mig_facet)


# --- 7. Pyramid plots -------------------------------------------------------------------------
cat("Calculating age-specific uncertainty bands for 2074 Pyramid...\n")
pop_fin_sims <- Pop_Proj[Y_LEN, , , ] 
male_sims <- pop_fin_sims[, 1, ]
female_sims <- pop_fin_sims[, 2, ]
df_pyramid <- data.frame(Age = rep(0:100, times = 2), Gender = rep(c("Male", "Female"), each = 101))
df_pyramid$Median <- c(apply(male_sims, 1, median), apply(female_sims, 1, median))
df_pyramid$Lower  <- c(apply(male_sims, 1, quantile, probs = 0.025), apply(female_sims, 1, quantile, probs = 0.025))
df_pyramid$Upper  <- c(apply(male_sims, 1, quantile, probs = 0.975), apply(female_sims, 1, quantile, probs = 0.975))

df_plot_pyr <- df_pyramid %>% mutate(Plot_Median = ifelse(Gender == "Male", -Median, Median), Plot_Lower = ifelse(Gender == "Male", -Upper, Lower), Plot_Upper = ifelse(Gender == "Male", -Lower, Upper))

plot_pyramid <- ggplot(df_plot_pyr, aes(x = Age, fill = Gender, color = Gender)) +
  geom_col(aes(y = Plot_Median), width = 0.8, alpha = 0.6, color = NA) +
  geom_linerange(aes(ymin = Plot_Lower, ymax = Plot_Upper), linewidth = 0.8) +
  coord_flip() + scale_y_continuous(labels = function(x) scales::comma(abs(x))) + scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_fill_manual(values = c("Male" = "#3b5998", "Female" = "#8b9dc3")) + scale_color_manual(values = c("Male" = "#1c2b4a", "Female" = "#3b4a6b")) + 
  ggtitle(sprintf("Stochastic Population Pyramid at Year %d (Baseline)", PROJ_END)) + ylab("Population") + xlab("Age") + theme_minimal() + theme(legend.position = "bottom")

print(plot_pyramid)


### ---- 8. Life Expectancy (e0) Fans -----------------------------------------------------------
cat("Calculating Life Expectancy Fan Charts...\n")
deaths_base_m <- df_train_d %>% filter(Year == TRAIN_END, Is_Male == 1) %>% arrange(Age) %>% pull(Deaths)
deaths_base_f <- df_train_d %>% filter(Year == TRAIN_END, Is_Male == 0) %>% arrange(Age) %>% pull(Deaths)
hist_pop_base_m <- df_train_d %>% filter(Year == TRAIN_END, Is_Male == 1) %>% arrange(Age) %>% pull(Pop_Count)
hist_pop_base_f <- df_train_d %>% filter(Year == TRAIN_END, Is_Male == 0) %>% arrange(Age) %>% pull(Pop_Count)

# Feed rates to calculate_e0 instead of deaths/pop
base_e0_m <- calculate_e0(deaths_base_m / hist_pop_base_m)
base_e0_f <- calculate_e0(deaths_base_f / hist_pop_base_f)

e0_Male_Fan   <- build_fan_df(e0_Proj[, 1, ], base_e0_m) %>% mutate(Gender = "Male")
e0_Female_Fan <- build_fan_df(e0_Proj[, 2, ], base_e0_f) %>% mutate(Gender = "Female")
e0_Combined <- bind_rows(e0_Male_Fan, e0_Female_Fan)

plot_e0_combined <- ggplot(e0_Combined, aes(x = Year, fill = Gender, color = Gender)) +
  geom_ribbon(aes(ymin = Q02.5, ymax = Q97.5), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.4, color = NA) +
  geom_line(aes(y = Median), linewidth = 1.2) + geom_point(aes(y = Median), size = 2) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Male" = "#1565c0", "Female" = "#c62828")) + scale_color_manual(values = c("Male" = "#0d47a1", "Female" = "#b71c1c")) +
  ggtitle("Stochastic Projection: Life Expectancy at Birth (e0)", subtitle = "Overlapping probability bands for Males and Females") + ylab("Life Expectancy (Years)") + xlab("Year") + theme_minimal() + theme(legend.position = "bottom")

print(plot_e0_combined)

# --- Scenario Comparison: Total Population --------------------------------
cat("Generating Scenario Comparison Plot...\n")
compare_dfs <- lapply(scenario_results, function(scen) {
  tot_pop_matrix <- apply(scen$Pop, c(1, 4), sum)
  df <- build_fan_df(tot_pop_matrix, anchor_pop)
  df$Scenario <- scen$Name
  return(df)
})
df_scenario_compare <- bind_rows(compare_dfs)

plot_scen_comp <- ggplot(df_scenario_compare, aes(x = Year, fill = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.4, color = NA) +
  geom_line(aes(y = Median), linewidth = 1.2) + geom_point(aes(y = Median), size = 2) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("Baseline" = "#03396c", "High Growth" = "#004d40", "Dem Winter" = "#b71c1c")) +
  scale_color_manual(values = c("Baseline" = "#03396c", "High Growth" = "#004d40", "Dem Winter" = "#b71c1c")) +
  ggtitle("Stochastic Scenario Comparison: Total Population", subtitle = "80% and 50% prediction intervals shown") + ylab("Total Individuals") + xlab("Year") + theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(plot_scen_comp)

# --- Scenario Comparison: Total Fertility Rate (TFR) --------------------------------------
cat("Generating Scenario Comparison Plot for TFR...\n")

anchor_tfr <- sum(df_train_b$ASFR[df_train_b$Year == TRAIN_END])

compare_dfs_tfr <- lapply(scenario_results, function(scen) {
  df <- build_fan_df(scen$TFR, anchor_tfr)
  df$Scenario <- scen$Name
  return(df)
})
df_tfr_compare <- bind_rows(compare_dfs_tfr)

plot_tfr_comp <- ggplot(df_tfr_compare, aes(x = Year, fill = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.4, color = NA) +
  geom_line(aes(y = Median), linewidth = 1.2) + geom_point(aes(y = Median), size = 2) +
  geom_hline(yintercept = 2.1, color = "black", linetype = "dashed", linewidth = 0.8, alpha = 0.6) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  scale_fill_manual(values = c("Baseline" = "#03396c", "High Growth" = "#004d40", "Dem Winter" = "#b71c1c")) + scale_color_manual(values = c("Baseline" = "#03396c", "High Growth" = "#004d40", "Dem Winter" = "#b71c1c")) +
  ggtitle("Stochastic Scenario Comparison: Total Fertility Rate (TFR)", subtitle = "Dashed black line indicates natural replacement level (2.1)") + ylab("Children per Woman") + xlab("Year") + theme_minimal() + theme(legend.position = "bottom")

print(plot_tfr_comp)

######################################################################################################
### PART 5: OUT-OF-SAMPLE Forecast EVALUATION                               ###
###################################################################################################
cat("\nEvaluating Forecast against Observed Short Series...\n")

actual_pop    <- df_pop_clean %>% group_by(Year) %>% summarize(Actual = sum(Count), .groups = "drop")
actual_births <- df_births %>% group_by(Year) %>% summarize(Actual = sum(Births), .groups = "drop")
actual_deaths <- df_deaths %>% group_by(Year) %>% summarize(Actual = sum(Deaths), .groups = "drop")
actual_imm    <- df_mig_tot %>% filter(Is_Immigrant==1) %>% group_by(Year) %>% summarize(Actual = sum(Count), .groups = "drop")
actual_emi    <- df_mig_tot %>% filter(Is_Immigrant==0) %>% group_by(Year) %>% summarize(Actual = sum(Count), .groups = "drop")

Total_Base <- apply(Pop_Proj, c(1, 4), sum)
Pop_Fan   <- build_fan_df(Total_Base, anchor_pop)

Birth_Fan <- build_fan_df(Births_Proj, anchor_births)
Death_Fan <- build_fan_df(Deaths_Proj, anchor_deaths)
Imm_Fan   <- build_fan_df(Imm_Proj, anchor_imm)
Emi_Fan   <- build_fan_df(Emi_Proj, anchor_emi)

evaluate_forecast <- function(fan_df, actual_df, component_name) {
  eval_df <- inner_join(fan_df, actual_df, by = "Year") %>% filter(Year > TRAIN_END) 
  if(nrow(eval_df) == 0) return(NULL)
  
  eval_df <- eval_df %>% mutate(
      Error = Median - Actual, Abs_Error = abs(Error), Pct_Error = Abs_Error / Actual,
      In_95_Band = ifelse(Actual >= Q02.5 & Actual <= Q97.5, 1, 0), In_50_Band = ifelse(Actual >= Q25 & Actual <= Q75, 1, 0))
  
  MAPE <- mean(eval_df$Pct_Error) * 100
  RMSE <- sqrt(mean(eval_df$Error^2))
  Cov_95 <- mean(eval_df$In_95_Band) * 100
  
  cat(sprintf("\n--- %s Evaluation (Years: %d to %d) ---\n", component_name, min(eval_df$Year), max(eval_df$Year)))
  cat(sprintf("MAPE: %.2f%%\n", MAPE))
  cat(sprintf("RMSE: %.0f\n", RMSE))
  cat(sprintf("95%% Interval Coverage: %.0f%%\n", Cov_95))
  
  p <- ggplot(eval_df, aes(x = Year)) +
    geom_ribbon(aes(ymin = Q02.5, ymax = Q97.5, fill = "95% Forecast Band"), alpha = 0.2) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = "50% Forecast Band"), alpha = 0.4) +
    geom_line(aes(y = Median, color = "Median Forecast"), linewidth = 1, linetype = "dashed") +
    geom_line(aes(y = Actual, color = "Actual Observed"), linewidth = 1.2) + geom_point(aes(y = Actual, color = "Actual Observed"), size = 3) +
    scale_fill_manual(name = "Uncertainty", values = c("95% Forecast Band" = "gray50", "50% Forecast Band" = "gray30")) +
    scale_color_manual(name = "Trajectory", values = c("Median Forecast" = "gray30", "Actual Observed" = "red")) +
    scale_x_continuous(breaks = scales::breaks_pretty()) + scale_y_continuous(labels = scales::comma) +
    ggtitle(sprintf("%s: Forecast vs Actual", component_name), subtitle = sprintf("Out-of-sample MAPE: %.2f%% | 95%% Coverage: %.0f%%", MAPE, Cov_95)) + ylab("Count") + xlab("Year") + theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  print(p)
  return(eval_df)
}

eval_pop    <- evaluate_forecast(Pop_Fan, actual_pop, "Total Population")
eval_births <- evaluate_forecast(Birth_Fan, actual_births, "Total Births")
eval_deaths <- evaluate_forecast(Death_Fan, actual_deaths, "Total Deaths")
eval_imm    <- evaluate_forecast(Imm_Fan, actual_imm, "Immigration")
eval_emi    <- evaluate_forecast(Emi_Fan, actual_emi, "Emigration")