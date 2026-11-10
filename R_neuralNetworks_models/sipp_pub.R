

### acknowledgement ------------------------------------------------------------
### this script has been developed with the assistance of Gemini (3.1.Pro, 2026)
### ----------------------------------------------------------------------------

###############################################################################
### SIPP: Stochastic Integrated Population Projections                        ###
### Using brnn, Bootstrapping, and Statistics Iceland Open Data (pxweb)     ###
###############################################################################

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
library(data.table)
library(DirichletReg)
library(pxmake)
library(openxlsx)

set.seed(42)

# --- GLOBAL PROJECTION PARAMETERS ---
TRAIN_END  <- 2025   # Last full year of historical flow data 
PROJ_START <- 2026   # First year of projection
PROJ_END   <- 2076   # 50-year horizon
PROJ_YEARS <- PROJ_START:PROJ_END
Y_LEN      <- length(PROJ_YEARS)

S_sims <- 1000       # projection simulations
B <-500             # Bootstrapping iterations for good uncertainty fanning: >= 500; 

Nneurons <- 5
Nneurons_mig <- 3  

damp_years_births <- 15
damp_years_mig <- 15

anchor_years_mig <- 9         
anchor_years_births <- 5   

## NOTE about why we need anchor and damping (not for mortality though, which is very stable and smooth):
## To ensure structural stability over the 50-year forecasting horizon, the models incorporate specific anchoring and damping mechanisms. 
## Anchoring gradually aligns the long-term stochastic projections with expert-derived equilibrium levels (such as a long-run total fertility rate trend), 
## preventing unrealistic drift in the distant future. 
## Simultaneously, damping techniques attenuate the influence of recent demographic shocks and autoregressive volatility, 
## ensuring that short-term fluctuations do not compound exponentially over the course of the projection period.

###  we build 60% and 80% intervals  in this version -----------------

# ---------------------------------------------------------
# Define Scenario Configurations  
# ---------------------------------------------------------
scenario_defs <- list(
  "Baseline"    = list(imm = 1.0,  emi = 1.0,  fert = 1.0),
  "High Growth" = list(imm = 1.05, emi = 0.95, fert = 1.1),
  "Dem Winter"  = list(imm = 0.95, emi = 1.05, fert = 0.9)
)


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

# Dynamic column renaming protects against Hagstofa dataset string updates
df_pop_clean <- pxweb_get(url = url_pop, query = list("Kyn"=c("1","2"), "Aldur"="*", "Ár"="*"), verbose = FALSE) %>%
  as.data.frame(column.name.type = "text", variable.value.type = "text") %>%
  rename(Pop_Count = 4) %>%  
  filter(Age != "Total") %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Age_Num = ifelse(grepl("Under", Age), 0, as.numeric(str_extract(Age, "\\d+"))),
    Gender_Str = ifelse(Sex == "Males", "Male", "Female"),
    Count = as.numeric(Pop_Count) 
  ) %>%
  select(Year, Age = Age_Num, Gender = Gender_Str, Count) %>%
  filter(Year >= 1990)

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
    Target = ASFR * 1000  
  )

df_test_b <- expand.grid(Year = PROJ_YEARS, Age = 15:45)
df_anchor_b <- df_test_b
df_anchor_b$Year <- PROJ_START
df_test_b$Weight <- 1 - exp(-(df_test_b$Year - PROJ_START) / damp_years_births)

base_model_b <- brnn(Target ~ Year + Age, data = df_train_b, neurons = Nneurons, verbose = FALSE)    

df_train_b$Resid <- df_train_b$Target - predict(base_model_b, newdata = df_train_b)
sd_age_b <- df_train_b %>% group_by(Age) %>% summarize(sd_lin = sd(Resid), .groups = "drop") %>% mutate(sd_lin = pmax(replace_na(sd_lin, 0.01), 0.01))
df_train_b <- df_train_b %>% left_join(sd_age_b, by = "Age")
df_test_b  <- df_test_b %>% left_join(sd_age_b, by = "Age")

Boot_ASFR_Raw <- matrix(NA, nrow = nrow(df_test_b), ncol = B)

# 10-year "Recent Regime" grid to build a stable anchor  
df_anchor_grid_b <- expand.grid(Year = (TRAIN_END - anchor_years_births):TRAIN_END, Age = 15:45)

for(i in 1:B) {
  boot_train <- df_train_b
  boot_train$Target <- predict(base_model_b, newdata = df_train_b) + rnorm(nrow(df_train_b), 0, df_train_b$sd_lin)
  
  boot_model <- brnn(Target ~ Year + Age, data = boot_train, neurons = Nneurons, verbose = FALSE)    
  raw_pred <- predict(boot_model, newdata = df_test_b)
  
  df_anchor_grid_b$Pred <- predict(boot_model, newdata = df_anchor_grid_b)
  anchor_df <- df_anchor_grid_b %>% group_by(Age) %>% summarize(Anchor = mean(Pred), .groups = "drop")
  anchor_pred <- left_join(df_test_b, anchor_df, by = "Age") %>% pull(Anchor)
  
  damped_pred <- (1 - df_test_b$Weight) * raw_pred + df_test_b$Weight * anchor_pred
  Boot_ASFR_Raw[, i] <- pmax(damped_pred / 1000, 0)
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
# 2B. Deaths Modeling (LOG-SPACE ASMR) -- BRNN with Bootstrapped Anchor
# ---------------------------------------------------------
cat("Training Deaths Model (BRNN with Bootstrapped Log-Linear Anchor)...\n")

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
# Dampening weight: Transitions smoothly over 25 years from BRNN to the stable linear anchor
df_test_d$Weight <- 1 - exp(-(df_test_d$Year - PROJ_START) / 25)

base_model_d <- brnn(Target_Log ~ Year + Age + Is_Male, data = df_train_d, neurons = Nneurons, verbose = FALSE)

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
  
  # 1. Inject stochastic noise into the historical training data
  boot_train$Target_Log <- predict(base_model_d, newdata = df_train_d) + rnorm(nrow(df_train_d), 0, df_train_d$sd_log)
  
  # 2. Fit the Neural Network on this specific stochastic universe
  boot_model <- brnn(Target_Log ~ Year + Age + Is_Male, data = boot_train, neurons = Nneurons, verbose = FALSE)
  
  # 3. Fit a Linear Anchor on THIS SAME stochastic universe to preserve natural variance
  boot_anchor_model <- lm(Target_Log ~ Year + Age + Is_Male, data = boot_train)
  
  # 4. Predict both trajectories
  raw_pred_log <- predict(boot_model, newdata = df_test_d)
  anchor_pred_log <- predict(boot_anchor_model, newdata = df_test_d)
  
  # 5. Blend them together over time
  damped_pred_log <- (1 - df_test_d$Weight) * raw_pred_log + df_test_d$Weight * anchor_pred_log
  
  clean_rate <- (exp(damped_pred_log) - 1) / 100000
  Boot_ASMR_Raw[, i] <- pmax(clean_rate, 0.0000001)
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
    Target = Mig_Rate * 1000 
  )

df_test_m <- expand.grid(Year = PROJ_YEARS, Age = 0:100, Is_Male = c(1, 0), Is_Immigrant = c(1, 0))
df_test_m$Weight <- 1 - exp(-(df_test_m$Year - PROJ_START) / damp_years_mig)

base_model_m <- brnn(Target ~ Year + Age + Is_Male + Is_Immigrant, data = df_train_m, neurons = Nneurons_mig, verbose = FALSE)

df_train_m$Resid <- df_train_m$Target - predict(base_model_m, newdata = df_train_m)
sd_age_m <- df_train_m %>% group_by(Age, Is_Male, Is_Immigrant) %>% summarize(sd_lin = sd(Resid), .groups = "drop") %>% mutate(sd_lin = pmax(replace_na(sd_lin, 0.01), 0.01))
df_train_m <- df_train_m %>% left_join(sd_age_m, by = c("Age", "Is_Male", "Is_Immigrant"))
df_test_m <- df_test_m %>% left_join(sd_age_m, by = c("Age", "Is_Male", "Is_Immigrant"))

Boot_Mig_Raw <- matrix(NA, nrow = nrow(df_test_m), ncol = B)

df_anchor_grid_m <- expand.grid(Year = (TRAIN_END - anchor_years_mig):TRAIN_END, Age = 0:100, Is_Male = c(1, 0), Is_Immigrant = c(1, 0))

for(i in 1:B) {
  boot_train <- df_train_m
  boot_train$Target <- predict(base_model_m, newdata = df_train_m) + rnorm(nrow(df_train_m), 0, df_train_m$sd_lin)
  
  boot_model <- brnn(Target ~ Year + Age + Is_Male + Is_Immigrant, data = boot_train, neurons = Nneurons_mig, verbose = FALSE)
  raw_pred <- predict(boot_model, newdata = df_test_m)
  
  df_anchor_grid_m$Pred <- predict(boot_model, newdata = df_anchor_grid_m)
  anchor_df <- df_anchor_grid_m %>% group_by(Age, Is_Male, Is_Immigrant) %>% summarize(Anchor = mean(Pred), .groups = "drop")
  anchor_pred <- left_join(df_test_m, anchor_df, by = c("Age", "Is_Male", "Is_Immigrant")) %>% pull(Anchor)
  
  damped_pred <- (1 - df_test_m$Weight) * raw_pred + df_test_m$Weight * anchor_pred
  Boot_Mig_Raw[, i] <- pmax(damped_pred / 1000, 0)
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
# The Core Projection Wrapper (Fully Stochastic & Vectorized)
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
      Pop_Proj[y_idx, , , sim] <- Current_Pop
      Next_Pop <- matrix(0, nrow = 101, ncol = 2) 
      
      e0_Proj[y_idx, 1, sim] <- calculate_e0(arr_deaths_rates[y_idx, , 1, idx_d])
      e0_Proj[y_idx, 2, sim] <- calculate_e0(arr_deaths_rates[y_idx, , 2, idx_d])
      
      fem_pop_repro <- Current_Pop[16:46, 2] 
      asfr <- arr_births_asfr[y_idx, , idx_b]
      TFR_Proj[y_idx, sim] <- sum(asfr)
      
      sim_births_repro <- rpois(length(fem_pop_repro), lambda = asfr * fem_pop_repro)
      total_births <- sum(sim_births_repro)
      
      pop_matrix <- round(Current_Pop) 
      
      lambda_imm <- arr_imm_rates[y_idx, , , idx_i] * pop_matrix
      imm_matrix <- matrix(rpois(n=length(pop_matrix), lambda_imm), nrow = 101, ncol = 2)
      
      p_emi <- pmin(arr_emi_rates[y_idx, , , idx_e], 1)
      emi_matrix <- matrix(rbinom(n=length(pop_matrix), size = pop_matrix, prob = p_emi), nrow = 101, ncol = 2)
      
      p_death <- pmin(arr_deaths_rates[y_idx, , , idx_d], 1)
      p_death[101, ] <- 0.45 
      deaths_matrix <- matrix(rbinom(n=length(pop_matrix), size = pop_matrix, prob = p_death), nrow = 101, ncol = 2)
      
      over_idx <- (deaths_matrix + emi_matrix) > pop_matrix
      emi_matrix[over_idx] <- pop_matrix[over_idx] - deaths_matrix[over_idx] 
      
      survivors_matrix <- pop_matrix - deaths_matrix + imm_matrix - emi_matrix
      survivors_matrix <- pmax(survivors_matrix, 0)
      
      Next_Pop[2:100, ] <- survivors_matrix[1:99, ]
      Next_Pop[101, ] <- survivors_matrix[100, ] + survivors_matrix[101, ]
      
      male_births <- rbinom(1, size = total_births, prob = 0.5122)
      Next_Pop[1, 1] <- male_births
      Next_Pop[1, 2] <- total_births - male_births
      
      Births_Proj[y_idx, sim] <- total_births
      Deaths_Proj[y_idx, sim] <- sum(deaths_matrix)
      Imm_Proj[y_idx, sim] <- sum(imm_matrix)
      Emi_Proj[y_idx, sim] <- sum(emi_matrix)
      
      Current_Pop <- Next_Pop
    }
  }
  
  return(list(Name = scenario_name, Pop = Pop_Proj, e0 = e0_Proj, TFR = TFR_Proj, Births = Births_Proj, Deaths = Deaths_Proj, Imm = Imm_Proj, Emi = Emi_Proj))
}


# ---------------------------------------------------------
# Execute Projection Scenarios (Direct Multipliers)
# ---------------------------------------------------------
cat("Running Stochastic Scenario Engine...\n")

scenario_results <- lapply(names(scenario_defs), function(s_name) {
  params <- scenario_defs[[s_name]]
  
  # Apply scenario multipliers directly to the forecasted structural rates (instant step-function)
  scen_births_asfr <- Boot_ASFR_Rates * params$fert
  scen_imm_rates   <- Boot_Imm_Rates * params$imm
  scen_emi_rates   <- Boot_Emi_Rates * params$emi
  
  # Execute the Poisson/Binomial engine
  run_scenario(s_name, scen_births_asfr, Boot_Mort_Rates, scen_imm_rates, scen_emi_rates, n_sims = S_sims)
})
names(scenario_results) <- names(scenario_defs)


###############################################################################
### PART 4: VISUALIZATIONS & STANDARDIZED CONFIDENCE BANDS (60% & 80%)      ###
###############################################################################
cat("Generating Visualizations...\n")

build_fan_df <- function(sim_matrix, baseline_val) {
  df <- data.frame(
    Year = PROJ_YEARS, 
    Median = apply(sim_matrix, 1, median),
    Q10 = apply(sim_matrix, 1, quantile, probs = 0.10),  
    Q90 = apply(sim_matrix, 1, quantile, probs = 0.90),  
    Q20 = apply(sim_matrix, 1, quantile, probs = 0.20),  
    Q80 = apply(sim_matrix, 1, quantile, probs = 0.80)   
  )
  base_df <- data.frame(Year=TRAIN_END, Median=baseline_val, Q10=baseline_val, Q90=baseline_val, Q20=baseline_val, Q80=baseline_val)
  bind_rows(base_df, df)
}

plot_fan <- function(df, title, y_label, color_hex) {
  ggplot(df, aes(x = Year)) +
    geom_ribbon(aes(ymin = Q10, ymax = Q90), fill = color_hex, alpha = 0.2) +
    geom_ribbon(aes(ymin = Q20, ymax = Q80), fill = color_hex, alpha = 0.4) +
    geom_line(aes(y = Median), color = color_hex, linewidth = 1.2) +
    geom_point(aes(y = Median), color = color_hex, size = 2) +
    scale_y_continuous(labels = comma) +
    ggtitle(title, subtitle = "60% and 90% prediction intervals") + 
    ylab(y_label) + xlab("Year") + theme_minimal() + theme(plot.title = element_text(face = "bold"))
}

plot_fan_1 <- function(df, title, y_label, color_hex, paths_df = NULL) {
  p <- ggplot(df, aes(x = Year)) +
    geom_ribbon(aes(ymin = Q10, ymax = Q90), fill = color_hex, alpha = 0.2) +
    geom_ribbon(aes(ymin = Q20, ymax = Q80), fill = color_hex, alpha = 0.4)
  
  if (!is.null(paths_df)) {
    p <- p + geom_line(data = paths_df, aes(x = Year, y = Value, group = Sim),
                       color = "black", alpha = 0.15, linewidth = 0.4)
  }
  p <- p +
    geom_line(aes(y = Median), color = color_hex, linewidth = 1.2) +
    geom_point(aes(y = Median), color = color_hex, size = 2) +
    scale_y_continuous(labels = comma) +
    ggtitle(title, subtitle = "60% and 80% prediction intervals with simulated paths") + 
    ylab(y_label) + xlab("Year") + theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  return(p)
}

# --- EXTRACT GLOBALS FOR LEGACY PLOTS ---
Pop_Proj    <- scenario_results[["Baseline"]]$Pop
e0_Proj     <- scenario_results[["Baseline"]]$e0
Births_Proj <- scenario_results[["Baseline"]]$Births
Deaths_Proj <- scenario_results[["Baseline"]]$Deaths
Imm_Proj    <- scenario_results[["Baseline"]]$Imm
Emi_Proj    <- scenario_results[["Baseline"]]$Emi

anchor_pop    <- df_pop_clean %>% filter(Year == TRAIN_END) %>% pull(Count) %>% sum()
anchor_births <- df_births %>% filter(Year == TRAIN_END) %>% pull(Births) %>% sum()
anchor_deaths <- df_deaths %>% filter(Year == TRAIN_END) %>% pull(Deaths) %>% sum()
anchor_imm    <- df_mig_tot %>% filter(Year == TRAIN_END, Is_Immigrant == 1, Age <= 100) %>% pull(Count) %>% sum()
anchor_emi    <- df_mig_tot %>% filter(Year == TRAIN_END, Is_Immigrant == 0, Age <= 100) %>% pull(Count) %>% sum()

# Spaghetti Plot 
tot_pop_sims <- apply(Pop_Proj, c(1, 4), sum) 
sample_sims <- sample(1:S_sims, 20)
paths_df <- data.frame(
  Year = rep(PROJ_YEARS, times = length(sample_sims)),
  Sim = rep(sample_sims, each = Y_LEN),
  Value = as.vector(tot_pop_sims[, sample_sims])
)
anchor_paths <- data.frame(
  Year = rep(TRAIN_END, length(sample_sims)),
  Sim = sample_sims,
  Value = rep(anchor_pop, length(sample_sims))
)
paths_df <- bind_rows(anchor_paths, paths_df) %>% arrange(Sim, Year)

plot_spaghetti <- plot_fan_1(
  df = build_fan_df(tot_pop_sims, anchor_pop),
  title = "Total Projected Population (Baseline)",
  y_label = "Individuals",
  color_hex = "#03396c",
  paths_df = paths_df
)
print(plot_spaghetti)


# Simple dynamic outputs
print(plot_fan(build_fan_df(Births_Proj, anchor_births), "Total Projected Births (Baseline)", "Live Births", "#4a148c"))
print(plot_fan(build_fan_df(Deaths_Proj, anchor_deaths), "Total Projected Deaths (Baseline)", "Deaths", "#b71c1c"))
print(plot_fan(build_fan_df(Imm_Proj, anchor_imm), "Total Projected Immigration (Baseline)", "Immigrants", "#004d40"))
print(plot_fan(build_fan_df(Emi_Proj, anchor_emi), "Total Projected Emigration (Baseline)", "Emigrants", "#e65100"))
Net_Mig_Sims <- Imm_Proj - Emi_Proj
print(plot_fan(build_fan_df(Net_Mig_Sims, anchor_imm - anchor_emi), "Total Projected Net Migration", "Net Migrants", "#00838f"))


# ---------------------------------------------------------
#  8. Age-Specific Component Facets
# ---------------------------------------------------------
cat("Calculating age-specific facets for Births, Deaths, and Migration...\n")

b_quantiles <- t(apply(Boot_ASFR_Raw, 1, quantile, probs = c(0.10, 0.20, 0.5, 0.80, 0.90)))
df_test_b_plot <- df_test_b %>% mutate(Q10 = b_quantiles[, 1], Q20 = b_quantiles[, 2], Median = b_quantiles[, 3], Q80 = b_quantiles[, 4], Q90 = b_quantiles[, 5])
df_train_b_plot <- df_train_b %>% mutate(Fitted = pmax(predict(base_model_b, newdata = df_train_b) / 1000, 0))
target_ages_b <- c(20, 25, 30, 35)

plot_births_facet <- ggplot() +
  geom_point(data = filter(df_train_b_plot, Age %in% target_ages_b), aes(x = Year, y = ASFR), color = "black", alpha = 0.5) +
  geom_line(data = filter(df_train_b_plot, Age %in% target_ages_b), aes(x = Year, y = Fitted), color = "#4a148c", linetype = "dashed") +
  geom_ribbon(data = filter(df_test_b_plot, Age %in% target_ages_b), aes(x = Year, ymin = Q10, ymax = Q90), fill = "#4a148c", alpha = 0.2) +
  geom_ribbon(data = filter(df_test_b_plot, Age %in% target_ages_b), aes(x = Year, ymin = Q20, ymax = Q80), fill = "#4a148c", alpha = 0.4) +
  geom_line(data = filter(df_test_b_plot, Age %in% target_ages_b), aes(x = Year, y = Median), color = "#4a148c", linewidth = 1) +
  facet_wrap(~ Age, labeller = label_both, scales = "free_y") +
  ggtitle("NN Forecast of ASFR by Mother's Age") + ylab("Fertility Rate (ASFR)") + xlab("Year") + theme_bw()
print(plot_births_facet)

d_quantiles <- t(apply(Boot_ASMR_Raw, 1, quantile, probs = c(0.10, 0.20, 0.5, 0.80, 0.90)))
df_test_d_plot <- df_test_d %>% mutate(Gender = ifelse(Is_Male == 1, "Male", "Female"), Q10 = d_quantiles[, 1], Q20 = d_quantiles[, 2], Median = d_quantiles[, 3], Q80 = d_quantiles[, 4], Q90 = d_quantiles[, 5])
df_train_d_plot <- df_train_d %>% mutate(Gender = ifelse(Is_Male == 1, "Male", "Female"), Fitted = pmax((exp(predict(base_model_d, newdata = df_train_d) + (sd_log^2)/2) - 1) / 100000, 0))
target_ages_d <- c(0, 40, 80)

plot_deaths_facet <- ggplot() +
  geom_point(data = filter(df_train_d_plot, Age %in% target_ages_d), aes(x = Year, y = Mort_Rate), color = "black", alpha = 0.4) +
  geom_line(data = filter(df_train_d_plot, Age %in% target_ages_d), aes(x = Year, y = Fitted), color = "#b71c1c", linetype = "dashed") +
  geom_ribbon(data = filter(df_test_d_plot, Age %in% target_ages_d), aes(x = Year, ymin = Q10, ymax = Q90), fill = "#b71c1c", alpha = 0.2) +
  geom_ribbon(data = filter(df_test_d_plot, Age %in% target_ages_d), aes(x = Year, ymin = Q20, ymax = Q80), fill = "#b71c1c", alpha = 0.4) +
  geom_line(data = filter(df_test_d_plot, Age %in% target_ages_d), aes(x = Year, y = Median), color = "#b71c1c", linewidth = 1) +
  facet_grid(Gender ~ Age, scales = "free_y", labeller = label_both) +
  ggtitle("NN Forecast of ASMR by Age and Gender") + ylab("Mortality Rate (ASMR)") + xlab("Year") + theme_bw() +
  theme(strip.background = element_rect(fill = "lightgray"))
print(plot_deaths_facet)

m_quantiles <- t(apply(Boot_Mig_Raw, 1, quantile, probs = c(0.10, 0.20, 0.5, 0.80, 0.90)))
df_test_m_plot <- df_test_m %>% mutate(Gender = ifelse(Is_Male == 1, "Male", "Female"), Flow = ifelse(Is_Immigrant == 1, "Immigrant", "Emigrant"), Q10 = m_quantiles[, 1], Q20 = m_quantiles[, 2], Median = m_quantiles[, 3], Q80 = m_quantiles[, 4], Q90 = m_quantiles[, 5])
df_train_m_plot <- df_train_m %>% mutate(Gender = ifelse(Is_Male == 1, "Male", "Female"), Flow = ifelse(Is_Immigrant == 1, "Immigrant", "Emigrant"), Fitted = pmax(predict(base_model_m, newdata = df_train_m) / 1000, 0))
target_ages_m <- c(25, 30, 35, 65)

plot_mig_facet <- ggplot() +
  geom_point(data = filter(df_train_m_plot, Age %in% target_ages_m), aes(x = Year, y = Mig_Rate, color = Flow), alpha = 0.3, size = 1) +
  geom_line(data = filter(df_train_m_plot, Age %in% target_ages_m), aes(x = Year, y = Fitted, color = Flow), linetype = "dashed") +
  geom_ribbon(data = filter(df_test_m_plot, Age %in% target_ages_m), aes(x = Year, ymin = Q10, ymax = Q90, fill = Flow), alpha = 0.2) +
  geom_ribbon(data = filter(df_test_m_plot, Age %in% target_ages_m), aes(x = Year, ymin = Q20, ymax = Q80, fill = Flow), alpha = 0.4) +
  geom_line(data = filter(df_test_m_plot, Age %in% target_ages_m), aes(x = Year, y = Median, color = Flow), linewidth = 1) +
  facet_grid(Gender ~ Age, scales = "free_y", labeller = label_both) +
  scale_color_manual(values = c("Immigrant" = "#004d40", "Emigrant" = "#e65100")) + scale_fill_manual(values = c("Immigrant" = "#004d40", "Emigrant" = "#e65100")) +
  ggtitle("NN Forecast of Migration Rates by Age and Gender") + ylab("Migration Rate") + xlab("Year") + theme_bw() + theme(strip.background = element_rect(fill = "lightgray"), legend.position = "bottom")
print(plot_mig_facet)


# --- 7. Pyramid plots ---
pop_fin_sims <- Pop_Proj[Y_LEN, , , ] 
male_sims <- pop_fin_sims[, 1, ]
female_sims <- pop_fin_sims[, 2, ]
df_pyramid <- data.frame(Age = rep(0:100, times = 2), Gender = rep(c("Male", "Female"), each = 101))
df_pyramid$Median <- c(apply(male_sims, 1, median), apply(female_sims, 1, median))
## df_pyramid$Lower  <- c(apply(male_sims, 1, quantile, probs = 0.10), apply(female_sims, 1, quantile, probs = 0.10))
## df_pyramid$Upper  <- c(apply(male_sims, 1, quantile, probs = 0.90), apply(female_sims, 1, quantile, probs = 0.90))
df_pyramid$Lower  <- c(apply(male_sims, 1, quantile, probs = 0.20), apply(female_sims, 1, quantile, probs = 0.20))
df_pyramid$Upper  <- c(apply(male_sims, 1, quantile, probs = 0.80), apply(female_sims, 1, quantile, probs = 0.80))

df_plot_pyr <- df_pyramid %>% mutate(Plot_Median = ifelse(Gender == "Male", -Median, Median), Plot_Lower = ifelse(Gender == "Male", -Upper, Lower), Plot_Upper = ifelse(Gender == "Male", -Lower, Upper))

plot_pyramid <- ggplot(df_plot_pyr, aes(x = Age, fill = Gender, color = Gender)) +
  # geom_col(aes(y = Plot_Median), width = 0.8, alpha = 0.6, color = NA) +
  geom_col(aes(y = Plot_Median), width = 0.8, alpha = 0.8, color = NA) +
  # geom_linerange(aes(ymin = Plot_Lower, ymax = Plot_Upper), linewidth = 0.8) +
  geom_linerange(aes(ymin = Plot_Lower, ymax = Plot_Upper), linewidth = 0.8, alpha=0.3) +
  coord_flip() + 
  scale_y_continuous(labels = function(x) scales::comma(abs(x))) + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_fill_manual(values = c("Male" = "#3b5998", "Female" = "#8b9dc3")) + 
  scale_color_manual(values = c("Male" = "#1c2b4a", "Female" = "#3b4a6b")) + 
  ggtitle(sprintf("Stochastic Population Pyramid at Year %d (60%% confidence bands)", PROJ_END)) + 
  ylab("Population") + 
  xlab("Age") + theme_minimal() + theme(legend.position = "bottom")
print(plot_pyramid)


# --- 8. Life Expectancy (e0) Fans ---
deaths_base_m <- df_train_d %>% filter(Year == TRAIN_END, Is_Male == 1) %>% arrange(Age) %>% pull(Deaths)
deaths_base_f <- df_train_d %>% filter(Year == TRAIN_END, Is_Male == 0) %>% arrange(Age) %>% pull(Deaths)
hist_pop_base_m <- df_train_d %>% filter(Year == TRAIN_END, Is_Male == 1) %>% arrange(Age) %>% pull(Pop_Count)
hist_pop_base_f <- df_train_d %>% filter(Year == TRAIN_END, Is_Male == 0) %>% arrange(Age) %>% pull(Pop_Count)

base_e0_m <- calculate_e0(deaths_base_m / hist_pop_base_m)
base_e0_f <- calculate_e0(deaths_base_f / hist_pop_base_f)

e0_Combined <- bind_rows(
  build_fan_df(e0_Proj[, 1, ], base_e0_m) %>% mutate(Gender = "Male"),
  build_fan_df(e0_Proj[, 2, ], base_e0_f) %>% mutate(Gender = "Female")
)

plot_e0_combined <- ggplot(e0_Combined, aes(x = Year, fill = Gender, color = Gender)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q20, ymax = Q80), alpha = 0.4, color = NA) +
  geom_line(aes(y = Median), linewidth = 1.2) + geom_point(aes(y = Median), size = 2) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Male" = "#1565c0", "Female" = "#c62828")) + scale_color_manual(values = c("Male" = "#0d47a1", "Female" = "#b71c1c")) +
  ggtitle("Stochastic Projection: Life Expectancy at Birth (e0)", subtitle = "60% and 90% probability bands") + ylab("Life Expectancy (Years)") + xlab("Year") + theme_minimal() + theme(legend.position = "bottom")
print(plot_e0_combined)


# --- Scenario Comparison: Total Population ---
compare_dfs <- lapply(scenario_results, function(scen) {
  df <- build_fan_df(apply(scen$Pop, c(1, 4), sum), anchor_pop)
  df$Scenario <- scen$Name
  return(df)
})

plot_scen_comp <- ggplot(bind_rows(compare_dfs), aes(x = Year, fill = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q20, ymax = Q80), alpha = 0.4, color = NA) +
  geom_line(aes(y = Median), linewidth = 1.2) + geom_point(aes(y = Median), size = 2) +
  scale_y_continuous(labels = comma) + scale_fill_manual(values = c("Baseline" = "#03396c", "High Growth" = "#004d40", "Dem Winter" = "#b71c1c")) + scale_color_manual(values = c("Baseline" = "#03396c", "High Growth" = "#004d40", "Dem Winter" = "#b71c1c")) +
  ggtitle("Stochastic Scenario Comparison: Total Population", subtitle = "60% and 80% prediction intervals shown") + ylab("Total Individuals") + xlab("Year") + theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
print(plot_scen_comp)


# --- Scenario Comparison: TFR ---
anchor_tfr <- sum(df_train_b$ASFR[df_train_b$Year == TRAIN_END])
compare_dfs_tfr <- lapply(scenario_results, function(scen) {
  df <- build_fan_df(scen$TFR, anchor_tfr)
  df$Scenario <- scen$Name
  return(df)
})

plot_tfr_comp <- ggplot(bind_rows(compare_dfs_tfr), aes(x = Year, fill = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q20, ymax = Q80), alpha = 0.4, color = NA) +
  geom_line(aes(y = Median), linewidth = 1.2) + geom_point(aes(y = Median), size = 2) +
  geom_hline(yintercept = 2.1, color = "black", linetype = "dashed", linewidth = 0.8, alpha = 0.6) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + scale_fill_manual(values = c("Baseline" = "#03396c", "High Growth" = "#004d40", "Dem Winter" = "#b71c1c")) + scale_color_manual(values = c("Baseline" = "#03396c", "High Growth" = "#004d40", "Dem Winter" = "#b71c1c")) +
  ggtitle("Stochastic Scenario Comparison: Total Fertility Rate (TFR)") + ylab("Children per Woman") + xlab("Year") + theme_minimal() + theme(legend.position = "bottom")
print(plot_tfr_comp)

# --- Scenario Comparison: Net Migration ---
compare_dfs_netmig <- list()
for (scen in names(scenario_results)) {
  sim_imm <- scenario_results[[scen]]$Imm
  sim_emi <- scenario_results[[scen]]$Emi
  if (length(dim(sim_imm)) > 2) {
    sim_imm <- apply(sim_imm, c(1, length(dim(sim_imm))), sum)
    sim_emi <- apply(sim_emi, c(1, length(dim(sim_emi))), sum)
  }
  sim_net_mig <- sim_imm - sim_emi
  compare_dfs_netmig[[scen]] <- data.table(
    Year = PROJ_YEARS, Scenario = scen, Median = apply(sim_net_mig, 1, median),
    Q10 = apply(sim_net_mig, 1, quantile, probs = 0.10), Q90 = apply(sim_net_mig, 1, quantile, probs = 0.90),
    Q20 = apply(sim_net_mig, 1, quantile, probs = 0.20), Q80 = apply(sim_net_mig, 1, quantile, probs = 0.80)
  )
}
df_comp_netmig <- rbindlist(compare_dfs_netmig)
if (all(c("High Growth", "Baseline", "Dem Winter") %in% unique(df_comp_netmig$Scenario))) {
  df_comp_netmig[, Scenario := factor(Scenario, levels = c("High Growth", "Baseline", "Dem Winter"))]
}

plot_netmig_comp <- ggplot(df_comp_netmig, aes(x = Year, color = Scenario, fill = Scenario)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q20, ymax = Q80), alpha = 0.4, color = NA) +
  geom_line(aes(y = Median), linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) + scale_color_viridis_d(option = "turbo") + scale_fill_viridis_d(option = "turbo") +
  labs(title = "Scenario Comparison: Projected Net Migration", subtitle = "60% and 80% Confidence Intervals", y = "Net Migration (Individuals)", x = "Year") + theme_bw() + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
print(plot_netmig_comp)



# --- 9. Age Schedules ---
target_years_cross <- c(2010, TRAIN_END, 2050, 2075)
plot_age_b <- ggplot(bind_rows(
  df_train_b_plot %>% filter(Year %in% target_years_cross) %>% select(Year, Age, Rate = Fitted) %>% mutate(Period = "Historical"),
  df_test_b_plot %>% filter(Year %in% target_years_cross) %>% select(Year, Age, Rate = Median) %>% mutate(Period = "Projected")
) %>% mutate(Year_Factor = as.factor(Year)), aes(x = Age, y = Rate, color = Year_Factor, linetype = Period)) + geom_line(linewidth = 1.2) + scale_color_viridis_d(option = "plasma", end = 0.8) + ggtitle("ASFR Across Time") + theme_minimal()
print(plot_age_b)

plot_age_d <- ggplot(bind_rows(
  df_train_d_plot %>% filter(Year %in% target_years_cross) %>% 
    select(Year, Age, Gender, Rate = Fitted) %>% 
    mutate(Period = "Historical"),
  df_test_d_plot %>% filter(Year %in% target_years_cross) %>% 
    select(Year, Age, Gender, Rate = Median) %>% 
    mutate(Period = "Projected")
) %>% 
  mutate(Year_Factor = as.factor(Year)), aes(x = Age, y = Rate, color = Year_Factor, linetype = Period)) + 
  geom_line(linewidth = 1) + 
  scale_y_log10(labels = scales::comma) + 
  facet_wrap(~ Gender) + scale_color_viridis_d(option = "mako", end = 0.8) + 
  ggtitle("ASMR Across Time (Log Scale)") + 
  theme_bw()
print(plot_age_d)

plot_age_m <- ggplot(bind_rows(
  df_train_m_plot %>% filter(Year %in% target_years_cross) %>% select(Year, Age, Gender, Flow, Rate = Fitted) %>% mutate(Period = "Historical"),
  df_test_m_plot %>% filter(Year %in% target_years_cross) %>% select(Year, Age, Gender, Flow, Rate = Median) %>% mutate(Period = "Projected")
) %>% mutate(Year_Factor = as.factor(Year)), aes(x = Age, y = Rate, color = Year_Factor, linetype = Period)) + geom_line(linewidth = 1) + facet_grid(Flow ~ Gender) + scale_color_viridis_d(option = "viridis", end = 0.8) + ggtitle("Age-Specific Migration Rates Across Time") + theme_bw()
print(plot_age_m)

# --- 10. Sex Ratio ---
df_hist_sex_ratio <- df_pop_clean %>% 
  group_by(Year, Gender) %>% 
  summarize(Total = sum(Count), .groups = "drop") %>% 
  pivot_wider(names_from = Gender, values_from = Total) %>% 
  mutate(Sex_Ratio = (Male / Female) * 100)
df_sr_fan <- build_fan_df((apply(Pop_Proj[, , 1, ], c(1, 3), sum) / apply(Pop_Proj[, , 2, ], c(1, 3), sum)) * 100, df_hist_sex_ratio %>% filter(Year == TRAIN_END) %>% pull(Sex_Ratio))

plot_sex_ratio <- ggplot() +
  geom_ribbon(data = df_sr_fan, aes(x = Year, ymin = Q10, ymax = Q90), fill = "#d4af37", alpha = 0.2) +
  geom_ribbon(data = df_sr_fan, aes(x = Year, ymin = Q20, ymax = Q80), fill = "#d4af37", alpha = 0.4) +
  geom_line(data = df_sr_fan, aes(x = Year, y = Median), color = "#b5952f", linewidth = 1.2) +
  geom_line(data = df_hist_sex_ratio, aes(x = Year, y = Sex_Ratio), color = "black", linewidth = 1.2) + 
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") + 
  labs(y=NULL) +
  theme_minimal() + 
  ggtitle("Total Population Sex Ratio")
print(plot_sex_ratio)


# --- 11. Trajectories ---
target_ages <- c(0, 18, 30, 65, 80)
df_hist_ages <- df_pop_clean %>% filter(Age %in% target_ages) %>% group_by(Year, Age) %>% summarize(Total = sum(Count), .groups = "drop")
df_proj_ages <- bind_rows(lapply(target_ages, function(a) { df_fan <- build_fan_df(apply(Pop_Proj[, a + 1, , ], c(1, 3), sum), df_hist_ages %>% filter(Year == TRAIN_END, Age == a) %>% pull(Total)); df_fan$Age <- a; return(df_fan) }))

plot_selected_ages <- ggplot() +
  geom_ribbon(data = df_proj_ages, aes(x = Year, ymin = Q10, ymax = Q90, fill = factor(Age)), alpha = 0.2) +
  geom_ribbon(data = df_proj_ages, aes(x = Year, ymin = Q20, ymax = Q80, fill = factor(Age)), alpha = 0.4) +
  geom_line(data = df_proj_ages, aes(x = Year, y = Median, color = factor(Age)), linetype = "dashed", linewidth = 1) + geom_line(data = df_hist_ages, aes(x = Year, y = Total, color = factor(Age)), linewidth = 1.2) +
  facet_wrap(~ Age, scales = "free_y") + 
  labs(y=NULL) +
  theme_bw() + 
  ggtitle("Population Trajectories by Selected Age") + 
  scale_y_continuous(labels = comma)
print(plot_selected_ages)


# --- 11B & 11C. Groups and Median ---
df_hist_group_traj <- df_pop_clean %>% mutate(Age_Group = case_when(Age <= 19 ~ "0-19", Age >= 65 ~ "65+", TRUE ~ NA_character_)) %>% filter(!is.na(Age_Group)) %>% group_by(Year, Age_Group) %>% summarize(Total = sum(Count), .groups = "drop")
df_proj_group_traj <- bind_rows(build_fan_df(apply(Pop_Proj[, 1:20, , ], c(1, 4), sum), df_hist_group_traj %>% filter(Year == TRAIN_END, Age_Group == "0-19") %>% pull(Total)) %>% mutate(Age_Group = "0-19"),
                                build_fan_df(apply(Pop_Proj[, 66:101, , ], c(1, 4), sum), df_hist_group_traj %>% filter(Year == TRAIN_END, Age_Group == "65+") %>% pull(Total)) %>% mutate(Age_Group = "65+"))
plot_group_traj <- ggplot() +
  geom_ribbon(data = df_proj_group_traj, aes(x = Year, ymin = Q10, ymax = Q90, fill = Age_Group), alpha = 0.2) + 
  geom_ribbon(data = df_proj_group_traj, aes(x = Year, ymin = Q20, ymax = Q80, fill = Age_Group), alpha = 0.4) +
  geom_line(data = df_proj_group_traj, aes(x = Year, y = Median, color = Age_Group), linetype = "dashed") + 
  geom_line(data = df_hist_group_traj, aes(x = Year, y = Total, color = Age_Group), linewidth = 1.2) + 
  facet_wrap(~ Age_Group, scales = "free_y") + 
  labs(y=NULL) +
  theme_bw()
print(plot_group_traj)

calc_continuous_median_age <- function(pop_counts, ages = 0:100) { 
  target <- sum(pop_counts)/2; 
  cum_pop <- cumsum(pop_counts); 
  idx <- which(cum_pop >= target)[1]; 
  if(is.na(idx)) return(NA); 
  if(idx==1) return(ages[1] + target/pop_counts[1]); 
  prev_cum <- cum_pop[idx-1]; 
  return(ages[idx] + (target-prev_cum)/pop_counts[idx]) }
df_hist_median_age <- df_pop_clean %>% 
  group_by(Year, Age) %>% 
  summarize(Count = sum(Count), .groups = "drop") %>% 
  group_by(Year) %>% 
  summarize(Median_Age = calc_continuous_median_age(Count, Age), .groups = "drop")
df_med_age_fan <- build_fan_df(apply(apply(Pop_Proj, c(1, 2, 4), sum), c(1, 3), calc_continuous_median_age), df_hist_median_age %>% 
                                 filter(Year == TRAIN_END) %>% 
                                 pull(Median_Age))

plot_median_age <- ggplot() + 
  geom_ribbon(data = df_med_age_fan, aes(x = Year, ymin = Q10, ymax = Q90), fill = "#7570b3", alpha = 0.2) + 
  geom_ribbon(data = df_med_age_fan, aes(x = Year, ymin = Q20, ymax = Q80), fill = "#7570b3", alpha = 0.4) + 
  geom_line(data = df_med_age_fan, aes(x = Year, y = Median), color = "#3f3c7a", linewidth = 1.2) + 
  geom_line(data = df_hist_median_age, aes(x = Year, y = Median_Age), color = "black", linewidth = 1.2) + 
  labs(y=NULL) +
  theme_minimal()
print(plot_median_age)


# --- 12-15. Macros ---
age_labels <- c(paste(seq(0, 90, by = 10), seq(9, 99, by = 10), sep = "-"), "100+")
df_hist_grp <- df_pop_clean %>% 
  mutate(AgeGroup = cut(Age, breaks = c(seq(0, 100, by = 10), Inf), labels = age_labels, right = FALSE)) %>% 
  group_by(Year, AgeGroup) %>% 
  summarize(Count = sum(Count), .groups = "drop")
df_hist_all <- bind_rows(df_pop_clean %>% group_by(Year) %>% 
                           summarize(Count = sum(Count), .groups = "drop") %>% 
                           mutate(AgeGroup = "Total"), df_hist_grp) %>% mutate(AgeGroup = factor(AgeGroup, levels = c("Total", age_labels))) %>% group_by(AgeGroup) %>% arrange(Year) %>% mutate(PctChange = case_when(lag(Count)==0 & Count==0 ~ 0, lag(Count)==0 & Count>0 ~ 1, TRUE ~ (Count/lag(Count))-1)) %>% ungroup() %>% filter(!is.na(PctChange))
Pop_Proj_Ages <- apply(Pop_Proj, c(1, 2, 4), sum); Pop_Proj_Total <- apply(Pop_Proj, c(1, 4), sum)
calc_sim_change <- function(hist_data, proj_sims, group_name) { anchor_count <- hist_data %>% filter(Year == TRAIN_END, AgeGroup == group_name) %>% pull(Count); anchor_rate <- hist_data %>% filter(Year == TRAIN_END, AgeGroup == group_name) %>% pull(PctChange); full_pop_matrix <- rbind(rep(anchor_count, S_sims), proj_sims); change_sims <- (full_pop_matrix[-1, ] / full_pop_matrix[-nrow(full_pop_matrix), ]) - 1; change_sims[is.nan(change_sims)] <- 0; change_sims[is.infinite(change_sims)] <- 1; df_fan <- build_fan_df(change_sims, anchor_rate); df_fan$AgeGroup <- group_name; return(df_fan) }

proj_change_list <- list(Total = calc_sim_change(df_hist_all, Pop_Proj_Total, "Total"))
for(i in 1:length(age_labels)) { grp_name <- age_labels[i]; if(i == 11) grp_sims <- Pop_Proj_Ages[, 101, ] else { idx <- ((i-1)*10+1):(i*10); grp_sims <- apply(Pop_Proj_Ages[, idx, ], c(1, 3), sum) }; proj_change_list[[grp_name]] <- calc_sim_change(df_hist_all, grp_sims, grp_name) }
plot_yoy_change <- ggplot() + 
  geom_ribbon(data = bind_rows(proj_change_list), aes(x = Year, ymin = Q10, ymax = Q90, fill = AgeGroup), alpha = 0.2) + 
  geom_ribbon(data = bind_rows(proj_change_list), aes(x = Year, ymin = Q20, ymax = Q80, fill = AgeGroup), alpha = 0.4) + 
  geom_line(data = bind_rows(proj_change_list), aes(x = Year, y = Median, color = AgeGroup), linetype="dashed") + 
  geom_line(data = df_hist_all, aes(x = Year, y = PctChange, color = AgeGroup)) + 
  facet_wrap(~ AgeGroup, scales = "free_y", ncol = 3) +
  labs(y=NULL) +
  theme_bw()
print(plot_yoy_change)


df_hist_dep <- df_pop_clean %>% 
  mutate(Group = case_when(Age<=19 ~ "Youth", Age>=20 & Age<=64 ~ "Working", Age>=65 ~ "Elderly")) %>% 
  group_by(Year, Group) %>% 
  summarize(Total = sum(Count), .groups = "drop") %>% 
  pivot_wider(names_from = Group, values_from = Total) %>% 
  mutate(Dep_Ratio = ((Youth + Elderly) / Working) * 100)
df_dr_fan <- build_fan_df(((apply(Pop_Proj[, 1:20, , ], c(1, 4), sum) + apply(Pop_Proj[, 66:101, , ], c(1, 4), sum)) / apply(Pop_Proj[, 21:65, , ], c(1, 4), sum)) * 100, df_hist_dep %>% filter(Year == TRAIN_END) %>% 
                            pull(Dep_Ratio))
plot_dep_ratio <- ggplot() + 
  geom_ribbon(data = df_dr_fan, aes(x = Year, ymin = Q10, ymax = Q90), fill = "#c24a49", alpha = 0.2) + 
  geom_ribbon(data = df_dr_fan, aes(x = Year, ymin = Q20, ymax = Q80), fill = "#c24a49", alpha = 0.4) + 
  geom_line(data = df_dr_fan, aes(x = Year, y = Median), color = "#8a2a29", linewidth = 1.2) + 
  geom_line(data = df_hist_dep, aes(x = Year, y = Dep_Ratio), color = "black", linewidth = 1.2) + 
  labs(y=NULL) +
  theme_minimal()
print(plot_dep_ratio)


df_hist_ni <- df_births %>% group_by(Year) %>% 
  summarize(Births=sum(Births)) %>% 
  left_join(df_deaths %>% group_by(Year) %>% 
              summarize(Deaths=sum(Deaths)), by="Year") %>% 
  mutate(Nat_Inc = Births-Deaths)
df_ni_fan <- build_fan_df(Births_Proj - Deaths_Proj, df_hist_ni %>% 
                            filter(Year == TRAIN_END) %>% pull(Nat_Inc))
plot_nat_inc <- ggplot() + 
  geom_ribbon(data = df_ni_fan, aes(x = Year, ymin = Q10, ymax = Q90), fill = "#2ca25f", alpha = 0.2) + 
  geom_ribbon(data = df_ni_fan, aes(x = Year, ymin = Q20, ymax = Q80), fill = "#2ca25f", alpha = 0.4) + 
  geom_line(data = df_ni_fan, aes(x = Year, y = Median), color = "#006d2c") + 
  geom_line(data = df_hist_ni, aes(x=Year, y=Nat_Inc)) + 
  labs(y=NULL) +
  theme_minimal()
print(plot_nat_inc)


hist_pop_tot <- df_pop_clean %>% group_by(Year) %>% 
  summarize(Total=sum(Count)) %>% 
  mutate(Abs_Change = Total-lag(Total), Pct_Change = Abs_Change/lag(Total)) %>% 
  filter(!is.na(Abs_Change))
full_pop_matrix <- rbind(rep(hist_pop_tot %>% 
                               filter(Year==TRAIN_END) %>% 
                               pull(Total), S_sims), Pop_Proj_Total)
abs_change_sims <- full_pop_matrix[-1, ] - full_pop_matrix[-nrow(full_pop_matrix), ]
df_abs_fan <- build_fan_df(abs_change_sims, hist_pop_tot %>% 
                             filter(Year==TRAIN_END) %>% 
                             pull(Abs_Change))
df_pct_fan <- build_fan_df(abs_change_sims / full_pop_matrix[-nrow(full_pop_matrix), ], hist_pop_tot %>% 
                             filter(Year==TRAIN_END) %>% 
                             pull(Pct_Change))


plot_abs_change <- ggplot() + 
  geom_ribbon(data = df_abs_fan, aes(x=Year, ymin=Q10, ymax=Q90), fill="#1f78b4", alpha=0.2) + 
  geom_ribbon(data = df_abs_fan, aes(x=Year, ymin=Q20, ymax=Q80), fill="#1f78b4", alpha=0.4) + 
  geom_line(data=df_abs_fan, aes(x=Year, y=Median), color="#08519c") + 
  geom_line(data=hist_pop_tot, aes(x=Year, y=Abs_Change)) + 
  labs(y=NULL) +
  theme_minimal()
print(plot_abs_change)


plot_pct_change <- ggplot() + 
  geom_ribbon(data = df_pct_fan, aes(x=Year, ymin=Q10, ymax=Q90), fill="#d95f02", alpha=0.2) + 
  geom_ribbon(data = df_pct_fan, aes(x=Year, ymin=Q20, ymax=Q80), fill="#d95f02", alpha=0.4) + 
  geom_line(data=df_pct_fan, aes(x=Year, y=Median), color="#a63603") + 
  geom_line(data=hist_pop_tot, aes(x=Year, y=Pct_Change)) + 
  labs(y=NULL) +
  theme_minimal()
print(plot_pct_change)


###############################################################################
################ PART II: projections by type of family #######################
###############################################################################

# ---------------------------------------------------------
#  20 & 21. Fully Stochastic Dynamic Family Projections (Bootstrapped Dirichlet + TFR)
# ---------------------------------------------------------

cat("Projecting Dynamic Family Shares (Bootstrapped Dirichlet with TFR Covariate)...\n")

url_family <- "https://px.hagstofa.is:443/pxen/api/v1/en/Ibuar/fjolsk/Fjolskyldan/MAN07108.px"
df_family_hist <- pxweb_get(url = url_family, query = list("Kjarnafjölskyldur" = "2", "Sveitarfélag" = "-", "Fjölskyldugerð" = "*", "Ár" = "*"), verbose = FALSE) %>% as.data.frame(column.name.type = "text", variable.value.type = "text") %>% rename(Count = 5) %>% mutate(Year = as.numeric(as.character(Year)), Count = as.numeric(Count), Family_Type = case_when(`Type of family` == "Not in nuclear families" ~ "Ekki í kjarnafjölskyldu", `Type of family` %in% c("Married couple without children", "Consensual union without children") ~ "Par án barna", `Type of family` %in% c("Married couple with children", "Consensual union with children") ~ "Par með börn", `Type of family` %in% c("Father with children", "Mother with children") ~ "Einstætt foreldri", TRUE ~ "Annað")) %>% group_by(Year, Family_Type) %>% summarize(Total_Individuals = sum(Count, na.rm = TRUE), .groups = "drop") %>% arrange(Year, Family_Type)
df_family_shares <- df_family_hist %>% group_by(Year) %>% mutate(Share = Total_Individuals / sum(Total_Individuals)) %>% ungroup()

df_hist_tfr <- df_train_b %>% group_by(Year) %>% summarize(TFR = sum(ASFR, na.rm = TRUE), .groups = "drop") %>% filter(Year %in% unique(df_family_shares$Year))
dt_shares_wide <- merge(dcast(as.data.table(df_family_shares), Year ~ Family_Type, value.var = "Share"), df_hist_tfr, by = "Year")

Y_matrix <- as.matrix(dt_shares_wide[, c("Par með börn", "Par án barna", "Einstætt foreldri", "Ekki í kjarnafjölskyldu")])
Y_matrix <- Y_matrix / rowSums(Y_matrix)
base_year <- min(dt_shares_wide$Year)
df_dir_model <- data.frame(Year_Scaled = dt_shares_wide$Year - base_year, TFR = dt_shares_wide$TFR)
df_dir_model$Y <- DR_data(Y_matrix)

# --- 1. Base Model & Residual Calculation ---
mod_dir_base <- DirichReg(Y ~ Year_Scaled + TFR, data = df_dir_model)
hist_fitted <- predict(mod_dir_base)

# Calculate historical standard deviations for each family type's residuals
sd_shares <- apply(Y_matrix - hist_fitted, 2, sd)

# --- 2. Bootstrap the Dirichlet Models (Epistemic Uncertainty) ---
cat("Bootstrapping B=200 Dirichlet Structural Models...\n")
boot_dir_models <- list()

for(b in 1:B) {
  # Inject random noise based on historical residual variance
  noise_matrix <- matrix(rnorm(n = length(Y_matrix), mean = 0, sd = rep(sd_shares, each = nrow(Y_matrix))), ncol = ncol(Y_matrix))
  Y_boot <- Y_matrix + noise_matrix
  
  # Enforce compositional bounds (must be > 0 and sum to exactly 1)
  Y_boot <- pmax(Y_boot, 0.001)
  Y_boot <- Y_boot / rowSums(Y_boot)
  
  df_boot <- df_dir_model
  df_boot$Y <- DR_data(Y_boot)
  
  # Fit model. Use tryCatch to silently fall back to base model if random noise causes a singularity
  mod_boot <- tryCatch(DirichReg(Y ~ Year_Scaled + TFR, data = df_boot), error = function(e) mod_dir_base)
  boot_dir_models[[b]] <- mod_boot
}

# --- 3. Execute 1,000 Fully Stochastic Simulations ---
fam_prop_sims <- list()
for(f_type in colnames(Y_matrix)) {
  fam_prop_sims[[f_type]] <- matrix(NA, nrow = Y_LEN, ncol = S_sims)
}

sim_tfr_matrix <- scenario_results[["Baseline"]]$TFR

for(sim in 1:S_sims) {
  # Pick one bootstrapped structural model at random
  idx_b <- sample(1:B, 1)
  mod_sim <- boot_dir_models[[idx_b]]
  
  # Feed it this simulation's specific stochastic TFR trajectory
  df_sim_future <- data.frame(
    Year_Scaled = PROJ_YEARS - base_year,
    TFR = sim_tfr_matrix[, sim]
  )
  
  # Predict the shares for this specific run
  pred_sim <- predict(mod_sim, newdata = df_sim_future)
  
  # FIX: Explicitly re-apply column names to prevent the 'dimnames' error
  colnames(pred_sim) <- colnames(Y_matrix)
  
  # Distribute predictions back to the storage matrices
  for(f_type in colnames(Y_matrix)) {
    fam_prop_sims[[f_type]][, sim] <- pred_sim[, f_type]
  }
}

# --- 4. Visualize the Structural Shift with Uncertainty ---
shares_fan_list <- lapply(names(fam_prop_sims), function(f_type) {
  mat <- fam_prop_sims[[f_type]]
  data.table(
    Year = PROJ_YEARS, Family_Type = f_type, Type = "Projected (Bootstrapped Dirichlet)",
    Median = apply(mat, 1, median),
    Q10 = apply(mat, 1, quantile, probs = 0.10), Q20 = apply(mat, 1, quantile, probs = 0.20),
    Q80 = apply(mat, 1, quantile, probs = 0.80), Q90 = apply(mat, 1, quantile, probs = 0.90)
  )
})
df_proj_shares <- rbindlist(shares_fan_list)
dt_hist_plot <- as.data.table(df_family_shares)[, .(Year, Family_Type, Median = Share, Type = "Historical")]
df_combined_shares <- rbindlist(list(dt_hist_plot, df_proj_shares), fill = TRUE)[, Family_Type := factor(Family_Type, levels = c("Par með börn", "Par án barna", "Einstætt foreldri", "Ekki í kjarnafjölskyldu"))]

plot_shares_dir <- ggplot(df_combined_shares, aes(x = Year, fill = Family_Type, color = Family_Type)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q20, ymax = Q80), alpha = 0.4, color = NA) +
  geom_line(aes(y = Median, linetype = Type), linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_viridis_d(option = "turbo") + scale_fill_viridis_d(option = "turbo") +
  labs(title = "Forecasted Family Structure Propensities (Fully Stochastic)",
       subtitle = "60% and 80% Intervals driven by Bootstrapped Compositional Models & TFR",
       y = "Share of Total Population", x = "Year") +
  theme_bw() + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
print(plot_shares_dir)

# --- 5. Apply to Stochastic Population Matrix (Baseline) ---
sim_pop_total <- apply(scenario_results[["Baseline"]]$Pop, c(1, 4), sum) 
fam_proj_list <- list()

for (f_type in names(fam_prop_sims)) {
  # Element-wise multiplication of two fully stochastic matrices: [51x1000] * [51x1000]
  sim_fam_matrix <- sim_pop_total * fam_prop_sims[[f_type]]
  
  temp_dt <- data.table(
    Year = PROJ_YEARS, Family_Type = f_type,
    Median = apply(sim_fam_matrix, 1, median),
    Q10 = apply(sim_fam_matrix, 1, quantile, probs = 0.10), Q20 = apply(sim_fam_matrix, 1, quantile, probs = 0.20),
    Q80 = apply(sim_fam_matrix, 1, quantile, probs = 0.80), Q90 = apply(sim_fam_matrix, 1, quantile, probs = 0.90)
  )
  
  anchor_val <- df_family_hist %>% filter(Year == PROJ_YEARS[1], Family_Type == f_type) %>% pull(Total_Individuals)
  if(length(anchor_val) == 1) temp_dt[1, c("Median", "Q10", "Q20", "Q80", "Q90") := anchor_val]
  
  fam_proj_list[[f_type]] <- temp_dt
}

df_proj_family <- rbindlist(fam_proj_list)[, Family_Type := factor(Family_Type, levels = c("Par með börn", "Par án barna", "Einstætt foreldri", "Ekki í kjarnafjölskyldu"))]

# --- 6. Visualize Absolute Stochastic Counts ---
plot_family_counts_dir <- ggplot() +
  geom_ribbon(data = df_proj_family, aes(x = Year, ymin = Q10, ymax = Q90, fill = Family_Type), alpha = 0.2) +
  geom_ribbon(data = df_proj_family, aes(x = Year, ymin = Q20, ymax = Q80, fill = Family_Type), alpha = 0.4) +
  geom_line(data = df_proj_family, aes(x = Year, y = Median, color = Family_Type), linetype = "dashed", linewidth = 1) + 
  geom_line(data = df_family_hist, aes(x = Year, y = Total_Individuals, color = Family_Type), linewidth = 1.2) + 
  facet_wrap(~ Family_Type, scales = "free_y") + scale_y_continuous(labels = scales::comma) + 
  scale_color_viridis_d(option = "turbo") + scale_fill_viridis_d(option = "turbo") + 
  labs(title = "Stochastic Projection: Individuals by Family Type", 
       subtitle = "Applying fully stochastic Dirichlet propensities to Baseline population",
       y = "Number of Individuals", x = "Year") + 
  theme_bw() + theme(legend.position = "none", plot.title = element_text(face = "bold"), 
                     strip.background = element_rect(fill = "lightgray"), strip.text = element_text(face = "bold"))

print(plot_family_counts_dir)


#############################################################
# ---------------------------------------------------------
#### Compile Master PDF Report
# ---------------------------------------------------------
#############################################################

cat("Compiling all visual reports into PDF...\n")
plot_list <- list(
  plot_spaghetti, plot_scen_comp, plot_tfr_comp, plot_netmig_comp,
  plot_pyramid, ## plot_e0_combined, 
  plot_sex_ratio,
  plot_selected_ages, plot_group_traj, plot_yoy_change, plot_dep_ratio,
  plot_median_age, plot_nat_inc, plot_abs_change, plot_pct_change,
  # plot_age_b, plot_age_d, plot_age_m,
  plot_shares_dir, plot_family_counts_dir
)

pdf_name <- sprintf("SIPP_Projections_Report_%s.pdf", format(Sys.Date(), "%Y%m%d"))
pdf(pdf_name, width = 11, height = 8.5)
for (p in plot_list) tryCatch(print(p), error = function(e) cat("Skipped a plot due to error.\n")) 
dev.off()
cat(sprintf("Success: Compiled demographic report saved as %s\n", pdf_name))


