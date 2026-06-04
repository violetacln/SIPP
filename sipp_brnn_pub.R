

## stochastic population projections with
## Bayesian regularized Neural Networks (2 layers)
## including model and data uncertainty effects
## using simulated data

################################## Births ##########################################

# Load necessary libraries
library(brnn)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# 1. Synthesize Realistic Demographic Data (Births by Age and Year)
# ---------------------------------------------------------
set.seed(42)

# Create a grid of every age (15-45) for every year (2000-2030)
df <- expand.grid(Year = 2000:2030, Age = 15:45)

# Synthesize Births: 
# - A bell curve peaking at a certain age.
# - The peak age shifts slightly older by 0.1 years every year.
# - The total volume of births declines slightly every year.
df <- df %>%
  mutate(
    # The peak age of motherhood starts at 26 in 2000, shifting older over time
    Peak_Age = 26 + (Year - 2000) * 0.1,
    
    # Overall volume drops slightly over the years
    Volume_Multiplier = 1 - (Year - 2000) * 0.01,
    
    # Calculate the bell curve (Normal PDF shape)
    Base_Births = 5000 * exp(-0.5 * ((Age - Peak_Age) / 4.5)^2) * Volume_Multiplier,
    
    # Add random demographic noise (Poisson-like variation)
    Births = Base_Births + rnorm(n(), mean = 0, sd = 150)
  ) %>%
  # Ensure no negative births due to random noise
  mutate(Births = pmax(Births, 0))  %>%
  select(Year, Age, Births)                       


# Split into Training (2000-2023) and Testing/Forecasting (2024-2030)
df_train_0 <- df %>% filter(Year <= 2023)
df_test_0  <- df %>% filter(Year > 2023)


## if use the simulated data:
df_train <- df_train_0
df_test <- df_test_0

## if not, assign here the real data with correct structure

# ---------------------------------------------------------
# 2. Fit the Base brnn Model (2D Feature Space)
# ---------------------------------------------------------
cat("Training the demographic brnn model...\n")

# We use both Year and Age to predict Births. 
# We increase neurons to 3 to help it learn the bell-curve shape across ages.
set.seed(123)
base_model <- brnn(
  Births ~ Year + Age, 
  data = df_train, 
  neurons = 5, 
  verbose = FALSE
)

# Extract fitted values, residuals, and base point forecast
base_fitted <- predict(base_model, newdata = df_train)
base_residuals <- df_train$Births - base_fitted
base_forecast <- predict(base_model, newdata = df_test)


# ---------------------------------------------------------
# 3. The Residual Bootstrap Loop
# ---------------------------------------------------------
B <- 100
n_test <- nrow(df_test)
boot_forecasts <- matrix(NA, nrow = n_test, ncol = B)

cat(sprintf("Running %d bootstrap iterations for the demographic surface...\n", B))

for(i in 1:B) {
  
  # a. Create an alternate historical reality by shuffling the residuals
  boot_train <- df_train
  boot_train$Births <- base_fitted + sample(base_residuals, replace = TRUE)
  
  # b. Refit the neural network to the alternate history
  boot_model <- brnn(
    Births ~ Year + Age, 
    data = boot_train, 
    neurons = 5, 
    verbose = FALSE
  )
  
  # c. Predict the future surface
  boot_pred <- predict(boot_model, newdata = df_test)
  
  # d. Add future demographic noise to the forecast
  boot_forecasts[, i] <- boot_pred + sample(base_residuals, size = n_test, replace = TRUE)
}


Boot_Births <- boot_forecasts    


# ---------------------------------------------------------
# 4. Calculate Quantiles and Format Output
# ---------------------------------------------------------
# Calculate the 2.5%, 10%, 90%, and 97.5% quantiles
quantiles <- apply(boot_forecasts, 1, quantile, probs = c(0.025, 0.10, 0.90, 0.975))
quantiles <- t(quantiles)

# Store in our test dataframe
df_test_forecast <- df_test %>%
  mutate(
    Estimate = base_forecast,
    Q2.5     = quantiles[, 1],
    Q10      = quantiles[, 2],
    Q90      = quantiles[, 3],
    Q97.5    = quantiles[, 4]
  ) %>%
  # Truncate negative prediction intervals to 0 (can't have negative births)
  mutate(across(c(Estimate, Q2.5, Q10, Q90, Q97.5), ~pmax(., 0)))

df_train_fitted <- df_train %>% mutate(Estimate = pmax(base_fitted, 0))

# ---------------------------------------------------------
# 5. Visualize Selected Age Groups using Facets
# ---------------------------------------------------------
# Plotting all 30 ages on one graph is too messy. 
# We filter to look at exactly three representative age groups: 20, 30, and 40.
target_ages <- c(20, 30, 40)

plot_train <- df_train %>% filter(Age %in% target_ages)
plot_train_fit <- df_train_fitted %>% filter(Age %in% target_ages)
plot_test <- df_test_forecast %>% filter(Age %in% target_ages)

ggplot() +
  # Actual historical data points
  geom_point(data = plot_train, aes(x = Year, y = Births), color = "black", alpha = 0.5) +
  
  # Historical fitted line
  geom_line(data = plot_train_fit, aes(x = Year, y = Estimate), color = "purple", linetype = "dashed") +
  
  # 95% Uncertainty Band
  geom_ribbon(data = plot_test, aes(x = Year, ymin = Q2.5, ymax = Q97.5), fill = "purple", alpha = 0.2) +
  
  # 80% Uncertainty Band
  geom_ribbon(data = plot_test, aes(x = Year, ymin = Q10, ymax = Q90), fill = "purple", alpha = 0.4) +
  
  # Forecasted mean line
  geom_line(data = plot_test, aes(x = Year, y = Estimate), color = "purple", linewidth = 1.2) +
  
  # Facet wrap splits the graph into separate panels for Age 20, 30, and 40
  facet_wrap(~ Age, labeller = label_both, scales = "free_y") +
  
  ggtitle("Bootstrapped Forecast of Births by Mother's Age") +
  ylab("Number of Births") +
  xlab("Year") +
  theme_minimal()



######################### Deaths #################################################################


# Load necessary libraries
library(brnn)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# 1. Synthesize Realistic Mortality Data 
# ---------------------------------------------------------
set.seed(42)

# Create a grid: Years (2000-2030), Ages (0-90), Gender (Male/Female)
df <- expand.grid(Year = 2000:2030, Age = 0:90, Gender = c("Males", "Females"))

# Synthesize Deaths:
# - High at Age 0, low in middle age, exponential growth at old age
# - Mortality improves (decreases) over time
# - Males typically have slightly higher mortality at most ages
df <- df %>%
  mutate(
    # Create the binary dummy variable for the neural network
    Is_Male = ifelse(Gender == "Males", 1, 0),
    
    # Time improvement (mortality drops by ~1% per year)
    Time_Multiplier = 1 - (Year - 2000) * 0.01,
    
    # Gender disadvantage (Males have ~15% higher base mortality in this simulation)
    Gender_Multiplier = 1 + (Is_Male * 0.15),
    
    # The Bathtub/J-Curve formula (Infant mortality + Senescent exponential)
    Base_Deaths = 10000 * (0.005 * exp(-Age/2) + 0.00005 * exp(Age * 0.07)),
    
    # Apply multipliers
    Expected_Deaths = Base_Deaths * Time_Multiplier * Gender_Multiplier,
    
    # Add random Poisson-like noise and prevent negatives
    Deaths = Expected_Deaths + rnorm(n(), mean = 0, sd = sqrt(Expected_Deaths + 1)),
    Deaths = pmax(Deaths, 0),
    
    # Log-transform to handle the massive exponential tail at old age
    # We add +1 to avoid log(0) errors if Deaths are exactly 0
    Log_Deaths = log(Deaths + 1)
  ) %>%
  select(Year, Age, Gender, Is_Male, Deaths, Log_Deaths)                        ###------------------###



# Split into Training (2000-2023) and Testing/Forecasting (2024-2030)
df_train_0 <- df %>% filter(Year <= 2023)
df_test_0  <- df %>% filter(Year > 2023)

### if use simulated data, then do:      
df_train <- df_train_0 
df_test <- df_test_0

### if real data is available, then use that one

# ---------------------------------------------------------
# 2. Fit the Base brnn Model (3D Feature Space)
# ---------------------------------------------------------
cat("Training the mortality brnn model...\n")

# We predict Log_Deaths using Time, Age, and the Is_Male binary flag.
# We use 5 (minimum 3) neurons to allow it to bend into the bathtub shape.



set.seed(123)
base_model <- brnn(
  Log_Deaths ~ Year + Age + Is_Male, 
  data = df_train,     
  neurons = 5, 
  verbose = FALSE,
  na.action=na.omit
)

# Extract fitted values, residuals, and base forecast (all in Log Space)
base_fitted <- predict(base_model, newdata = df_train)
base_residuals <- df_train$Log_Deaths - base_fitted
base_forecast <- predict(base_model, newdata = df_test)

sum(is.na(base_residuals))
sum(is.na(base_forecast))

# ---------------------------------------------------------
# 3. The Residual Bootstrap Loop
# ---------------------------------------------------------
B <- 100
n_test <- nrow(df_test)
boot_forecasts <- matrix(NA, nrow = n_test, ncol = B)

cat(sprintf("Running %d bootstrap iterations for the mortality surface...\n", B))

for(i in 1:B) {
  # a. Alternate history in log-space
  boot_train <- df_train      
  boot_train$Log_Deaths <- base_fitted + sample(na.omit(base_residuals), replace = TRUE)
  
  # b. Refit the model
  boot_model <- brnn(
    Log_Deaths ~ Year + Age + Is_Male, 
    data = boot_train, 
    neurons = 5, 
    verbose = FALSE
  )
  
  # c. Predict the future
  boot_pred <- predict(boot_model, newdata = df_test)
  
  # d. Add noise to create prediction intervals
  boot_forecasts[, i] <- boot_pred + sample(na.omit(base_residuals), size = n_test, replace = TRUE)
}


### Boot_Deaths <- boot_forecasts   ####
sum(is.na(boot_forecasts))


# ---------------------------------------------------------
# Extracting and Reshaping the Bootstrapped Deaths
# ---------------------------------------------------------
cat("Formatting the Boot_Deaths 4D Array...\n")

# 1. Reverse the Log Transformation
# The base formula was Log_Deaths = log(Deaths + 1). 
# We exponentiate, subtract 1, and use pmax to prevent any negative deaths.
raw_boot_deaths <- pmax(exp(boot_forecasts) - 1, 0)

# 2. Define the Dimensions for the Cohort-Component Engine
Y_len <- length(2024:2030) # 7 Years
A_len <- length(0:90)      # 91 Ages (0 to 90)
G_len <- 2                 # 2 Genders (Male, Female)
B <- ncol(boot_forecasts)  # Number of bootstrap simulations (e.g., 100)

# 3. Reshape into the 4D Array: [Year, Age, Gender, Sim]
# Because df_test was created via expand.grid(Year, Age, Gender),
# the rows perfectly match the filling order of R's array() function.
Boot_Deaths <- array(
  raw_boot_deaths, 
  dim = c(Y_len, A_len, G_len, B),
  dimnames = list(2024:2030, 0:90, c("Males", "Females"), 1:B)
)

cat("Successfully formatted Boot_Deaths [Year, Age, Gender, Sim]!\n")



# ---------------------------------------------------------
# 4. Calculate Quantiles and Exponentiate Output
# ---------------------------------------------------------
quantiles <- apply(boot_forecasts, 1, quantile, probs = c(0.025, 0.10, 0.90, 0.975))
quantiles <- t(quantiles)


# We must EXPONENTIATE the results (and subtract the 1 we added earlier)
df_test_forecast <- df_test %>%
  mutate(
    Estimate = exp(base_forecast) - 1,
    Q2.5     = exp(quantiles[, 1]) - 1,
    Q10      = exp(quantiles[, 2]) - 1,
    Q90      = exp(quantiles[, 3]) - 1,
    Q97.5    = exp(quantiles[, 4]) - 1
  ) %>%
  mutate(across(c(Estimate, Q2.5, Q10, Q90, Q97.5), ~pmax(., 0)))


df_train_fitted <- df_train %>% mutate(Estimate = pmax(exp(base_fitted) - 1, 0))


# ---------------------------------------------------------
# 5. Visualize Selected Demographics (Facet Grid)
# ---------------------------------------------------------
# We will look at Ages 0 (Infants), 40 (Adults), and 80 (Elderly)
target_ages <- c(0, 40, 80)

plot_train <- df_train %>% filter(Age %in% target_ages) %>% na.omit()
plot_train_fit <- df_train_fitted %>% filter(Age %in% target_ages) %>% na.omit()
plot_test <- df_test_forecast %>% filter(Age %in% target_ages) %>% na.omit()

ggplot() +
  geom_point(data = plot_train, aes(x = Year, y = Deaths), color = "black", alpha = 0.4, size = 1) +
  geom_line(data = plot_train_fit, aes(x = Year, y = Estimate), color = "red", linetype = "dashed") +
  
  geom_ribbon(data = plot_test, aes(x = Year, ymin = Q2.5, ymax = Q97.5), fill = "red", alpha = 0.2) +
  geom_ribbon(data = plot_test, aes(x = Year, ymin = Q10, ymax = Q90), fill = "red", alpha = 0.4) +
  geom_line(data = plot_test, aes(x = Year, y = Estimate), color = "red", linewidth = 1) +
  
  # facet_grid creates a matrix of plots: Gender on rows, Age on columns
  facet_grid(Gender ~ Age, scales = "free_y", labeller = label_both) +
  
  ggtitle("Bootstrapped Forecast of Deaths by Age and Gender") +
  ylab("Number of Deaths") +
  xlab("Year") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "lightgray"))



######################## Migration ########################################################



# Load necessary libraries
library(brnn)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# 1. Synthesize Realistic Migration Data (Rogers-Castro Shape)
# ---------------------------------------------------------
set.seed(42)

# Create a 4-dimensional grid: Year, Age, Gender, and Flow type 
df <- expand.grid(
  Year = 2000:2030, 
  Age = 0:90, 
  Gender = c("Male", "Female"),
  Flow = c("Immigrant", "Emigrant")
)

df_0 <- df %>%
  mutate(
    # Create binary dummy variables for the neural network
    Is_Male = ifelse(Gender == "Male", 1, 0),
    Is_Immigrant = ifelse(Flow == "Immigrant", 1, 0),
    
    # Mathematical components of human migration
    Childhood_Curve = 800 * exp(-Age / 5),
    Labor_Peak = 3000 * exp(-0.5 * ((Age - 26) / 6)^2),
    Retirement_Bump = 200 * exp(-0.5 * ((Age - 65) / 5)^2),
    
    Base_Migration = Childhood_Curve + Labor_Peak + Retirement_Bump,
    
    # Multipliers to simulate real-world demographic dynamics
    Time_Mult = 1 + (Year - 2000) * 0.015,         # Migration grows 1.5% per year
    Gender_Mult = 1 + (Is_Male * 0.10),            # Males migrate 10% more in this dataset
    Direction_Mult = ifelse(Is_Immigrant == 1, 1.3, 1.0), # Country has net-positive immigration
    
    Expected_Count = Base_Migration * Time_Mult * Gender_Mult * Direction_Mult,
    
    # Add random noise and prevent negatives
    Count = Expected_Count + rnorm(n(), mean = 0, sd = sqrt(Expected_Count * 5)),
    Count = pmax(Count, 0),
    
    # Log transform to handle the massive labor peak vs the flat older ages
    Log_Count = log(Count + 1)
  ) %>%
  select(Year, Age, Gender, Flow, Is_Male, Is_Immigrant, Count, Log_Count)   ########----------------##########



# Split into Training (2000-2023) and Testing (2024-2030)
df_train_0 <- df_0 %>% filter(Year <= 2023)
df_test_0  <- df_0 %>% filter(Year > 2023)


### if use simulated data, then do:      
df_train <- df_train_0 
df_test <- df_test_0

## if you have real data, then use that in the right structure



# ---------------------------------------------------------
# 2. Fit the Base brnn Model (4D Feature Space)
# ---------------------------------------------------------
cat("Training the 4D migration brnn model...\n")

# We predict Log_Count using Time, Age, Gender, AND Flow Direction.
# Because the Rogers-Castro shape is complex (three different bumps/curves),
# we increase neurons to 5 so the network has enough flexibility to draw it.
set.seed(123)
base_model <- brnn(
  Log_Count ~ Year + Age + Is_Male + Is_Immigrant, 
  data = df_train, 
  neurons = 5, 
  verbose = FALSE,
  na.action=na.omit
)


# Extract fitted values, residuals, and base forecast
base_fitted <- predict(base_model, newdata = df_train)
base_residuals <- na.omit(df_train$Log_Count - base_fitted)        ### but remember that some of the Log_Count are missing!
base_forecast <- predict(base_model, newdata = df_test)


# ---------------------------------------------------------
# 3. The Residual Bootstrap Loop
# ---------------------------------------------------------
B <- 100
n_test <- nrow(df_test)
boot_forecasts <- matrix(NA, nrow = n_test, ncol = B)

cat(sprintf("Running %d bootstrap iterations...\n", B))

for(i in 1:B) {
  # a. Alternate history
  boot_train <- df_train
  boot_train$Log_Count <- base_fitted + sample(na.omit(base_residuals), replace = TRUE)
  
  # b. Refit the model
  boot_model <- brnn(
    Log_Count ~ Year + Age + Is_Male + Is_Immigrant, 
    data = boot_train, 
    neurons = 5, 
    verbose = FALSE
  )
  
  # c. Predict the future and add noise
  boot_pred <- predict(boot_model, newdata = df_test)
  boot_forecasts[, i] <- boot_pred + sample(na.omit(base_residuals), size=n_test, replace = TRUE)
}


### now we calculate Boot_Immigrants and Boot_Emigrants as follows:---------------
# --------------------------------------------------
# Extracting and Reshaping the Component Forecasts
# --------------------------------------------------

# e1. Identify the row indices for each flow in the test dataset
idx_imm <- which(df_test$Flow == "Immigrant")
idx_emi <- which(df_test$Flow == "Emigrant")

# e2. Subset the bootstrapped matrix (these are still in log-space!)
log_boot_immigrants <- boot_forecasts[idx_imm, ]
log_boot_emigrants  <- boot_forecasts[idx_emi, ]

# e3. Exponentiate to return to raw counts of people (and subtract the +1 we added earlier)
raw_boot_immigrants <- pmax(exp(log_boot_immigrants) - 1, 0)
raw_boot_emigrants  <- pmax(exp(log_boot_emigrants) - 1, 0)

# e4. Reshape into 4D Arrays: [Year, Age, Gender, Sim]
# Because of how expand.grid() generated the data, the rows are already perfectly 
# ordered by Year (fastest), then Age, then Gender.
Y_len <- length(2024:2030) # 7 Years
A_len <- length(0:90)      # 91 Ages (Make sure this matches your demographics!)
G_len <- 2                  # 2 Genders

B <- ncol(boot_forecasts)

Boot_Immigrants <- array(
  raw_boot_immigrants, 
  dim = c(Y_len, A_len, G_len, B),
  dimnames = list(2024:2030, 0:90, c("Male", "Female"), 1:B)
)

Boot_Emigrants <- array(
  raw_boot_emigrants, 
  dim = c(Y_len, A_len, G_len, B),
  dimnames = list(2024:2030, 0:90, c("Male", "Female"), 1:B)
)

 cat("Successfully extracted and formatted Boot_Immigrants and Boot_Emigrants 4D Arrays!\n")
###-----------------------


# ---------------------------------------------------------
# 4. Calculate Quantiles and Exponentiate Output
# ---------------------------------------------------------
quantiles <- apply(boot_forecasts, 1, quantile, probs = c(0.025, 0.10, 0.90, 0.975))
quantiles <- t(quantiles)


# Exponentiate and calculate
df_test_forecast <-  
  df_test %>% 
  mutate(
    Estimate = exp(base_forecast) - 1,
    Q2.5     = exp(quantiles[, 1]) - 1,
    Q10      = exp(quantiles[, 2]) - 1,
    Q90      = exp(quantiles[, 3]) - 1,
    Q97.5    = exp(quantiles[, 4]) - 1
  ) %>%
  mutate(across(c(Estimate, Q2.5, Q10, Q90, Q97.5), ~pmax(., 0)))

df_train_fitted <- df_train %>% mutate(Estimate = pmax(exp(base_fitted) - 1, 0))


# ---------------------------------------------------------
# 5. Visualize Overlapping Flows by Age and Gender
# ---------------------------------------------------------
# We select three distinct points on the migration curve:
# Age 10 (Children), Age 25 (Labor Peak), Age 65 (Retirees)
target_ages <- c(10, 25, 30, 35, 65)

### I needed to add the omitting here to avoid plots like zig-zag:
plot_train <- df_train %>% filter(Age %in% target_ages) %>% na.omit()   
plot_train_fit <- df_train_fitted %>% filter(Age %in% target_ages) %>% na.omit()
plot_test <- df_test_forecast %>% filter(Age %in% target_ages) ## %>% na.omit()

ggplot() +
  # Historical data points (color coded by Flow)
  geom_point(data = plot_train, aes(x = Year, y = Count, color = Flow), alpha = 0.3, size = 1) +
  
  # Historical fit
  geom_line(data = plot_train_fit, aes(x = Year, y = Estimate, color = Flow), linetype = "dashed") +
  
  # Forecasted 80% Uncertainty Band (using fill = Flow for overlapping transparent bands)
  geom_ribbon(data = plot_test, aes(x = Year, ymin = Q10, ymax = Q90, fill = Flow), alpha = 0.3) +
  
  # Forecasted Mean Path
  geom_line(data = plot_test, aes(x = Year, y = Estimate, color = Flow), linewidth = 1) +
  
  # Facet matrix: Gender on rows, Age on columns
  facet_grid(Gender ~ Age, scales = "free_y", labeller = label_both) +
  
  # Use distinct colors for Immigrants vs Emigrants
  scale_color_manual(values = c("Immigrant" = "blue", "Emigrant" = "darkorange")) +
  scale_fill_manual(values = c("Immigrant" = "blue", "Emigrant" = "darkorange")) +
  
  ggtitle("Stochastic Forecast: Immigrants vs. Emigrants") +
  ylab("Number of Migrants") +
  xlab("Year") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    legend.position = "bottom"
  )



######################## Stochastic Projections #####################################


# ---------------------------------------------------------
# 1. Generate the Baseline Population (Year 2023)
# ---------------------------------------------------------
cat("Generating 2023 Baseline Population...\n")

# Matrix: 91 Ages (0-90) x 2 Genders
Baseline_Pop <- matrix(0, nrow = 91, ncol = 2)
colnames(Baseline_Pop) <- c("Male", "Female")
rownames(Baseline_Pop) <- 0:90

for(age in 0:90) {
  # Create a realistic shape: Bulge in the middle, exponential drop at the end
  if (age < 25) {
    base_count <- 120000 + rnorm(1, 0, 2000)
  } else if (age < 55) {
    base_count <- 140000 - (age - 25) * 500 + rnorm(1, 0, 2000)
  } else {
    base_count <- 125000 * exp(-0.08 * (age - 55)) + rnorm(1, 0, 500)
  }
  base_count <- max(base_count, 0)
  
  # Males are ~51.2% at birth, but higher mortality means Females outnumber them by old age
  male_ratio <- 0.512 - (age / 90) * 0.06 
  
  Baseline_Pop[age + 1, 1] <- base_count * male_ratio       # Male
  Baseline_Pop[age + 1, 2] <- base_count * (1 - male_ratio) # Female
}


head(Baseline_Pop)

### use your own Baseline_Pop, i.e. real data of the population at the starting year of projections


B_original <- 100   # The number of bootstrap paths we generated earlier
S_sims <- 1000      # The number of Monte Carlo population simulations we want to run
Y <- 7              # Forecast horizon (2024 to 2030)
A <- 91             # Ages (0 to 90)
G <- 2              # Genders (Male, Female)


### we have Boot_Births, Boot_Deaths, Boot_Im/E-Migrants: calculated above

cat(sprintf("Running %d Monte Carlo Demographic Simulations...\n", S_sims))

# Master Array to hold the Population Projections
Pop_Proj <- array(0, dim = c(Y, A, G, S_sims), 
                  dimnames = list(2024:2030, 0:90, c("Male", "Female"), 1:S_sims))

# ---------------------------------------------------------
# 2. The Monte Carlo Cohort-Component Engine
# ---------------------------------------------------------
for(sim in 1:S_sims) {
  
  # --- CRITICAL STEP: INDEPENDENT TRAJECTORY SAMPLING ---
  # We randomly pick an index (1 to 100) for each demographic component.
  # We pick ONE index per component for the whole trajectory to maintain 
  # the temporal autocorrelation (the momentum) that the neural network learned!
  
  idx_b <- sample(1:B_original, 1)  # Which Births reality are we in?
  idx_d <- sample(1:B_original, 1)  # Which Deaths reality are we in?
  idx_i <- sample(1:B_original, 1)  # Which Immigration reality?
  idx_e <- sample(1:B_original, 1)  # Which Emigration reality?
  
  Current_Pop <- Baseline_Pop 
  
  for(y_idx in 1:Y) {
    Next_Pop <- matrix(0, nrow = A, ncol = G) 
    
    # A. AGING AND SURVIVAL (Ages 1 to 90)
    for(age in 0:89) {
      for(gender in 1:G) {
        
        # Pull the specific values from our chosen bootstrapped realities
        d <- Boot_Deaths[y_idx, age + 1, gender, idx_d]
        i <- Boot_Immigrants[y_idx, age + 1, gender, idx_i]
        e <- Boot_Emigrants[y_idx, age + 1, gender, idx_e]
        
        survivors <- Current_Pop[age + 1, gender] - d + i - e
        Next_Pop[age + 2, gender] <- max(survivors, 0)
      }
    }
    
    # B. NEWBORNS (Age 0)
    total_births <- Boot_Births[y_idx, idx_b]
    Next_Pop[1, 1] <- total_births * 0.5122  # Male
    Next_Pop[1, 2] <- total_births * 0.4878  # Female
    
    # C. SAVE AND ADVANCE
    Pop_Proj[y_idx, , , sim] <- Next_Pop
    Current_Pop <- Next_Pop 
  }
}


# ---------------------------------------------------------
# 3. Extract and Plot the Projections
# ---------------------------------------------------------
# Sum across ages and genders for each simulation
Total_Pop_Sims <- apply(Pop_Proj, c(1, 4), sum)


# Calculate Median and 95% Bounds across the 1000 new simulations
Pop_Forecast <- data.frame(
  Year = 2024:2030,
  Median = apply(Total_Pop_Sims, 1, median),
  Lower_95 = apply(Total_Pop_Sims, 1, quantile, probs = 0.025),
  Upper_95 = apply(Total_Pop_Sims, 1, quantile, probs = 0.975)
)

print(Pop_Forecast)


#########################################################
###----------- visualisations-------------------------###
########################################################



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(scales) # For formatting the Y-axis with commas

# ---------------------------------------------------------
# 1. Calculate Multiple Quantiles for the Fan
# ---------------------------------------------------------
cat("Calculating multiple probability intervals for the Fan Chart...\n")

Pop_Fan <- data.frame(
  Year = 2024:2030,
  Median = apply(Total_Pop_Sims, 1, median),
  
  # 95% Interval limits
  Q02.5  = apply(Total_Pop_Sims, 1, quantile, probs = 0.025),
  Q97.5  = apply(Total_Pop_Sims, 1, quantile, probs = 0.975),
  
  # 80% Interval limits
  Q10    = apply(Total_Pop_Sims, 1, quantile, probs = 0.10),
  Q90    = apply(Total_Pop_Sims, 1, quantile, probs = 0.90),
  
  # 50% Interval limits (The interquartile range)
  Q25    = apply(Total_Pop_Sims, 1, quantile, probs = 0.25),
  Q75    = apply(Total_Pop_Sims, 1, quantile, probs = 0.75)
)

# ---------------------------------------------------------
# 2. Attach the Baseline Year (to anchor the fan to a point)
# ---------------------------------------------------------
# We assume 'Baseline_Pop' is in the environment from the earlier script
total_baseline <- sum(Baseline_Pop)

baseline_row <- data.frame(
  Year = 2023, 
  Median = total_baseline, 
  Q02.5 = total_baseline, Q97.5 = total_baseline,
  Q10 = total_baseline, Q90 = total_baseline,
  Q25 = total_baseline, Q75 = total_baseline
)

Pop_Fan <- bind_rows(baseline_row, Pop_Fan)

# ---------------------------------------------------------
# 3. Visualize the Uncertainty Fan with ggplot2
# ---------------------------------------------------------


ggplot(Pop_Fan, aes(x = Year)) +
  
  # LAYER 1: 95% Prediction Interval (Lightest, widest band)
  geom_ribbon(aes(ymin = Q02.5, ymax = Q97.5, fill = "95% Interval"), alpha = 0.3) +
  
  # LAYER 2: 80% Prediction Interval (Medium band)
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = "80% Interval"), alpha = 0.5) +
  
  
  # LAYER 3: 50% Prediction Interval (Darkest, narrowest band)
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = "50% Interval"), alpha = 0.7) +
  
  # LAYER 4: The Median Forecast Line
  geom_line(aes(y = Median, color = "Median Projection"), linewidth = 1.2) +
  geom_point(aes(y = Median, color = "Median Projection"), size = 2.5) +
  
  # Customizing Colors and Legend
  scale_fill_manual(
    name = "Prediction Intervals",
    values = c(
      "95% Interval" = "#b3cde0", 
      "80% Interval" = "#6497b1", 
      "50% Interval" = "#005b96"
    )
  ) +
  scale_color_manual(
    name = "Forecast",
    values = c("Median Projection" = "#03396c")
  ) +
  
  # Formatting the Y-axis to prevent scientific notation (e.g., 1e+07)
  scale_y_continuous(labels = comma) +
  
  # Titles and Theming
  ggtitle("Stochastic Population Forecast", 
          subtitle = "Bootstrapped Cohort-Component Method with Monte Carlo Sampling") +
  ylab("Total Population") +
  xlab("Year") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank() # Cleans up the background
  )

# ---------------------------------------------------------


### pyramids ########--------------------------------------
# Load necessary libraries
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# 1. Extract 2030 Data and Calculate Quantiles
# ---------------------------------------------------------
cat("Calculating age-specific uncertainty bands for 2030...\n")

# Pop_Proj dimensions are [Year, Age, Gender, Simulation]
# Year 7 corresponds to 2030 in our setup
pop_2030_sims <- Pop_Proj[7, , , ] # This yields a [91 Ages, 2 Genders, 1000 Sims] array

# Separate by gender for easier quantile calculation
male_sims <- pop_2030_sims[, 1, ]
female_sims <- pop_2030_sims[, 2, ]

# Build the dataframe
df_2030 <- data.frame(
  Age = rep(0:90, times = 2),
  Gender = rep(c("Male", "Female"), each = 91)
)

# Calculate Median, 2.5%, and 97.5% quantiles
df_2030$Median <- c(apply(male_sims, 1, median), apply(female_sims, 1, median))
df_2030$Lower  <- c(apply(male_sims, 1, quantile, probs = 0.025), apply(female_sims, 1, quantile, probs = 0.025))
df_2030$Upper  <- c(apply(male_sims, 1, quantile, probs = 0.975), apply(female_sims, 1, quantile, probs = 0.975))

# Calculate Median, 10%, and 90% quantiles
# df_2030$Median <- c(apply(male_sims, 1, median), apply(female_sims, 1, median))
# df_2030$Lower  <- c(apply(male_sims, 1, quantile, probs = 0.10), apply(female_sims, 1, quantile, probs = 0.10))
# df_2030$Upper  <- c(apply(male_sims, 1, quantile, probs = 0.90), apply(female_sims, 1, quantile, probs = 0.90))


# ---------------------------------------------------------
# 2. Apply the "Male = Negative" Plotting Trick
# ---------------------------------------------------------
df_plot <- df_2030 %>%
  mutate(
    # The median is straightforward
    Plot_Median = ifelse(Gender == "Male", -Median, Median),
    
    # CRITICAL MATH: Because Male values are plotted backwards (into the negatives),
    # the "widest" absolute population (Upper) becomes the "most negative" plot value.
    Plot_Lower = ifelse(Gender == "Male", -Upper, Lower),
    Plot_Upper = ifelse(Gender == "Male", -Lower, Upper)
  )

# ---------------------------------------------------------
# 3. Visualize the Pyramid with Error Bands
# ---------------------------------------------------------
ggplot(df_plot, aes(x = Age, fill = Gender, color = Gender)) +
  
  # 1. Draw the Median as a semi-transparent bar
  geom_col(aes(y = Plot_Median), width = 0.8, alpha = 0.6, color = NA) +
  
  # 2. Draw the 95% Uncertainty Bands as solid lines through the bars
  geom_linerange(aes(ymin = Plot_Lower, ymax = Plot_Upper), linewidth = 0.8) +
  
  # Flip the axes to make it a pyramid
  coord_flip() +
  
  # Format the Y-axis to hide negative signs and use commas
  scale_y_continuous(labels = function(x) scales::comma(abs(x))) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  
  # Color theming (bars get 'fill', error lines get 'color')
  scale_fill_manual(values = c("Male" = "#3b5998", "Female" = "#8b9dc3")) +
  scale_color_manual(values = c("Male" = "#1c2b4a", "Female" = "#3b4a6b")) + # Darker lines
  
  ggtitle("Stochastic Population Pyramid: 2030", 
          subtitle = "Bars represent median projection. Lines represent 95% prediction intervals.") +
  ylab("Population") +
  xlab("Age") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(color = "darkgray", face = "italic")
  )


### could add total deaths, births, e/im-migration, with uncertainty fans ...


##############################################################################################


