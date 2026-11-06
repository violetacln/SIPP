
### acknowledgement: this code has been developed with the assistance of Gemini 3.1.Pro-2026 ----------

################ Projections by type of family #######################

### this assumes the data-frame of population past and projections is in the R-environment 
### (for instance created with the https://github.com/violetacln/SIPP/blob/main/R_neuralNetworks_models/sipp_pub.R )
### and it is included as Part II of that script for the time being.

# ---------------------------------------------------------
# Fully Stochastic Dynamic Family Projections (Bootstrapped Dirichlet + TFR)
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

