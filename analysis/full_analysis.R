################################################################################
# Project: Belief-Practice Relation Analysis
# Author:  Chi Zhang
# Date:    2025-12-12 (Updated)
# Purpose: Bayesian Multilevel Modeling (brms) on Teacher Beliefs and Practices
################################################################################

# ------ 1. Packages ------
library(tidyverse)   
library(readxl)      
library(here)        
library(brms)       
library(rstan)       
library(mice)        
library(bayestestR)  
library(bayesplot) 
library(patchwork)  
library(viridis)     
library(ggridges)    
library(GGally)      
library(extrafont)   

# create a "models" folder in the current working directory to store fitted
# model objects for easier re-usage
if (!dir.exists("models")) {
  dir.create("models")
}
# Visualization Settings
theme_set(bayesplot::theme_default())

# ------2. Data loading ------

MLM_data <- read_excel("data/MLM_data.xlsx") 
# including 8 plausible sets of IRT person abilities
school_data <- read_excel("data/school_data_processing.xlsx")

# ------ 3. Data preprocessing ------

# Teacher data cleaning
cleaned_data <- MLM_data %>%
  rename(Gender = gender) %>% 
  mutate(
    Sch_ID = if_else(Sch_ID %in% c(0, 100, 199, 200, 299, 300, 399), 0, Sch_ID),
    across(c(District, Gender, YearG, Attain_Lvl), ~ na_if(., 0)),
    across(c(District, Sch_ID, YearG, Attain_Lvl, Gender), as.factor)
  )

# Create Plausible Value Subsets

pv_data_list <- lapply(1:8, function(i) {
  
  cleaned_data %>%
    select(
      District, Sch_ID, Gender, YearG, Attain_Lvl,
      # Select dynamic columns
      theta_P  = !!paste0("theta_P_", i),
      thetaB_G = !!paste0("thetaB_G_", i),
      thetaB_A = !!paste0("thetaB_A_", i),
      thetaB_B = !!paste0("thetaB_B_", i),
      thetaB_C = !!paste0("thetaB_C_", i)
    )
})

# School data processing

processed_school_data <- school_data %>%
  mutate(
    across(c(school_building_area, teacher, financial_expenditure), 
           ~ . / student_enrollment, .names = "{.col}_ratio"),
    
    # Standardization (Z-scores)
    z_area    = as.numeric(scale(school_building_area_ratio)),
    z_teacher = as.numeric(scale(teacher_ratio)),
    z_finance = as.numeric(scale(financial_expenditure_ratio)),
    
    # Weighted Index
    z_resource = 0.2 * z_area + 0.5 * z_teacher + 0.3 * z_finance,
    
    # Housing Price (Log & Standardized)
    log_hp  = log(housing_price),
    zlog_hp = as.numeric(scale(log_hp)),
    Sch_ID = as.factor(Sch_ID)
  ) %>%
  select(Sch_ID, z_resource, log_hp, zlog_hp)

# ------4. Multiple Imputation (MICE) ------

# test run for default methods and matrix
imp_template <- mice(pv_data_list[[1]], maxit = 0, print = FALSE)

# Extract and Modify Methods
meth <- imp_template$method
meth["Gender"] <- "logreg"
meth[c("District", "YearG", "Attain_Lvl")] <- "polyreg"
meth[c("thetaB_G", "thetaB_A", "thetaB_B", "thetaB_C")] <- "norm"
meth[c("Sch_ID", "theta_P")] <- "" 

# Define Predictor Matrix (Who predicts whom)
pred <- imp_template$predictorMatrix
pred[,] <- 1            # All variables predict each other initially
diag(pred) <- 0         # Remove self-prediction
pred["Sch_ID", ] <- 0   # Sch_ID is not imputed
pred[, "Sch_ID"] <- 0   # Sch_ID predicts nothing
pred["theta_P", ] <- 0  # theta_P is not imputed 

# >> Execution
# Generate 40 imputed datasets (8 PV sets * 5 imputations)
imputed_full_list <- lapply(seq_along(pv_data_list), function(i) {
  
  imp_run <- mice(
    pv_data_list[[i]],
    m = 5, 
    maxit = 20, 
    method = meth, 
    predictorMatrix = pred, 
    seed = 123 + i
  )
  
  mice::complete(imp_run, "all")
  
}) %>% 
  unlist(recursive = FALSE)

# ------ 5. School Data Processing & Merging ------ 

processed_school_data <- school_data %>%
  mutate(
    across(c(school_building_area, teacher, financial_expenditure), 
           ~ . / student_enrollment, 
           .names = "{.col}_ratio"),
    
    z_resource = 0.2 * as.numeric(scale(school_building_area_ratio)) +
      0.5 * as.numeric(scale(teacher_ratio)) +
      0.3 * as.numeric(scale(financial_expenditure_ratio)),
    
    zlog_hp = as.numeric(scale(log(housing_price))),
    
    Sch_ID = as.factor(Sch_ID)
  ) %>%
  select(Sch_ID, z_resource, zlog_hp)

# ------ 6. Final dataset ------ 
final_dataset_list <- lapply(imputed_full_list, function(df) {
  
  df %>%
    left_join(processed_school_data, by = "Sch_ID") %>%
    mutate(
      across(starts_with("theta"), ~ as.numeric(scale(.)), .names = "z{.col}"),
      # Define Factor Types for Bayesian Modeling
      YearG = factor(YearG, ordered = FALSE),           
      Attain_Lvl = factor(Attain_Lvl, ordered = TRUE)
    )
})

# ------ 7. Model Specification & Fitting ------ 

# Weakly informative priors
global_priors <- c(
  prior(normal(0, 3), class = "Intercept"),
  prior(normal(0, 3), class = "b")
)

# Sampling controls
stan_control <- list(adapt_delta = 0.99, max_treedepth = 15)

# Model 1: Basic
formula_basic <- bf(ztheta_P ~ zthetaB_G + District + Gender + YearG + 
                      zlog_hp + z_resource + mo(Attain_Lvl) + (1|Sch_ID))

# Model 2: 4-Dimensions
formula_4dim <- bf(ztheta_P ~ zthetaB_G + zthetaB_A + zthetaB_B + zthetaB_C + 
                     District + Gender + YearG + zlog_hp + z_resource + 
                     mo(Attain_Lvl) + (1|Sch_ID))

# Model 3: Final Interaction 1 (Housing * Year)
formula_final <- bf(ztheta_P ~ zthetaB_G + zthetaB_A + zthetaB_B + zthetaB_C + 
                      District + Gender + z_resource + mo(Attain_Lvl) + 
                      zlog_hp * YearG + (1|Sch_ID))

# Model 4: Final Interaction 2 (Belief * Attainment)
formula_final2 <- bf(ztheta_P ~ zthetaB_G * mo(Attain_Lvl) + 
                       zthetaB_A + zthetaB_B + zthetaB_C + 
                       District + Gender + z_resource + 
                       zlog_hp * YearG + (1|Sch_ID))

# 1. Basic Model (Test)
test_basic_mlm <- brm(
  formula = formula_basic,
  data = final_dataset_list[[1]], # Single dataset
  prior = global_priors,
  chains = 4, cores = 4, iter = 1000, warmup = 500,
  control = stan_control,
  file = here("models", "test_basic_mlm") # Match existing RDS
)

# 2. 4-Dim Model (Test)
test_4dim_mlm <- brm(
  formula = formula_4dim,
  data = final_dataset_list[[1]],
  prior = global_priors,
  chains = 4, cores = 4, iter = 1000, warmup = 500,
  control = stan_control,
  file = here("models", "test_4dim_mlm")
)

# 3. Final Model 1 (Test)
test_final_mlm <- brm(
  formula = formula_final,
  data = final_dataset_list[[1]],
  prior = global_priors,
  chains = 4, cores = 4, iter = 1000, warmup = 500,
  control = stan_control,
  file = here("models", "test_final_mlm")
)

# 4. Final Model 2 (Test)
test_final2_mlm <- brm(
  formula = formula_final2,
  data = final_dataset_list[[1]],
  prior = global_priors,
  chains = 4, cores = 4, iter = 1000, warmup = 500,
  control = stan_control,
  file = here("models", "test_final2_mlm")
)

# 1. Basic Model (Multiple)
basic_mlm_multiple <- brm_multiple(
  formula = formula_basic,
  data = final_dataset_list, # List of 40 datasets
  prior = global_priors,
  chains = 4, cores = 4, iter = 1000, warmup = 500,
  control = stan_control,
  file = here("models", "basic_mlm_multiple")
)

# 2. 4-Dim Model (Multiple)
dim4_mlm_multiple <- brm_multiple(
  formula = formula_4dim,
  data = final_dataset_list,
  prior = global_priors,
  chains = 4, cores = 4, iter = 1000, warmup = 500,
  control = stan_control,
  file = here("models", "dim4_mlm_multiple")
)

# 3. Final Model 1 (Multiple)
final_mlm_multiple <- brm_multiple(
  formula = formula_final,
  data = final_dataset_list,
  prior = global_priors,
  chains = 4, cores = 4, iter = 1000, warmup = 500,
  control = stan_control,
  file = here("models", "final_mlm_multiple")
)

# 4. Final Model 2 (Multiple)
final2_mlm_multiple <- brm_multiple(
  formula = formula_final2,
  data = final_dataset_list,
  prior = global_priors,
  chains = 4, cores = 4, iter = 1000, warmup = 500,
  control = stan_control,
  file = here("models", "final2_mlm_multiple")
)

# Calculating yeargroup categorical effect

# 1. Final 1 Model (Test - Single)
hypothesis(test_final_mlm, c("zlog_hp + zlog_hp:YearG2 = 0", 
                            "zlog_hp + zlog_hp:YearG3 = 0"))

# 2. Final 1 Model (Joint - Multiple)
hypothesis(final_mlm_multiple, c("zlog_hp + zlog_hp:YearG2 = 0", 
                                "zlog_hp + zlog_hp:YearG3 = 0"))

# 3. Final 2 Model (Test - Single)
hypothesis(test_final2_mlm, c("zlog_hp + YearG2:zlog_hp = 0", 
                              "zlog_hp + YearG3:zlog_hp = 0"))

# 4. Final 2 Model (Joint - Multiple)
hypothesis(final2_mlm_multiple, c("zlog_hp + YearG2:zlog_hp = 0", 
                                  "zlog_hp + YearG3:zlog_hp = 0"))


# ------ 8. Visualisation Figure 1 ------ 

ce_m3 <- conditional_effects(final_mlm_multiple, "zthetaB_G")
ce_m4 <- conditional_effects(final2_mlm_multiple, "zthetaB_G:Attain_Lvl")

raw_y3 <- range(c(ce_m3$zthetaB_G$estimate__, ce_m3$zthetaB_G$lower__, ce_m3$zthetaB_G$upper__), na.rm = TRUE)
buf3   <- diff(raw_y3) * 0.15
lims3  <- c(raw_y3[1] - buf3, raw_y3[2] + buf3)

raw_y4 <- range(c(ce_m4$`zthetaB_G:Attain_Lvl`$estimate__, ce_m4$`zthetaB_G:Attain_Lvl`$lower__, ce_m4$`zthetaB_G:Attain_Lvl`$upper__), na.rm = TRUE)
buf4   <- diff(raw_y4) * 0.15
lims4  <- c(raw_y4[1] - buf4, raw_y4[2] + buf4)

base_style <- list(
  theme_bw(),
  theme(
    text = element_text(family = "serif", size = 10),
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.grid = element_blank(),
    plot.margin = unit(c(10, 10, 10, 10), "pt")
  ),
  scale_x_continuous(expand = expansion(mult = 0.05)),
  scale_y_continuous(expand = expansion(mult = 0.05))
)

p1 <- plot(ce_m3, plot = FALSE)[[1]] +
  labs(title = "General belief (Model 3)", y = "Practice", x = "General belief") +
  coord_cartesian(xlim = c(-2.2, 2.2), ylim = lims3) + base_style

p2 <- plot(ce_m4, plot = FALSE)[[1]] +
  labs(title = "General belief by perceived attainment (Model 4)",
       y = "Practice", x = "General belief", color = "PA", fill = "PA") +
  coord_cartesian(xlim = c(-2.2, 2.2), ylim = lims4) + base_style +
  theme(legend.position = "right")

final_fig1 <- p1 | p2
print(final_fig1)

ggsave(
  filename = here("figures", "Fig1.png"),
  plot = final_fig1, width = 9, height = 4, units = "in"
)

# ------ 8. Visualisation Figure 2 ------ 

ce_m2 <- conditional_effects(dim4_mlm_multiple, "zlog_hp")
ce_m3 <- conditional_effects(final_mlm_multiple, "zlog_hp:YearG")

raw_y_range <- range(
  c(ce_m2$zlog_hp$estimate__, ce_m2$zlog_hp$lower__, ce_m2$zlog_hp$upper__,
    ce_m3$`zlog_hp:YearG`$estimate__, ce_m3$`zlog_hp:YearG`$lower__, ce_m3$`zlog_hp:YearG`$upper__),
  na.rm = TRUE
)
y_buffer <- diff(raw_y_range) * 0.15
y_limits <- c(raw_y_range[1] - y_buffer, raw_y_range[2] + y_buffer)

year_map <- c("1" = "7", "2" = "8", "3" = "9")

base_style <- list(
  coord_cartesian(ylim = y_limits),
  theme_bw(),
  theme(
    text = element_text(family = "serif", size = 10),
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.grid = element_blank()
  )
)

p1 <- plot(ce_m2, plot = FALSE)[[1]] +
  labs(title = "Housing price (Model 2)", y = "Practice", x = "Housing price") + base_style

p2 <- plot(ce_m3, plot = FALSE)[[1]] +
  labs(title = "Housing price by year group (Model 3)", y = NULL, x = "Housing price", color = "Year", fill = "Year") +
  scale_color_discrete(labels = year_map) +
  scale_fill_discrete(labels = year_map) +
  base_style +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "right")

final_fig2 <- p1 | p2
print(final_fig2)

ggsave(
  filename = here("figures", "Fig2.png"),
  plot = final_fig2, width = 9, height = 4, units = "in"
)

# ------ 9. Model compare ------

# Calculate LOO
loo_basic   <- loo(test_basic_mlm)
loo_4dim    <- loo(test_4dim_mlm)
loo_final   <- loo(test_final_mlm)
loo_final2  <- loo(test_final2_mlm)

# Compare
loo_compare(loo_final, loo_final2, loo_basic, loo_4dim)

# ------ 10. Prior sensitive analysis (for model 3) ------

# Define alternative priors
priors_restrictive <- c(prior(normal(0, 1), class = "Intercept"),
                        prior(normal(0, 1), class = "b"))
priors_wide        <- c(prior(normal(0, 5), class = "Intercept"),
                        prior(normal(0, 5), class = "b"))

# Load models with different priors
test_sens_N01 <- brm(
  formula = formula_final, data = final_dataset_list[[1]],
  prior = priors_restrictive,
  chains = 4, cores = 4, iter = 1000, warmup = 500, control = stan_control,
  file = here("models", "test_final_mlm_sens1_N01")
)

test_sens_N05 <- brm(
  formula = formula_final, data = final_dataset_list[[1]],
  prior = priors_wide,
  chains = 4, cores = 4, iter = 1000, warmup = 500, control = stan_control,
  file = here("models", "test_final_mlm_sens2_N05")
)

# Joint Models
joint_sens_N01 <- brm_multiple(
  formula = formula_final, data = final_dataset_list,
  prior = priors_restrictive,
  chains = 4, cores = 4, iter = 1000, warmup = 500, control = stan_control,
  file = here("models", "joint_final_mlm_sens1_N01")
)

joint_sens_N05 <- brm_multiple(
  formula = formula_final, data = final_dataset_list,
  prior = priors_wide,
  chains = 4, cores = 4, iter = 1000, warmup = 500, control = stan_control,
  file = here("models", "joint_final_mlm_sens2_N05")
)


################################################################################
##                                    end
################################################################################
