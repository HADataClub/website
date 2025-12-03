## Info ####
## who: Ed H
## what: HADC!
## when: 2025-12-02

## CONTENTS ####
## 01 Set up
## 02 Code

## 01 Set up ####   

## 02 Code ####

###############################################################################
# Car Price Prediction Project (R / RStudio script)
# -------------------------------------------------
# End-to-end example using the CarPrice_Assignment.csv dataset:
# - Load and inspect data
# - Clean and prepare variables
# - Fit and interpret a multiple linear regression model (price ~ predictors)
# - Visualize price vs engine size with a regression line
###############################################################################

############################################
# 1. Setup: packages, working directory
############################################

# Install packages if needed (uncomment if you don't have them)
# install.packages("tidyverse")
# install.packages("GGally")
# install.packages("broom")
# install.packages("car")

library(tidyverse)   # dplyr, ggplot2, readr, etc.
library(GGally)      # optional: pair plots
library(broom)       # tidy model outputs
library(car)         # VIF and diagnostic helpers

# Set your working directory to where CarPrice_Assignment.csv is located.
# setwd("path/to/your/folder")

############################################
# 2. Load and inspect the data
############################################

# Read the CSV file
car_price_df <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

# Quick structure and first few rows
str(car_price_df)
head(car_price_df)

# Basic summary stats
summary(car_price_df$price)

############################################
# 3. Data cleaning and preparation
############################################

# Remove ID and raw text columns that aren't useful as-is for regression
# (We could engineer features from CarName, but for this project we'll drop it.)
car_price_lm_df <- car_price_df %>%
  select(
    -car_ID,    # pure identifier
    -CarName    # raw text name of the car
  )

# Convert selected columns to factors (categorical variables)
categorical_vars <- c(
  "fueltype",
  "aspiration",
  "doornumber",
  "carbody",
  "drivewheel",
  "enginelocation",
  "enginetype",
  "cylindernumber",
  "fuelsystem"
)

car_price_lm_df <- car_price_lm_df %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Check structure after conversion
str(car_price_lm_df)

############################################
# 4. Exploratory plot: price vs engine size ####
############################################

# Scatterplot with linear regression line using ggplot2
ggplot(car_price_lm_df, aes(x = enginesize, y = price)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Price vs Engine Size",
    x = "Engine Size",
    y = "Price"
  ) +
  theme_minimal()

# Optional: quick correlation-like check among numeric variables
numeric_vars <- car_price_lm_df %>%
  select(where(is.numeric))

if (ncol(numeric_vars) > 1) {
  ggpairs(numeric_vars[, 1:min(6, ncol(numeric_vars))])  # subset to avoid huge plot
}

############################################
# 5. Build the full linear regression model
############################################

# Create a formula: price ~ all other variables
predictor_names <- setdiff(names(car_price_lm_df), "price")
full_formula <- as.formula(
  paste("price ~", paste(predictor_names, collapse = " + "))
)

# Fit the linear model
lm_full <- lm(full_formula, data = car_price_lm_df)

# Model summary: coefficients, significance, R-squared, etc.
summary(lm_full)

############################################
# 6. Multicollinearity check (VIF)
############################################

# Calculate variance inflation factors; high VIF (> 5 or 10) suggests multicollinearity
vif_values <- vif(lm_full)
vif_values

############################################
# 7. Model diagnostics
############################################

# Basic diagnostic plots: residuals, QQ plot, leverage, etc.
# These will open in the Plots pane in RStudio.
par(mfrow = c(2, 2))
plot(lm_full)
par(mfrow = c(1, 1))

# Extract residuals and fitted values
model_diag <- augment(lm_full)  # from broom

# Residuals vs fitted plot using ggplot
ggplot(model_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Price",
    y = "Residuals"
  ) +
  theme_minimal()

############################################
# 8. Optional: Simplified model via stepwise selection
############################################

# Stepwise (both directions) starting from the full model
# This can help reduce the model to a more parsimonious set of predictors.
lm_step <- step(lm_full, direction = "both", trace = FALSE)

summary(lm_step)

# Compare full vs stepwise model by AIC and R-squared
AIC(lm_full, lm_step)
summary(lm_full)$r.squared
summary(lm_step)$r.squared

############################################
# 9. Interpretation helpers (sorted coefficients, etc.)
############################################

# Tidy coefficients with p-values for the stepwise model
tidy_step <- tidy(lm_step) %>%
  arrange(p.value)  # smallest p-value first

head(tidy_step, 20)  # show the 20 most significant terms

# Glance at overall fit metrics
glance(lm_step)

############################################
# 10. Using the model for prediction
############################################

# Create a small example "new car" for prediction
new_car <- tibble(
  symboling       = 0,
  fueltype        = factor("gas", levels = levels(car_price_lm_df$fueltype)),
  aspiration      = factor("std", levels = levels(car_price_lm_df$aspiration)),
  doornumber      = factor("four", levels = levels(car_price_lm_df$doornumber)),
  carbody         = factor("sedan", levels = levels(car_price_lm_df$carbody)),
  drivewheel      = factor("fwd", levels = levels(car_price_lm_df$drivewheel)),
  enginelocation  = factor("front", levels = levels(car_price_lm_df$enginelocation)),
  wheelbase       = 100,
  carlength       = 180,
  carwidth        = 66,
  carheight       = 54,
  curbweight      = 2500,
  enginetype      = factor("ohc", levels = levels(car_price_lm_df$enginetype)),
  cylindernumber  = factor("four", levels = levels(car_price_lm_df$cylindernumber)),
  enginesize      = 130,
  fuelsystem      = factor("mpfi", levels = levels(car_price_lm_df$fuelsystem)),
  # plus any other predictors in the final model, if needed:
  boreratio       = mean(car_price_lm_df$boreratio, na.rm = TRUE),
  stroke          = mean(car_price_lm_df$stroke, na.rm = TRUE),
  compressionratio= mean(car_price_lm_df$compressionratio, na.rm = TRUE),
  horsepower      = 90,
  peakrpm         = 5200,
  citympg         = 25,
  highwaympg      = 30
)

# Predict price for the new car with confidence intervals
predicted_price <- predict(lm_step, newdata = new_car, interval = "prediction")
predicted_price

############################################
# 11. Save key outputs (optional)
############################################

# Save model object to an .RDS file for later use
saveRDS(lm_step, file = "car_price_lm_step_model.rds")

# Save a CSV of tidy coefficients for documentation
write.csv(tidy_step, "car_price_lm_step_coefficients.csv", row.names = FALSE)

###############################################################################
# End of script
###############################################################################