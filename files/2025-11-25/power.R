## Info ####
## what: HADC Sophie power white stork
## when: 2025-11-25

## Contents ####
## 1 setup
## 2 Estimates
## 3 Power
## 4 LME models and effect size


## 1 setup ####
install.packages("pwr")
library(pwr)

## 2 Estimates ####
# From Bondi pp 224
# Exploration time in Caracara
# measured diff between M and F
# We assume Fig1 Ad vs Juv is same as stork M versus F
# n = 9F, 10M
# M = 134 +- 44 (mean +- SE)
# F = 108 +- 22

# SD = SE*sqrt(n)

mF <- 108
mM <- 134
seF <- 22
seM <- 44
nF <- 9
nM <- 10

(sdF <- seF * sqrt(nF))
(sdM <- seM * sqrt(nM))
(sdPooled <- (sdF * nF/19) +  (sdM * nM/19))

(differenceMF <- abs(mM - mF))

## 3 Power ####

(cohen_d <- differenceMF/sdPooled)


# what was the power in the Bondi paper for the time var
pwr.t.test(n = 19/2, d = cohen_d, sig.level = 0.05, power = NULL)

pwr.t.test(n = NULL, d = cohen_d, sig.level = 0.05, power = 0.8)


## 4 LME models and effect size ####

# library(lme4)
# fit <- lmer(RT ~ prime + (prime|item) + (prime|participant), data = adelman)
# summary(fit)

# difference between means
mydiff <- 16.029

var_int_part <- 10032.3
var_int_item <- 1900.12
var_slope_part <- 27.89
var_slope_item <- 19.88

var_resid <- 22128.15

# d <- difference / sqrt((all random variance components added up + residual variance))
(d <- mydiff / sqrt((var_int_part + var_int_item + var_slope_part + var_slope_item + var_resid)))




