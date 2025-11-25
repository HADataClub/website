## Info ####
## what: Sophie power white stork
## when: 2025=11-21

## Contents ####
## 1 setup
## 2 Estimates
## 3 Power

## 1 setup ####
install.packages("pwr")
library(pwr)

## 2 Estimates ####
# From Bondi pp 224
# Exploration time in Caracara
# measured diff between M and F
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



