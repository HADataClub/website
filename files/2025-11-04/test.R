## Info ####
## what: GBIF data demo for HADC!
## when: 2025-11-04
## Who: Ed H

## Contents ####
## 1 Setup
## 2 Simple search
## 3 Caveats
## 4 Download real data

## 1 Setup ####
library(rgbif)
library(ggplot2)

# Setup also includes pondering questions like when, where and what data do you want

## 2 Simple search ####

# Simplest search: NB limit of 500 records
occ_search(scientificName = "Troglodytes troglodytes") 

# Country code argument
occ_search(scientificName = "Erithacus rubecula", country = "GB")

# NB weird year specification format - there are many more weirdnesses!
occ_search(scientificName = "Erithacus rubecula", country = "GB", year="2020,2025")

# look up a single occurrence record
occ_get(key=5006738747) # not many reasons to do this but it exists


## 3 Caveats ####
count <- 0
for(i in 1:25){
  count[i] <- occ_count(year=i+1999)
}
mydat <- data.frame(year = 2000:2024, count)

ggplot(mydat, aes(x = year, y = log10(count))) +
  geom_line() + 
  geom_point() + 
  ylab("Log10 Occurrence records") +
  xlab("Year") +
  theme_minimal()
         


     