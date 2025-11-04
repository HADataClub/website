## Info ####
## what: GBIF data demo for HADC!
## when: 2025-11-04
## Who: Ed H

## Contents ####
## 1 Setup
## 2 GBIF search function
## 3 Shiny ui
## 4 Shiny server

occ_search(scientificName = "Erithacus rubecula")
occ_search(scientificName = "Erithacus rubecula",country = "GB")
occ_search(scientificName = "Erithacus rubecula",country = "GB",year="2020,2025")

# look up a single occurrence record
occ_get(key=5006738747) # not many reasons to do this but it exists

year<-0
for(i in 1:25){
  year[i] <- occ_count(year=i+1999)
}

plot(2000:2024, log10(year),
    # ylim = c(500000, 315000000),
     type = 'b')

     