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
library(usethis)

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
         
## 4 Download real data ####

# You could...  set credentials explicitly in this session if needed
# options(gbif_user = "username")
# options(gbif_pwd = "passsword")
# options(gbif_email = "email@mail.com")

# Download Troglodytes troglodytes for Shropshire, 2024
# Shropshire bounding box (approximate): 
# West: -3.2, East: -2.3, South: 52.4, North: 53.0

# OR, need a .Renviron file with variables GBIF_USER, GBIF_PWD, GBIF_EMAIL set
# usethis::edit_r_environ()

# Test 1: Check if there are ANY records for this species in 2024 UK
test_search <- occ_search(
  scientificName = "Troglodytes troglodytes",
  country = "GB",
  year = "2024",
  hasCoordinate = TRUE,
  limit = 10
)
cat("Total records for Troglodytes troglodytes in GB 2024:", test_search$meta$count, "\n")

# Test 2: Try 2023 instead
test_2023 <- occ_search(
  scientificName = "Troglodytes troglodytes",
  country = "GB",
  year = "2023",
  hasCoordinate = TRUE,
  limit = 10
)
cat("Total records for Troglodytes troglodytes in GB 2023:", test_2023$meta$count, "\n")

# Test 3: Check records in the Shropshire bounding box with broader year range
test_shrop <- occ_search(
  scientificName = "Troglodytes troglodytes",
  geometry = "POLYGON((-3.2 52.4, -2.3 52.4, -2.3 53.0, -3.2 53.0, -3.2 52.4))",
  year = "2020,2024",
  hasCoordinate = TRUE,
  limit = 10
)
cat("Records in Shropshire box 2020-2024:", test_shrop$meta$count, "\n\n")

# Download with corrected predicates (using pred_and to combine)
download_key <- occ_download(
  pred_and(
    pred("taxonKey", 2490974),  # Troglodytes troglodytes
    pred("year", 2024),
    pred("hasCoordinate", TRUE),
    pred_within("POLYGON((-3.2 52.4, -2.3 52.4, -2.3 53.0, -3.2 53.0, -3.2 52.4))")
  ),
  user = Sys.getenv("GBIF_USER"),
  pwd = Sys.getenv("GBIF_PWD"),
  email = Sys.getenv("GBIF_EMAIL")
)

# Check download status
occ_download_wait(download_key)

# Import the data
wren_data <- occ_download_get(download_key) %>%
  occ_download_import()