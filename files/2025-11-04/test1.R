
library(rgbif)


clade <- "Aves"
gadm_code <- "GBR.1.84.1_1" # Shropshire GADM code

# Test 1: Just GADM code
occ_search(gadmGid = "GBR.1.84.1_1", limit = 10)

# Test 2: Try the bounding box approach instead
occ_search(
  geometry = "POLYGON((-3.1 52.3, -3.1 53.1, -2.4 53.1, -2.4 52.3, -3.1 52.3))",
  scientificName = "Aves",
  hasCoordinate = TRUE,
  limit = 500
)

# Test 3: Try with country + stateProvince variations
occ_search(
  country = "GB",
  scientificName = "Aves",
  hasCoordinate = TRUE,
  decimalLatitude = "52.3,53.1",
  decimalLongitude = "-3.1,-2.4",
  limit = 500
)

occ_search(
  gadmGid = "GBR.1.84.1_1",
  taxonKey = 212,
  hasCoordinate = TRUE,
  limit = 500
)
