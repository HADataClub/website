
library(rgbif)


clade <- "Aves"
gadm_code <- "GBR.1.84.1_1" # Shropshire GADM code

occ_search_result <- occ_search(
  gadmGid = gadm_code,
  scientificName = clade,
  hasCoordinate = TRUE,
  limit = 500
)
return(occ_search_result)

search_occurrences$meta$count  # See how many records matched
