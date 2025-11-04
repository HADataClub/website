
library(rgbif)

bbox <- c(min_longitude = -3.1,
          max_longitude = -1.9, 
          min_latitude = 52.3,
          max_latitude = 53.1)


clade <- "Aves"

x<- occ_search(
  geometry = paste("POLYGON((", bbox["min_longitude"], " ", bbox["min_latitude"], ",", 
                   bbox["min_longitude"], " ", bbox["max_latitude"], ",", 
                   bbox["max_longitude"], " ", bbox["max_latitude"], ",", 
                   bbox["max_longitude"], " ", bbox["min_latitude"], ",", 
                   bbox["min_longitude"], " ", bbox["min_latitude"], "))"),
  month = c(1:12), # define months of the year
  year = 2025,
  scientificName = clade,
  hasCoordinate = TRUE
)

x$meta$count  # See how many records matched
