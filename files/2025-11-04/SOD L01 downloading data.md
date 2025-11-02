# Tutorial (LO1): Import Species Occurrence Data into R

**Focus:** UK example — *bird (Aves) records from GBIF* within *Shropshire county*  
**Format:** Markdown tutorial with embedded R code

---

## 0. Front Matter
- **Title:** Species Occurrence Data Download — Birds in Shropshire  
- **Authors:** [Your Name / Institution]  
- **Date:** [Insert Date]  
- **Learning Objective:** LO1  
- **Estimated Time:** 60–90 minutes  
- **Prerequisites:** Basic R, familiarity with data frames, packages `{sf}`, `{rgbif}`  
- **Dataset License:** GBIF open data license (CC BY 4.0)  
- **Reproducibility:** Set seed and working directory  

---

## 1. Introduction
Species occurrence data underpin biodiversity assessments, ecosystem service evaluations, and agri-environmental schemes.  
In this tutorial, we will use R to **import bird occurrence data from GBIF** for the county of **Shropshire, UK**, focusing on data collected since the year 2000.  
This dataset will form the foundation for subsequent cleaning, bias correction, and spatial analysis exercises in later modules.

---

## 2. Learning Outcomes
By completing this section, participants will be able to:
1. Identify and access key biodiversity data portals.  
2. Understand GBIF’s API structure and querying system.  
3. Import occurrence records reproducibly into R.  
4. Apply spatial and temporal filters.  
5. Document provenance and licensing information.

---

## 3. Setup and Project Scaffolding
```r
#| label: setup
packages <- c("rgbif", "sf", "osmdata", "dplyr", "readr", "ggplot2")
invisible(lapply(packages, \(p) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)))
lapply(packages, library, character.only = TRUE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

## 4. Define Study Region: Shropshire Boundary (OSM)
#| label: shropshire-boundary
shrop <- opq("Shropshire, England") |>
  add_osm_feature(key = "admin_level", value = "6") |>
  osmdata_sf()

shrop_poly <- shrop$osm_multipolygons |>
  dplyr::filter(grepl("^Shropshire$", name)) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

wkt_shrop <- sf::st_as_text(sf::st_geometry(sf::st_union(shrop_poly)))

## 5. Plan the GBIF Query
# Taxon: Aves
# Spatial Filter: Shropshire county boundary
# Temporal Filter: Records from 2000–present
# Options: Only georeferenced records
# Output: Record fields and license information
# Citation: GBIF download DOI is automatically generated

## 6. Quick Exploratory Import — rgbif::occ_search()
#| label: gbif-sample
birds_sample <- rgbif::occ_search(
  taxonKey = name_backbone(name = "Aves")$usageKey,
  geometry = wkt_shrop,
  hasCoordinate = TRUE,
  year = "2000,2025",
  limit = 300
)$data

dplyr::glimpse(birds_sample)

## 7. Reproducible Bulk Import — rgbif::occ_download()
#| label: gbif-download
library(rgbif)

aves_key <- name_backbone(name = "Aves")$usageKey

preds <- pred_and(
  pred("taxonKey", aves_key),
  pred_within(wkt_shrop),
  pred("hasCoordinate", TRUE),
  pred_gte("year", 2000)
)

key <- occ_download(preds, format = "SIMPLE_CSV")
occ_download_wait(key)

zip_path <- occ_download_get(key, path = "data/raw")[[1]]
occ <- occ_download_import(zip_path)

## 8. Restricting to a Single Source (Example: iNaturalist)
#| label: gbif-inat
inat_key <- "50c9509d-2de5-4b59-97d3-3c6bdfd017f0"

preds_inat <- pred_and(
  pred("taxonKey", aves_key),
  pred_within(wkt_shrop),
  pred("hasCoordinate", TRUE),
  pred_gte("year", 2000),
  pred("datasetKey", inat_key)
)

key_inat <- occ_download(preds_inat, format = "SIMPLE_CSV")
occ_download_wait(key_inat)

zip_path_inat <- occ_download_get(key_inat, path = "data/raw")[[1]]
occ_inat <- occ_download_import(zip_path_inat)

nrow(occ_inat)

## 9. Minimal Validation and Metadata Capture
#| label: gbif-metadata
occ |> count(license)
datasets <- unique(occ$datasetKey)
write_lines(
  paste("GBIF download key:", key,
        "\nDatasets:", paste(datasets, collapse = ", ")),
  "data/raw/README_data.md"
)

## 10. Quick Visual Check
#| label: map-peek
ggplot() +
  geom_sf(data = shrop_poly) +
  geom_point(aes(decimalLongitude, decimalLatitude),
             data = occ, alpha = 0.2, size = 0.5) +
  coord_sf() +
  labs(title = "GBIF Bird Records in Shropshire (2000–Present)")

## 11. Common Pitfalls & Checklist
- County boundary mismatches
- Duplicated records across datasets
- Licensing restrictions
- Temporal or spatial sampling bias
- Sensitive species redaction or coordinate generalization

## 12. Save Outputs and Session Info
#| label: save-outputs
write_csv(occ, "data/raw/gbif_aves_shropshire.csv")
saveRDS(shrop_poly, "data/raw/shropshire_boundary.rds")
sessionInfo()

## 13. Exercises
Exercise 1: Temporal filtering
Modify the code in section 7 to restrict the dataset to records collected after 2015.
Hint: Replace pred_gte("year", 2000) with pred_gte("year", 2015).
After re-importing, compare record counts and discuss any notable changes in data density.

Exercise 2: Source comparison
Use the code in section 8 to download data separately from iNaturalist and from all GBIF datasets combined.
Compare the number of records per source and visualize both datasets on the same map.
Discuss the possible reasons for differences in spatial coverage or sampling bias.

Exercise 3: Basis of record filtering
Explore the use of pred_in("basisOfRecord", c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION")).
Filter the data to retain only wild bird observations (exclude captive or fossil specimens).
Count how many records remain and reflect on how this affects the dataset for ecological modelling.

Exercise 4: Custom region
Modify the spatial query to focus on a different UK county (e.g., Herefordshire or Cheshire).
Compare the number of records retrieved and plot both regions to visualize biodiversity differences.

Exercise 5: Metadata inspection
Inspect the dataset metadata (datasetKey, license, datasetName).
Create a simple table summarizing the number of records by datasetName.
Identify which sources contribute most to the total Shropshire bird data.


## Appendix A: Alternative Data Sources
## Appendix A: Alternative Data Sources (Programmatic API Examples)

> All examples below are **fully programmatic**. They reuse the Shropshire boundary (WKT) built with OSM.  
> Run this boundary setup once, then use it across sources.

```r
# Boundary setup (reused by all sections)
# install.packages(c("sf","osmdata","dplyr"))
library(sf); library(osmdata); library(dplyr)

shrop <- opq("Shropshire, England") |>
  add_osm_feature(key = "admin_level", value = "6") |>
  osmdata_sf()

shrop_poly <- shrop$osm_multipolygons |>
  dplyr::filter(grepl("^Shropshire$", name)) |>
  st_make_valid() |>
  st_transform(4326)

# WKT polygon + bbox for services that only accept bbox
wkt_shrop <- st_as_text(st_geometry(st_union(shrop_poly)))
bb <- st_bbox(st_union(shrop_poly))  # xmin, ymin, xmax, ymax
```

