## Info ####
## who: Ed H
## what: HADC! Likert viz
## when: 2025-12-09

## Contents ####
## 01 Setup
## 02 Data exploration
## 03 Graphing options
## 04 References

## 01 Setup ####
library(ggstats)
library(dplyr)
library(ggplot2)

source("scripts/make_likert_data.R")

## 02 Data exploration ####
likert_levels
likert_levels_dk

head(df)

## 03 Graphing options ####

### Quickest plot ####
gglikert(df) # Note the details!

### Specific questions ####
gglikert(df, include = q1:q3) 

### Custom colours ####
gglikert(df) +
  ggtitle("A Likert-type items plot", 
          subtitle = "generated with gglikert()") +
  scale_fill_brewer(palette = "RdYlBu")

### Sort by response ####
gglikert(df, sort = "ascending") # default sort by proportion

gglikert(df, sort = "ascending", sort_method = "mean") # Sort by mean etc.

### Order of answers
gglikert(df, reverse_likert = TRUE)


### Proportion labels ####
gglikert(df, add_labels = FALSE) # remove

gglikert( # Customise
  df,
  labels_size = 3,
  labels_accuracy = .1,
  labels_hide_below = .2,
  labels_color = "blue"
)

### Tweak margin totals
gglikert(
  df,
  totals_include_center = TRUE,
  sort = "descending",
  sort_prop_include_center = TRUE
) # half centre added to each side

gglikert( # Customise
  df,
  totals_size = 4,
  totals_color = "blue",
  totals_fontface = "italic",
  totals_hjust = .20
)

gglikert(df, add_totals = FALSE) # Remove

### Question labels ####
if (require(labelled)) {
  df <- df |>
    set_variable_labels(
      q1 = "first question",
      q2 = "second question",
      q3 = "this is the third question with a quite long variable label"
    )
}
gglikert(df)

gglikert( # Alter labels
  df,
  variable_labels = c(
    q1 = "alternative label for the first question",
    q6 = "another custom label"
  )
)

gglikert(df, y_label_wrap = 20) # label wrapping



## 04 References ####

# Vignette
# Lamarange. Plot Likert-like items with gglikert()
# https://larmarange.github.io/ggstats/articles/gglikert.html

# Agresti. 2013. Categorical Data Analysis 3ed 