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

gglikert(df, y_label_wrap = 200)

### Customise centre ####

# cutoff controls how many categories are considered "negative"
gglikert(df, cutoff = 0)
gglikert(df, cutoff = 1)
gglikert(df, cutoff = 1.25) # first and 1/4 of second are neg
gglikert(df, cutoff = 1.75) # NB only affects centreing (spp?)
gglikert(df, cutoff = 3)
gglikert(df, cutoff = 5)

### X axis symmetry ####
gglikert(df, cutoff = 1)
gglikert(df, cutoff = 1, symmetric = TRUE)

### Customise on certain values ####
gglikert(df_dk)

# Convert Don't Know to NA
df_dk |>
  mutate(across(everything(), ~ factor(.x, levels = likert_levels))) |>
  gglikert()

# Exclude fill values but still use for proportion
df_dk |> gglikert(exclude_fill_values = "Don't know")


### Facets OMG! ####
# Simulate some groups for grouping
df_group <- df
df_group$group1 <- sample(c("A", "B"), 150, replace = TRUE)
df_group$group2 <- sample(c("a", "b", "c"), 150, replace = TRUE)

gglikert(df_group,
         q1:q6,
         facet_cols = vars(group1),
         labels_size = 3
)

# By 2 groups 
gglikert(df_group,
         q1:q2,
         facet_rows = vars(group1, group2),
         labels_size = 3
)

# Go completely crazy
gglikert(df_group,
         q3:q6,
         facet_cols = vars(group1),
         facet_rows = vars(group2),
         labels_size = 3
) +
  scale_x_continuous(
    labels = label_percent_abs(),
    expand = expansion(0, .2)
  )

### Stacked Bar Plot (classic... or old fashioned) ####
gglikert_stacked(df)

gglikert_stacked( # Customise
  df,
  sort = "asc",
  add_median_line = TRUE,
  add_labels = FALSE
)

# With facets
gglikert_stacked(
  df_group,
  include = q1:q4,
  y = "group2"
) +
  facet_grid(
    rows = vars(.question),
    labeller = label_wrap_gen(15)
  )

### weighting data (moderately advanced) ####

# simulate some weights 
df$sampling_weights <- runif(nrow(df))
gglikert(df, q1:q4, weights = sampling_weights)


### Long data for stacked barplots ####
gglikert_data(df) |>
  head()

ggplot(gglikert_data(df[, 1:6])) +
  aes(y = .question, fill = .answer) +
  geom_bar(position = "fill")

## 04 References ####

# Vignette
# Lamarange. Plot Likert-like items with gglikert()
# https://larmarange.github.io/ggstats/articles/gglikert.html

# Agresti. 2013. Categorical Data Analysis 3ed 