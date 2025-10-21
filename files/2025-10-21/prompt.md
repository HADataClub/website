# This here is the prompt file for my stats analysis project

## Files
- Use the following files:
    - script_template.R for a basic R analysis script template
    - report_template_brief.qmd for a Quarto-rendered report based on the analysis
    - guidelines.md for general instructions for you to strictly adhere to

## Specific instructions

### Data
- wilt.csv is my data file
- Data description: Data are a subset of the CIC dataset in the R package {agricolae} in case you need to cite it:
  Felipe de Mendiburu and Muhammad Yaseen(2020).  agricolae:
  Statistical Procedures for Agricultural Research.R package version 1.4.0, https://myaseen208.github.io/agricolae/https://cran.r-project.org/package=agricolae.
- wilt_percent is the dependent var; there are only 2 values so it is binary
- amendment and crop are factors - the explanatory variables of interest
- block is a blocking factor

### Instructions
- Analyse my data using an appropriate statistical test
- script.R should be based on script_template.R and should provide a basic analysis including inferential statistical results and appropriate graphs
- report.qmd should be based on the template report_template_brief.qmd and contain a report-based version of the analysis in script.R, following the guidelines within report_tempalte_brief.qmd itself
- The analysis need not be exhaustive, but should analyse the data mentioned in a single model, e.g. a multivariate linear model is better than several univariate models
- Avoid extensive exploratory data analysis 
- To account for the block factor and binary dependent variable, consider a mixed effects logistic model