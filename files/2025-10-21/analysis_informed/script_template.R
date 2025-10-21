## HEADER ####
# who: name of author
# when: date of analysis yyyy-mm-dd
# what: title of analysis

## CONTENTS ####
# 01 DATA PREP AND SETUP
# 02 EDA
# 03 ANALYSIS

# 01 DATA PREP AND SETUP ####
# This here is me loading the necessary packages and libraries
library(package_name)

# This here is me getting and prepping the data for analysis

mydata <- read.csv("path/to/mydata.csv")
names(mydata)
str(mydata)

# This here is me converting variable classes if necessary
mydata$my_factor_variable <- factor(mydata$my_factor_variable)
mydata$my_numeric_variable <- as.numeric(mydata$my_numeric_variable)

# 02 EDA ####
# This here is my EDA
# This will typically include bivariate plots of the response variable vs predictor variables, 
# and maybe some multivariate plots of the data

# 03 ANALYSIS ####
# This here is my analysis