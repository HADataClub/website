## Info ####
# who:
# what:
# when:


## CONTENTS ####
# 1 Setup
# 2 Look at "donor Data"
# 3 Synthetic data generation
# 4 Reference

# 1 Setup ####
# install.packages("synthpop")
library(synthpop)
?synthpop

# 2 Look at "donor Data" ####
# This code will use the supplied data frame SD2011
help(SD2011)                   # this will give you information about it
dim(SD2011)                    # get size of data frame 
codebook.syn(SD2011)$tab       # get summary info about variables

# Notice that eduspec has 27 levels - so leave this for later.
# Note the negative values in some continuous variables (e.g. nociga).
# SD2011 has 35 variables, so lets go for a smaller number.
# Also bmi could be derived from height and weight, 
# so we'll leave it out for now.

mydata <- SD2011[, c(1, 3, 6, 8, 11, 17, 18, 19, 20, 10)] 
codebook.syn(mydata)$tab 

# Check weird negative income values
table(mydata$income[mydata$income < 0], useNA = "ifany")

# We can see that income has both NA values and -8. 
# We will ignore this for now
mysyn <- syn(mydata)  

summary(mysyn)
compare(mysyn, mydata, stat = "counts")

# 3 Synthetic data generation ####



# 4 Reference ####

# Synthpop on CRAN
# https://cran.r-project.org/web/packages/synthpop/index.html

# Synthpop info
# https://www.synthpop.org.uk/get-started.html
