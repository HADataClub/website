## HEADER ####
# who: name of author
# when: date of analysis yyyy-mm-dd
# what: title of analysis

## CONTENTS ####
# 01 DATA PREP AND SETUP
# 02 EDA
# 02 ANALYSIS


# 01 DATA PREP AND SETUP ####

# This here is me loading the necessary packages and libraries
library(package_name)

# This here is me getting and prepping the data for analysis

mydata <- read.csv("path/to/mydata.csv")
names(mydata)
head(mydata)

# This here is me converting variable classes if necessary
mydata$my_factor_variable <- factor(mydata$my_factor_variable)
mydata$my_numeric_variable <- as.numeric(mydata$my_numeric_variable)


# 02 EDA ####

# This here is my EDA
mosaicplot(table(wilt$crop, wilt$wilt_bin), 
           color = c("darkgreen", "goldenrod"),
           ylab = "Wilting present",
           main = "Rotation crop")
mosaicplot(table(wilt$amendment, wilt$wilt_bin), 
           color = c("darkgreen", "goldenrod"),
           ylab = "Wilting present",
           main = "Amendment")
mosaicplot(table(wilt$block, wilt$wilt_bin), 
           color = c("darkgreen", "goldenrod"),
           ylab = "Wilting present",
           main = "Block")

# 02 ANALYSIS ####

# This here is my analysis
# install.packages("lme4")
library(lme4) # Statistician's preference
# install.packages("lmerTest")
library(lmerTest) # Calculates your p-vals in case CI is not good enough for you

lme1 <- glmer(wilt_bin ~ crop + amendment + (1|block), 
            data = wilt,
            family = binomial(link = "logit") )

print(lme1, corr=F)
summary(lme1)

se <- sqrt(diag(vcov(lme1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(lme1), LL = fixef(lme1) - 1.96 * se, UL = fixef(lme1) + 1.96 *
                se))

# install.packages("sjPlot")
library(sjPlot)

plot_model(lme1, type = "pred", terms = c("crop"))
plot_model(lme1, type = "pred", terms = c("amendment"))
