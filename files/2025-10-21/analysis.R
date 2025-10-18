
# This here is me getting and prepping the data for analysis
# install.packages("agricolae")
library(agricolae)

?Chz2006
data(Chz2006)
names(Chz2006)

wilt <- Chz2006$wilt
head(wilt)
names(wilt)

table(wilt$wilt_percent)
wilt$block <- factor(wilt$block)

# This here creates a binary version of the wilt var
wilt$wilt_bin <- wilt$wilt_percent
wilt$wilt_bin[wilt$wilt_bin == 50] <- 1


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

# This here is my analysis
# install.packages("lme4")
library(lme4) # Statistician's preference
# install.packages("lmerTest")
library(lmerTest) # Calculates your p-vals in case CI is not good enough for you

mod <- glmer(wilt_bin ~ crop + amendment + (1|block), 
            data = wilt,
            family = binomial(link = "logit") )

print(mod, corr=F)
summary(mod)

se <- sqrt(diag(vcov(mod)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(mod), LL = fixef(mod) - 1.96 * se, UL = fixef(mod) + 1.96 *
                se))

install.packages("sjPlot")
library(sjPlot)

plot_model(mod, type = "pred", terms = c("crop"))
plot_model(mod, type = "pred", terms = c("amendment"))
