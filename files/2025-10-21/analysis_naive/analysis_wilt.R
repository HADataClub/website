
# Analysis script for wilt.csv
# Reads data, sets factors, fits mixed model with block as random effect and performs ANOVA.
# Requires packages: readr, dplyr, lme4, lmerTest, emmeans, ggplot2, car (optional)

library(readr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)

df <- read_csv("wilt.csv")
df <- df %>% mutate(
  amendment = as.factor(amendment),
  crop = as.factor(crop),
  block = as.factor(block)
)

# 1) OLS ANOVA treating block as fixed (classic RCBD)
ols <- aov(wilt_percent ~ amendment * crop + block, data = df)
summary(ols)

# 2) Mixed-effects model treating block as random
model_mixed <- lmer(wilt_percent ~ amendment * crop + (1 | block), data = df)
summary(model_mixed)
anova(model_mixed)  # sequential (Type I) for mixed model
# For Type III tests:
if(requireNamespace("car", quietly=TRUE)) {
  library(car)
  Anova(model_mixed, type=3)
}

# Estimated marginal means and pairwise comparisons
emm <- emmeans(model_mixed, ~ amendment * crop)
print(emm)
pairs(emm)

# Interaction plot
p <- ggplot(df, aes(x=amendment, y=wilt_percent, group=crop)) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(fun=mean, geom="line") +
  facet_wrap(~crop) +
  labs(title="Interaction: mean wilt_percent by amendment and crop")
ggsave("interaction_plot_R.png", p, width=8, height=5)

# Diagnostic plots saved by base plot
png("residuals_plot_R.png", width=800, height=600)
plot(model_mixed)
dev.off()
