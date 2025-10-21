
# Wilt percent analysis — brief report

**Data:** `wilt.csv` (subset of CIC dataset from agricolae package).  
**Response:** `wilt_percent`.  
**Predictors:** `amendment`, `crop`.  
**Blocking factor:** `block`.

## Methods
- OLS ANOVA including `block` as a fixed effect (RCBD).
- Linear mixed-effects model with `block` as random intercept (lmer).
- Interaction examined; use emmeans for pairwise contrasts if needed.

## OLS ANOVA results (block as fixed effect)
Number of observations: 1920  
amendment levels: 0C, 3C, 3C1Z, 6C  
crop levels: Cabbage, Corn, Fallow, Pea  
number of blocks: 3

ANOVA table (Type II where available):
```
                           sum_sq      df         F  PR(>F)
C(amendment)           75656.2500     3.0  112.0832  0.0000
C(crop)                 4322.9167     3.0    6.4043  0.0003
C(block)                 799.4792     2.0    1.7766  0.1695
C(amendment):C(crop)    1187.5000     9.0    0.5864  0.8092
Residual              427950.5208  1902.0       NaN     NaN
```

## Recommendation / Interpretation
- Look at `amendment:crop` interaction p-value in the ANOVA table. If p < 0.05, interpret treatment effects within each crop (use emmeans output in R).  
- If interaction not significant, interpret main effects from the mixed model.  
- Use diagnostic plots to check assumptions (residual homogeneity and normality). If serious violations occur, consider transformation or nonparametric alternatives.

## Files included
- `analysis_wilt.R` — R script to reproduce the analysis.
- `interaction_plot_R.png` and `residuals_plot_R.png` — plots saved by the R script when run.
