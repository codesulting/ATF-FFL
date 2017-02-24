# Model Building

## Rural-Urban Proportions data

The value that we're looking to explain here is the per capita Federal Firearms License count, by state. Previously, what appeared to be an inverse relationship between a state's population and FFL count was observed. 

Are there characteristics of the population that might also contribute to FFL counts? 

Does the percentage of the population living in Rural Areas play a role? In Urban Clusters? 

Could the Land Area of Rural America have an effect on the number of Federal Firearms Licenses in a given state? 

_note:_ Why look at only at Rural Populations and Land Areas, and not also Urbanized Areas? The US census defines these two as opposites - any land that doesn't fit the population and land area criteria for 'Urban' is by default considered 'Rural'.

```{R}
rural.urban.01 <- lm(perCapitaFFL ~ POPPCT_RURAL + POPPCT_UC + AREAPCT_RURAL + AREAPCT_UC + 
                     AREA_RURAL + AREA_UC, data = ffl.16)
```

This first model looks to see if per Capita FFL counts could be explained by: 

- percentage of population _n_ living in Rural Areas, _n_ < 2,500
- percentage of population _n_ living in Urban Clusters (2,500 < _n_ < 50,000)
- percentage of Rural Land Area
- percentage of Urban Cluster Land Area
- total Rural Land Area
- total Urban Cluster Land Area

What do the coefficients for this model look like? 

```{R}
tidy(rural.urban.01)
           term      estimate    std.error statistic      p.value
1   (Intercept)  3.912484e+01 1.559204e+01  2.509284 1.585326e-02
2  POPPCT_RURAL  4.035331e-01 1.178348e-01  3.424567 1.343717e-03
3     POPPCT_UC  1.844273e+00 2.363076e-01  7.804543 7.711391e-10
4 AREAPCT_RURAL -4.634996e-01 1.815600e-01 -2.552872 1.422859e-02
5    AREAPCT_UC -5.395559e+00 1.584342e+00 -3.405552 1.419769e-03
6    AREA_RURAL  2.515727e-11 6.923526e-12  3.633592 7.270708e-04
```

Going by `p.value`, the most significant explanatory variables in this case would be:

- Urban Cluster Population Percentage (`POPPCT_UC`)
- Rural Land Area (`AREA_RURAL`). 

How do the R-squared and fitted values look? 

```{R}
glance(rural.urban.01)[, c(1:5, 8, 10)]
# r.squared   adj.r.squared    sigma statistic      p.value      AIC deviance
# 1 0.8333481     0.8144104 9.306224  44.00469 4.927771e-16 372.5705 3810.656

summary(rural.urban.01)
Residuals:
     Min       1Q   Median       3Q      Max 
-18.4432  -4.7065   0.5332   5.0442  30.2534 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)    2.960e+01  1.600e+01   1.851  0.07110 .  
POPPCT_RURAL   3.944e-01  1.147e-01   3.438  0.00131 ** 
POPPCT_UC      1.652e+00  2.517e-01   6.565 5.51e-08 ***
AREAPCT_RURAL -3.011e-01  1.967e-01  -1.531  0.13320    
AREAPCT_UC    -4.606e+00  1.598e+00  -2.883  0.00612 ** 
AREA_RURAL     2.713e-11  6.817e-12   3.981  0.00026 ***
AREA_UC       -3.726e-09  1.989e-09  -1.873  0.06791 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 9.052 on 43 degrees of freedom
Multiple R-squared:  0.8459,	Adjusted R-squared:  0.8244 
F-statistic: 39.34 on 6 and 43 DF,  p-value: 6.618e-16
```

![histogram of residuals 01](R_plots/01-model-building/rural01-hist-resid.png)

I'd like to say the distribution of residuals approaches Normal - except for the outliers in the 30s. 

![lm01 plot](R_plots/01-model-building/rural01-lm-plot.png)

Alaska seems to be exerting a strong influence, as do Wyoming and Delaware. Montana, though, constantly appears as an oultier as well. How much did each of these affect the parameter estimates? 

```{R}
rural.urban.02 <- update(rural.urban.01, subset = (NAME != "Alaska" & NAME != "Wyoming" & NAME != "Montana"))

summary(rural.urban.02)
Residuals:
    Min      1Q  Median      3Q     Max 
-12.280  -2.828  -0.162   4.012  14.686 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)    5.565e+00  1.074e+01   0.518  0.60702    
POPPCT_RURAL   5.061e-01  8.949e-02   5.655 1.44e-06 ***
POPPCT_UC      6.580e-01  2.144e-01   3.069  0.00385 ** 
AREAPCT_RURAL  3.597e-02  1.382e-01   0.260  0.79601    
AREAPCT_UC    -2.375e+00  1.201e+00  -1.978  0.05483 .  
AREA_RURAL     3.524e-11  1.361e-11   2.589  0.01336 *  
AREA_UC       -5.444e-09  1.598e-09  -3.406  0.00151 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.72 on 40 degrees of freedom
Multiple R-squared:  0.8511,	Adjusted R-squared:  0.8288 
F-statistic:  38.1 on 6 and 40 DF,  p-value: 4.889e-15
```

Glancing at the summary, there's a better chance that the residuals will be closer to normally distributed.

![histogram of residuals 02](R_plots/01-model-building/rural02-hist-resid.png)

![lm02 plot](R_plots/01-model-building/rural02-lm-plot.png)

TODO: CREATE TABLE COMPARING MODEL.01 AND MODEL.02

Removing large outliers might be revealing the presence of other, (relatively) smaller outliers. 

- What about looking at the strongest explanatory variables from the first model? `POPPCT_UC` & `AREA_RURAL`
- Would it be beneficial to try a reduced model with only these variables, with a minimally adequate model as a goal?

Before fitting this reduced model, a quick look at these variables against per capita FFL counts.

![FFLs ~ POPPCT_UC](R_plots/01-model-building/rural01-obs-POPPCT_UC.png)

![FFLs ~ AREA_RURAL](R_plots/01-model-building/rural01-obs-AREA_RURAL.png)

Visually (and very roughly), `POPPCT_UC` appears more likely than `AREA_RURAL` to be able to describe per capita FFLs in a linear model.

```{R}
rural.urban.03 <- lm(perCapitaFFL ~ POPPCT_UC + AREA_RURAL, data = ffl.16)
summary(rural.urban.03)
Residuals:
    Min      1Q  Median      3Q     Max 
-33.858  -5.848   0.395   6.428  35.333 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.480e+00  3.254e+00  -0.455  0.65133    
POPPCT_UC    2.064e+00  2.098e-01   9.840 5.37e-13 ***
AREA_RURAL   2.463e-11  7.507e-12   3.281  0.00195 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 11.26 on 47 degrees of freedom
Multiple R-squared:  0.7392,	Adjusted R-squared:  0.7281 
F-statistic: 66.62 on 2 and 47 DF,  p-value: 1.912e-14
```

![rural03-lm-plot.png](R_plots/01-model-building/rural03-lm-plot.png)

With the model reduced to these two variables, the Q-Q plot shows the residuals closest to normally distributed out of any of the previous models. Outliers such as Montana and Wyoming still exert influence, but less so from the first model. 

![rural03](R_plots/01-model-building/rural03-hist-resid.png)




















