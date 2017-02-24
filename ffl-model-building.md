# Model Building

## Rural-Urban Proportions

The value that we're looking to explain here is the per capita Federal Firearms License count, by state. Previously, what appeared to be an inverse relationship between a state's population and FFL count was observed. 

Are there characteristics of the population that might also contribute to FFL counts? 

Does the percentage of the population living in Rural Areas play a role? In Urban Clusters? 

Could the Land Area of Rural America have an effect on the number of Federal Firearms Licenses in a given state? 

_note:_ Why look at only at Rural Populations and Land Areas, and not also Urbanized Areas? The US census defines these two as opposites - any land that doesn't fit the population and land area criteria for 'Urban' is by default considered 'Rural'.

## Model 01 - Population and Land Area Features

This first model looks to see if per Capita FFL counts could be explained by: 

- percentage of population _n_ living in Rural Areas, _n_ < 2,500
- percentage of population _n_ living in Urban Clusters (2,500 < _n_ < 50,000)
- percentage of Rural Land Area
- percentage of Urban Cluster Land Area
- total Rural Land Area
- total Urban Cluster Land Area

```{R}
rural.urban.01 <- lm(perCapitaFFL ~ POPPCT_RURAL + POPPCT_UC + AREAPCT_RURAL + AREAPCT_UC + 
                     AREA_RURAL + AREA_UC, data = ffl.16)
```

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

### Model 02: How Influential are the Outliers?

To measure the influence of the outliers, I removed them and updated the first model.

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

## Model 03: Further Reductions

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

![rural03-fitted-obs](R_plots/01-model-building/rural03-fitted-obs.png)

## Model Comparison

None of the 3 models above are perfect.

```{R}
model.comparison <- glance(rural.urban.01) %>%
  bind_rows(glance(rural.urban.02), 
            glance(rural.urban.03)) %>%
  mutate(model = 1:3) %>%
  print
  
  r.squared adj.r.squared     sigma statistic      p.value df    logLik      AIC      BIC deviance df.residual model
1 0.8459155     0.8244153  9.051906  39.34460 6.617928e-16  7 -177.3251 370.6502 385.9464 3523.291          43     1
2 0.8510906     0.8287542  5.720005  38.10329 4.889207e-15  7 -144.8669 305.7338 320.5350 1308.738          40     2
3 0.7392453     0.7281493 11.263203  66.62300 1.911863e-14  3 -190.4771 388.9542 396.6023 5962.408          47     3
```

- `AIC` drops dramatically in the 2nd model, where influential outliers were removed.
- Although `AIC` and both R-Squared metrics are 'best' in the 2nd model, the 1st and 3rd models both appear to conform better to the assumption of normally distributed residuals for linear models. 

While on paper the 1st model appears to be preferable to the 3rd, a minimally adequate model is the overall goal here. It might be worth checking independence of variables in the 1st model, as I suspect they might show collinearity. 

```{R}
model.01.cor <- ru.01.fit %>%
  select(3:8) %>%
  cor(.)

par(mfrow = c(1, 1), family = "GillSans")
corrplot(model.01.cor, method = "shade", shade.col = NA,
         tl.col = "gray23", tl.srt = 45, tl.cex = 1, 
         addCoef.col = "black", number.cex = 1,
         order = "hclust", mar = c(1, 1, 1, 1))
```

![rural01-corrplot](R_plots/01-model-building/rural01-corrplot.png)

Correlation matrix actually shows some reasonable values. 

## Model 04 - Inversely Proportional

What would happen if adding the roughly _inverse relationship_ between Population and Federal Firearms Licenses that was observed in exploration? 

![perCapitaFFLs-population](R_plots/01-model-building/perCapitaFFLs-Population.png)

Plotting with a function shows that _y = 1/x_ would not be an exact fit, but rather a _very_ general approximation. How does this approximation interact with the previous model? 


```{R}
ffl.16$perCapitaPop <- ffl.16$POPESTIMATE2016/100000

inverse.02 <- lm(perCapitaFFL ~ I(1/perCapitaPop) + POPPCT_UC + AREA_RURAL, data = ffl.16)

summary(inverse.02)
Residuals:
    Min      1Q  Median      3Q     Max 
-34.936  -3.294  -0.661   6.044  35.321 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)       -6.337e-01  3.050e+00  -0.208  0.83632    
I(1/perCapitaPop)  1.287e+02  4.544e+01   2.832  0.00684 ** 
POPPCT_UC          1.637e+00  2.471e-01   6.623 3.35e-08 ***
AREA_RURAL         2.301e-11  7.025e-12   3.275  0.00201 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.51 on 46 degrees of freedom
Multiple R-squared:  0.778,	Adjusted R-squared:  0.7635 
F-statistic: 53.72 on 3 and 46 DF,  p-value: 4.532e-15
```

![inverse02-lm](R_plots/01-model-building/inverse02-lm.png)

Again, it seems like the most trouble with fitting these models is the influence of outlier states.























