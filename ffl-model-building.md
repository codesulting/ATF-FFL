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

Going by `p.value`, the most significant explanatory variables in this case would be Urban Cluster Population Percentage (`POPPCT_UC`) and Rural Land Area (`AREA_RURAL`). 

How does the R-squared look? 

```{R}
glance(rural.urban.01)[, c(1:5, 8, 10)]
# r.squared   adj.r.squared    sigma statistic      p.value      AIC deviance
# 1 0.8333481     0.8144104 9.306224  44.00469 4.927771e-16 372.5705 3810.656
```












