# ACS Income Regressions

```{R}
mod.00 <- lm(perCapitaFFL ~ ., incomePerCapita)
summary(mod.00)

Call:
lm(formula = perCapitaFFL ~ ., data = incomePerCapita)
Residuals:
#     Min      1Q  Median      3Q     Max 
# -24.971  -7.088  -2.324   4.376  46.665 

Coefficients:
                          Estimate Std. Error t value Pr(>|t|)    
(Intercept)              73.524379  48.622204   1.512 0.138767    
perCapita.LessThan5000   -0.066995   0.018208  -3.679 0.000722 ***
perCapita.5000to9999      0.029504   0.020510   1.439 0.158478    
perCapita.10000to14999   -0.013507   0.017298  -0.781 0.439752    
perCapita.15000to19999   -0.017010   0.020492  -0.830 0.411677    
perCapita.20000to24999    0.048891   0.022378   2.185 0.035139 *  
perCapita.25000to34999    0.020788   0.020244   1.027 0.310957    
perCapita.35000to49999   -0.013768   0.014266  -0.965 0.340594    
perCapita.50000to74999   -0.011191   0.010193  -1.098 0.279165    
perCapita.75000to99999   -0.013470   0.011944  -1.128 0.266492    
perCapita.100000to149999  0.033937   0.008343   4.068 0.000231 ***
perCapita.150000.or.more -0.021080   0.004261  -4.947 1.57e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 14.52 on 38 degrees of freedom
Multiple R-squared:  0.6497,	Adjusted R-squared:  0.5483 
F-statistic: 6.407 on 11 and 38 DF,  p-value: 7.112e-06
```

## Robust on all
```{R}
income.01 <- rlm(perCapitaFFL ~ ., data = incomePerCapita)
summary(income.01)

# check weights
income.huber01 <- data.frame(.rownames = income$NAME, 
                             .resid = income.01$resid,
                             weight = income.01$w) %>% arrange(weight)
                             
income.huber01
# .rownames      .resid    weight
1         Montana  57.9578355 0.1677200
2          Alaska  41.9405974 0.2317900
3    South Dakota  26.6799040 0.3643113
4         Wyoming  22.6669341 0.4289518
5       Wisconsin -19.1931400 0.5064662
6     Connecticut  15.3394775 0.6337042
7        Nebraska -14.7099159 0.6607946
8          Hawaii -14.2274766 0.6831173
9         Indiana -11.6987166 0.8308687
10      Minnesota -10.8123890 0.8989951
11        Arizona  10.2464510 0.9487217
```


