# Firearms Freedom Act

Certain states introduced legislation through a specific interpretation of a 'commerce clause' in the 10th Amendment. Called the Firearms Freedom Act (FFA), it argued that firearms manufactured and kept in-state were beyond Federal regulation - citing Congressional interference on within-state commerce<sup>[1](#Notes), [2](#Notes)</sup>.

Montana was the first to introduce this bill in 2009, the first to have the bill challenged, and the only state to have the bill struck down in court. 

Data on this act and variants of it from state to state were acquired from [gunwars.news21](http://gunwars.news21.com/interactives/nullification.html).


#### Linear Maximal Model

```{R}
Call:
lm(formula = perCapitaFFL ~ . - NAME, data = ffa)

Residuals:
    Min      1Q  Median      3Q     Max 
-31.502 -11.532  -0.856   7.763  52.237 

Coefficients: (1 not defined because of singularities)
                   Estimate Std. Error t value Pr(>|t|)   
(Intercept)        -28.4662    17.7529  -1.603  0.11599   
ffa.introduced T    61.7798    18.1893   3.396  0.00146 **
ffa.enacted T       21.8009     7.7398   2.817  0.00724 **
ffa.strenghtened T   6.3626    17.5848   0.362  0.71922   
no.action T         53.4244    17.5848   3.038  0.00400 **
nullified T              NA         NA      NA       NA   
ATF.Region          -0.5253     0.9466  -0.555  0.58174   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 16.21 on 44 degrees of freedom
Multiple R-squared:  0.4943,	Adjusted R-squared:  0.4368 
F-statistic:   8.6 on 5 and 44 DF,  p-value: 9.596e-06
```

The introduction of the FFA bill appears to be the strongest predictor for per capita FFLs - followed by states taking no action on the bill, and states that enact the bill. 

![](R_plots/net-international-migration/ffa-lm-01.png)





# Notes

<sup>1</sup> Montana Firearms Freedom Act, [Wikipedia article](https://en.wikipedia.org/wiki/Montana_Firearms_Freedom_Act)

<sup>2</sup> Firarms Freedom Act [webpage](http://firearmsfreedomact.com/)