# Immigration by State

After fitting a regression tree model to all features (education, income, industry, legislative, and population) - `Net International Migration` was repeatedly chosen as the top split. The US Census calculates an annual estimate on Net International Migration, taking into account several migration factors: 

- foreign-born immigration
- foreign-born emigration
- migration between the US and Puerto Rico
- native-born migration
- movement of armed forces to and from overseas

Although the Census seeks to capture the total international migration flow for the US (coming and going), `Net International Migration` is comprised mostly of **_Foreign-Born Immigrants._**

So - is a distinctly _American_ characteristic to have plentiful, ready access to firearms? The question is somehat tongue-in-cheek - but what does it mean if a general trend shows that there are _**more Federal Firearms Licenses**_ when there are _**less Foreign-Born Immigrants**_ in a given state? 

Of course, there are many potential questions that come into play when thinking about where foreign-born immigrants might choose to settle in America. Is there an international airport? Is there family, or a community of immigrants, at the destination? Are there opportunities for work, or education? How's the weather there?

# Immigrants make up America, and Firearms Prove It

```{R}
# Single Linear Regressions on Immigration Variables --------------------------

im01 <- tidy(lm(perCapitaFFL ~ y2014, data = immigration.sub))
im02 <- tidy(lm(perCapitaFFL ~ INTERNATIONALMIG2014, data = immigration.sub))
im03 <- tidy(lm(perCapitaFFL ~ NATURALINC2014, data = immigration.sub))
im04 <- tidy(lm(perCapitaFFL ~ DEATHS2014, data = immigration.sub))

single.immigrations <- bind_rows(im01, im02, im03, im04) %>%
  filter(term != "(Intercept)") %>%
  arrange(p.value)

single.immigrations
#                       term      estimate    std.error statistic      p.value
#      1                y2014 -0.083914355 0.01698826 -4.9395510 0.00000990162
#      2 INTERNATIONALMIG2014 -0.085035288 0.01760288 -4.8307589 0.00001431094
#      3       NATURALINC2014  0.013249795 0.01313756  1.0085432 0.31825098157
#      4           DEATHS2014  0.009657999 0.02468805  0.3912014 0.69737952716
```

Linear regression on all variables: 

```{R}
im.all <- lm(perCapitaFFL ~ .-.rownames, data = immigration.sub)

Residuals:
    Min      1Q  Median      3Q     Max 
-25.360 -11.082  -0.076   4.788  56.362 

Coefficients: (2 not defined because of singularities)
                     Estimate Std. Error t value Pr(>|t|)   
(Intercept)          67.67139   41.71699   1.622  0.11208   
y2014                -0.08715    0.04294  -2.029  0.04863 * 
NPOPCHG_2014          0.26289    0.08812   2.983  0.00469 **
BIRTHS2014           -0.25976    0.09350  -2.778  0.00807 **
DEATHS2014            0.24133    0.09804   2.462  0.01792 * 
NATURALINC2014             NA         NA      NA       NA   
INTERNATIONALMIG2014 -0.27605    0.09451  -2.921  0.00554 **
DOMESTICMIG2014      -0.27419    0.09060  -3.026  0.00417 **
NETMIG2014                 NA         NA      NA       NA   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 15.88 on 43 degrees of freedom
Multiple R-squared:  0.5258,	Adjusted R-squared:  0.4597 
F-statistic: 7.948 on 6 and 43 DF,  p-value: 0.000008533
```


Check the weights on the robust model:
```{R

im.weights <- data.frame(.rownames = rownames(immigration.sub),
                         resid = im.rr01$resid, 
                         weight = im.rr01$w) %>% arrange(weight)
        .rownames       resid    weight
im.weights
1         Montana  60.8211994 0.2367252
2         Wyoming  56.4501523 0.2550592
3         Vermont  26.0488879 0.5527580
4  South Carolina -22.5599899 0.6382401
5        Delaware -18.2512721 0.7889027
6    North Dakota  15.2450262 0.9443245
7     Mississippi -14.5733669 0.9880110
8        Illinois -14.4509294 0.9963026
...
```

A second robust regression model to that includes `NATURALINC2014`, since it was omitted before due to non-convergence. 

```{R}

im.rr02 <- rlm(perCapitaFFL ~ NATURALINC2014, data = immigration.sub)
summary(im.rr02)
im.weights.02 <- data.frame(.rownames = rownames(immigration.sub),
                         resid = im.rr02$resid, 
                         weight = im.rr02$w) %>% arrange(weight)
                         
im.weights.02
        .rownames       resid    weight
1         Montana  76.3345383 0.2728827
2         Wyoming  76.1026363 0.2737204
3          Alaska  51.0837288 0.4078084
4    South Dakota  30.6777278 0.6790565
5           Idaho  27.8466551 0.7480970
6    North Dakota  25.3603400 0.8214552
7      New Jersey -24.1293527 0.8632591
8         Vermont  23.1802380 0.8985715
9   West Virginia  23.0006629 0.9055485
10     California -22.6568282 0.9192821
11         Hawaii -21.4793167 0.9696965
```

```{R}
im.rr03 <- rlm(perCapitaFFL ~ y2014 + NATURALINC2014 + INTERNATIONALMIG2014,
                data = immigration.sub)
summary(im.rr03)

im.weights03 <- data.frame(.rownames = rownames(immigration.sub),
                           resid = im.rr03$resid,
                           weight = im.rr03$w) %>% arrange(weight)

im.weights03
        .rownames       resid    weight
1         Montana  63.3264342 0.2407006
2         Wyoming  63.1899716 0.2412230
3          Alaska  46.6689818 0.3266281
4    South Dakota  22.8553538 0.6669599
5    North Dakota  20.4217810 0.7464383
6         Vermont  18.2786684 0.8338940
7           Idaho  17.7542369 0.8585873 
```






