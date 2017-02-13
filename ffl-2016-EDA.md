# What is a Federal Firearms License?

In the United States, a Federal Firearms License (FFL) is a requirement for those who engage in the business of firearms - generally **dealers**, **manufacturers**, and **importers**. It's not actually a license to carry a firearm; it's strictly for the conducting business involving firearms. It's not necessary to have one if selling at gun shows, or when purchasing guns for personal reasons. 

The ATF considers 9 __types__ of FFLs: 

- Dealer
- Pawnbroker
- Collector
- Manufacturer of Ammunition
- Manufacturer of Firearms
- Dealer in Destructive Devices
- Manufacturer of Destructive Devices
- Importer of Destructive Devices

'Destructive Devices' constitute their own class, and are [defined by the National Firearms Act](https://www.atf.gov/firearms/firearms-guides-importation-verification-firearms-national-firearms-act-definitions-1), [26 U.S.C. § 5845(F) (page 82)](https://www.atf.gov/firearms/docs/guide/atf-guidebook-importation-verification-firearms-ammunition-and-implements-war/download). Three examples, as shown in an illustration from the ATF Guidebook:

![destructive devices](indd/assets/dd-definition.png)

The ATF bullet points defining Destructive Device are: 

- A missile having an explosive or incendiary charge of more than 1/4 oz. (**1/4 oz explosive/incendiary**)
- Any type of weapon by whatever name known which will, or which may readily be converted to expel a projectile, by the action of an explosive or other propellant, the barrel or barrels of which have a bore greater than one-half inch in diameter. (**1/2" bore**)
- A combination of parts designed and intended for use in converting a device into a destructive device and from which a destructive device can be readily assembled.

Back to FFLs - the ATF [publishes data on this FFL holders](https://www.atf.gov/firearms/listing-federal-firearms-licensees-ffls-2016) monthly, from 2013 to present. Additionally, an [annual commerce report](https://www.atf.gov/resource-center/data-statistics) is released, which contains numbers on weapons registrations, imports and taxes, and historical FFL data. 

# ATF - Federal Firearms Licenses 2016

Here's a walkthrough of initial exploratory plots and analysis for Federal Firearms Licenses in 2016. 

```{r}
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

f16 <- fread("data/ffl-2016-V3.csv", stringsAsFactors = T)
f16 <- as.data.frame(f16)
```

## License Count by State

So the first broad question that comes to mind: Which states had the most firearms licenses? A license count variable for each state was computed while munging.

```{r}
# Broadly: which states had the most firearms licenses? -----------------------

summary(f16$LicCount)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      20   14630   20580   25220   29630   75960
```
![count by state 01](R_plots/2016-LicCountByState.png)

Texas appears to have twice as many license holders than Florida - but it is also the largest state in the continental US. A state's total land area might be misleading here; although at the lower end of scale are where much smaller states such as Rhode Island and Delaware appear. 

Also what might be misleading is that the above plot is for _raw counts_, that doesn't take into account each state's population. After binding US Census population data for each state, license count can be normalized **per 100,000 residents**. This shows a different picture of FFL holders across the states (population mapped to color fill):

![per capita count by state](R_plots/2016-FFL-byState-perCapitaPopulation.png)

- Texas, while leading the raw count in FFLs, has less than 25 FFLs per capita. 
- Wyoming, with the lowest population in the US, has over 100 FFLs per 100,000 residents. 

Alaska (3rd least populous) along with North and South Dakota (4th and 5th least populous) appear to be part of a trend of more FFLs despite lower population.

Broadly speaking, it appears that the most populous states have less firearms license holders per capita than the least populous states.

![mapbar01](R_plots/mapbar-2016-FFL-by-State.jpg)

![mapbar02](R_plots/mapbar-2016-pop-by-State-desc.jpg)

Looking at these maps...it's tempting to want to say that the number of Fedeally Licensed Firearms dealers is inversely proportional to a state's population. We can create two rank variables to specifically look at this possibility. Or - put FFLs and population on a scatterplot, and possibly fit an exploratory linear regression if there appears to be a pattern.

![pop-ffl scatterplot](R_plots/Population-FFL-01.png)

Looking at the scatterplot of FFLs per 100k residents vs population...it is still tempting to see an inverse relationship between the two. Why would a higher population have less FFLs? It can also be noted that there's possibly 3 very general clusters that form on the scatterplot - the most populous states, least populous states, and states in general (which very much looks like would follow f(x) = 1/x to me.) 

How does this look on a log scale? 

![pop-ffl scatterplot (log)](R_plots/log-Population-FFL-01.png)

And with fitted values from `lm(FFL.rate ~ Population)`?

![percapFFL-pop](R_plots/perCapitaFFL-RawPop.png)

![perCapFFL-log](R_plots/perCapitaFFL-Pop-log.png)

**TODO**: compare urban density across US to FFL density.

# Rural to Urban Proportions

According to the U.S. Census:

- Urbanized Areas are defined as having a population of over 50,000.
- Urban Clusters have a population 5,000 < n < 50,000.
- Rural Areas have a population less than 5,000.

Further, there appears again to be an inverted relationship between Land Area and Population when looking at Urban vs Rural Areas - i.e., while Urban Areas comprise only 3 percent of United States Land Area, they also count for 80.7 percent of the population. Rural Areas, by contrast, make up 97% of US Land Area while only 19.3 percent of the population.<sup>[2](#works-cited)</sup> 

Given the observation of an inverse relationship between FFLs vs Population by state, what can be learned from differences in rural- and urban-defined areas in the United States in regard to FFLs? 

After combining rural-urban proportions data with per capita FFL data, we can look for correlation amonst the variables. 



## License Count By Month

Was there much variance in the number of licenses in each state, from month to month? How does this look specifically, from 2015 to 2016? 

![Lic Count Total by Month 2015-16](R_plots/2015-16-LicTotalByMonth.png)

# FFL History: 1975-2015

By the 9 Types of FFLs as defined by the ATF, how have the counts changed from 1975 to 2015? **'dd'** stands for Destructive Device, **'mfg'** for manufacturer.

![FFL-History-02](R_plots/FFL-History-02.png)

- Looking specifically at dd's, the number has increased heavily.
- Manufacturers of Ammo have gone down dramatically
- around 2010, manufacturers of firearms began to increase steadily.
- peak of all FFL types happened in the early 1990s.


# Edit

The sections below are likely to be edited out.

## License Count By Month

Was there much variance in the number of licenses in each state, from month to month?

![Lic by Month](R_plots/2016-LicCountMonthly.png)

## License Count by Region

Are there any trends by region? Could state population be tell us more about license holders? 

(For whatever reason, it's difficult to find the ATF's Region codes online, but the states belonging to each region can be found by simply filtering the data.)

![count by state by region](R_plots/2016-CountByRegion.png)

So most regions have less than 40,000 licenses per state, but appear _generally_ similar. Region 8 has just 4 states and a low number of licenses, other than Pennsylvania. Region 6 appears to have the lowest number of licenses by region overall; most states here have less than 10,000, with the outlier of New York at about 20,000. Perhaps a summary of each region by state would be easier to read than this plot. 


# Why Own a Gun?

_an examination of ATF Federal Firearms License data_

Protection, Recreation, and Entitlement: these are 3 reasons commonly cited for owning a firearm in the United States. A Pew Research poll<sup>[1](#works-cited)</sup> actually asked the question "Why Own a Gun?", and their survey shows an interesting figure in people's primary reasons for doing so. 

- In 1999, 49% of those surveyed cited 'Hunting' as a primary reason, and 26% claimed 'Protection'. 
- In 2013, 48% claimed 'Protection', while 32% cited 'Hunting' as a primary reason. 

The question of _Why?_ is beyond the direct scope of Federal Firearms License data alone; FFL covers not individual owners but rather dealers, manufacturers, and suppliers of firearms. However, by looking at patterns and outliers in federally licensed firearm trade...

# Works Cited

<sup>1</sup> ["Why Own a Gun? Protection Is Now Top Reason"](http://www.pewresearch.org/daily-number/why-own-a-gun-protection-is-now-top-reason/). Pew Research. May 9th, 2013.

<sup>2</sup> ["Life Off the Highway: A Snapshot of Rural America"](http://blogs.census.gov/2016/12/08/life-off-the-highway-a-snapshot-of-rural-america/). U.S. Census Bureau. Dec 8th, 2016. 










