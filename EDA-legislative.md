# State Goverment

State government data for the years 2014-2016. The **legislative branch** of each state's government generally comprises a State Sentate and State House of Representatives<sup>[1](#notes)</sup>, and generally these are referred to as the Upper and Lower Houses, respectively<sup>[2](#notes)</sup>. The **executive branch** of a state's government is headed by an elected Governor. 

Historical data on parties elected to state government were acquired from the National Conference of State Legislatures ([NCLS](http://www.ncsl.org/)), in the form of annual tables which were converted to CSVs using Tabula. 

Structure of the data:

```{R}
str(legislature)
'data.frame':	150 obs. of  14 variables:
 $ STATE        : Factor w/ 50 levels "Alabama","Alaska",..: 1 2 3 4 5 6 7 8 9 10 ...
 $ Total.Seats  : int  140 60 90 135 120 100 187 62 160 236 ...
 $ Total.Senate : int  35 20 30 35 40 35 36 21 40 56 ...
 $ Senate.Dem   : int  11 7 12 13 28 18 21 13 14 18 ...
 $ Senate.Rep   : int  23 13 17 22 11 17 14 8 26 38 ...
 $ Senate.other : Factor w/ 8 levels "0","1","1v","2",..: 2 1 3 1 3 1 3 1 1 1 ...
 $ Total.House  : Factor w/ 31 levels "100","101","105",..: 3 18 23 1 27 24 13 20 6 16 ...
 $ House.Dem    : chr  "37" "14" "24" "48" ...
 $ House.Rep    : chr  "66" "26" "36" "51" ...
 $ House.other  : Factor w/ 16 levels "","0","1","1, 1v",..: 4 2 2 3 2 2 2 2 7 3 ...
 $ Legis.Control: Factor w/ 4 levels "Dem","N/A","Rep",..: 3 3 3 3 1 1 1 1 3 3 ...
 $ Gov.Party    : Factor w/ 3 levels "Dem","Rep","Ind": 2 2 2 1 1 1 1 1 2 2 ...
 $ State.Control: Factor w/ 4 levels "Dem","Divided",..: 4 4 4 2 1 1 1 1 4 4 ...
 $ Year         : chr  "2014" "2014" "2014" "2014" ...
```

# State Governments by Party - 2014-2016

Given the legislative and executive branches of state government, 3 categories were presented in the data:

- Legislative Control - party which holds both Senate and House, or split
- Governing Party - party of the elected governor
- State Control - party which holds both Legislative Control and Governorship, or split

or 

- **Legislative Control** + **Governing Party** = **State Control**

![](R_plots/06-model-building-legislature/EDA-facet-bar-party-by-year.png)

Observations on the party totals over the years:

- generally - a receding of **Democratic** seats, and
- slight increase in **Divided** and **Split** seats.
- emergence of an  **Independent** seat.
- **Republican** seats remain relatively steady, with a slight increase from 2014 that carries over to 2016 (the _general_ length of term)

There might be a popular notion that Republicans are 'gaining' seats on Democrats, but the voting results show Democrats are losing seats to Independents, and within-state divisions are increasing - particularly arriving at 2016.

![](R_plots/06-model-building-legislature/legislative-facet-map-all.jpg)

**State Control** becomes an interesting category to look at on the map - from 2014 onwards the United States visually appears to be of 3 parties - this is superficial to some degree, as the third 'party' is simply states divided between Democrat and Republican. A stacked area chart gives another sense of these three 'parties' and the space they occupy:

![](R_plots/06-model-building-legislature/EDA-facet-bar-by-area.png)

# Political Control in 2014

2014 becomes a particular year of interest because the governing parties in place would feasibly be enacting legislation that would affect 2015-onwards. 

![](R_plots/06-model-building-legislature/legislative-map-2014.jpg)



# Notes
<sup>1</sup> Nebraska is the exception, having a unicameral legislative body.

<sup>2</sup> "State governments of the United States", [Wikipedia](https://en.wikipedia.org/wiki/State_governments_of_the_United_States).



