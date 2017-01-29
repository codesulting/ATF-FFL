# House of Representatives - Expenditures

The United States House of Representives receives a spending budget annually, and each member decides how they'd like to use those funds. 

Each quarter the data is reported by the House in PDF form, and the Sunlight Foundation/ProPublica parse this into [downloadable CSV files](https://projects.propublica.org/represent/expenditures). 

What follows below is initial exploratory looks at House Expenditure data, for the third quarter of 2016, and here is the [R Script](R/house-disburse-EDA-01.R) used for exploration (_in progress_).

## 2016 - Quarter 3

Glimpse of the data:

![glimpse](http://pi.mozzarella.website/ATF-FFL/q3-2016-HouseGlimpse.png)

There's a total of 92555 observations of 15 variables:



### Who is spending? 

There are 523 offices from which expenditures were made.

Many (approx 2/3 or more) are individual Respresentatives in the House, 
denoted by the abbrevaition "HON." preceding a given name.

The rest are broad offices, groups, positions, and commitees, e.g.
"COMMITTEE ON BENGHAZI", "HOMELAND SECURITY", "VETERANS' AFFAIRS", "OFFICE OF THE SPEAKER"

### What is the reason for spending? 

`CATEGORY` is a high-level, broad variable of 10 levels.
`PURPOSE` dives into finer grain, with details on spending over 2529 levels.

![Categories](http://pi.mozzarella.website/ATF-FFL/q3-2016-Categories.png)

With many more levels, `PURPOSE` here is broken down into counts over- and under- 500. 

![PurposesOver500](http://pi.mozzarella.website/ATF-FFL/q3-2016-PurposeOver500.png)

![PurposeUnder500](http://pi.mozzarella.website/ATF-FFL/q3-2016-PurposeUnder500.png)

### Who is being paid? 

Both `PAYEE` AND `RECIP (orig.)` have 18450 levels. TODO: test to see if identical. 

12462 fields are left blank, meaning about 13% of payment recipients are potentially unknown.

Recurring Payees - over 100 instances: 

![PayeeOver100](http://pi.mozzarella.website/ATF-FFL/q3-2016-PayeeOver100.png)

The Citibank Government Card Service is an outlier in counts. TODO: check how many members of the House use this service. 

A glance at the list shows that 'CITI' has multiple, more specific subcategories.Might be worth quickly parsing the text of the of the PAYEE field; or at least examining the Citibank payments. 

Here also appears direct payments to House representatives `('HON')`

Who is using Photoshop?? 

_(`'CITI PCARD - ADOBE CREATIVE CLOUD'`, `'CIIT PCARD - ADOBE PHOTOGRAPHY PLAN'`)

Another obseravation to potentially aggregate: `'DEPT OF EDUCATION'`

Recurring Payees - between 25 and 100 instances:

![PayeeUnder100](http://pi.mozzarella.website/ATF-FFL/q3-2016-PayeeUnder100.png)


### What's being charged to the Citibank card?

```{r}
citibank <- q3.16 %>% 
  select(OFFICE, CATEGORY, PAYEE, AMOUNT) %>%
  mutate(citi = grepl("CITI PCARD", q3.16$PAYEE)) %>%
  filter(citi == T)

summary(citibank)
```









