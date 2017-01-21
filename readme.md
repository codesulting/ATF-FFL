
## Entitlement, Protection, and Recreation

_Why Own a Gun?_ An examination of ATF Federal Firearms License data.

Initial notes on a broad question, in an attempt to understand an issue, describe particular data, and note correlations across other data. Who, What, When, and Where are easily answered by summary statistics in this case; can these fields provide any clues as to _Why?_

![raw notes 01](http://pi.mozzarella.website/ATF-FFL/ffl-domain-notes.jpg)

## Data - Federal Firearms Licenses

The Bureau of Alcohol, Tobacco, Firearms, and Explosives ([ATF](https://www.atf.gov/)) maintains [listings of Federal Firearms licensees](https://www.atf.gov/firearms/listing-federal-firearms-licensees-ffls-2016) online, dating back to 2013. This detailed data comes in the form of .xlsx or .txt files.


Other notes:

- 2016: .pdf for 'FFL Type by State' for April is actually for Explosives, not Firearms
- 2015: no data for September, October (no explanation)
- 2014: January through May, and September are missing Expiration variable
- 2013: no data for October (government shutdown)

Glimpse of the data:

![June 2016 ATF data](http://pi.mozzarella.website/ATF-FFL/ffl-2016-glimpse-01.png)

## Variables Provided

- Region
- District
- County
- Firearm Type
- Expiration
- Seqn
- License Name
- Business Name
- Premise Street, City, State, ZIP
- Mailing Street, City, State, ZIP
- Phone number

Further Breakdowns:
- Region

|  Region  |      States     							   |
|----------|:---------------------------------------------:|
|    1	   | AL, FL, GA, MS, NC, SC, TN, VA
|    3     | IL, MN, ND, SD, WI  
|    4     | IN, KY, MI, OH, WV
|    5     | AR, CO, IA, KS, LA, MO, NE, NM, OK, TX, WY
|    6     | CT, MA, ME, NH, NY, RI, VT
|    8     | DE, MD, NJ, PA
|    9     | AK, AZ, CA, HI, ID, MT, NV, OR, UT, WA

## Variables Derived

- year
- month
- License holder count
- State population
- Latitude and Longitude coordinates

## Variables to derive

Total cost of Licenses by Type can be calculated as:
- 01 - $200 - Dealer (01), Including Pawnbroker (02), in Firearms Other Than Destructive Devices (Includes: Rifles, Shotguns, Pistols,
Revolvers, Gunsmith activities and National Firearms Act (NFA) Weapons)
- 02 - $200 - see above (pawnbroker)
- 06 - $30 - Manufacturer of Ammunition for Firearms Other Than Ammunition for Destructive Devices or Armor Piercing Ammunition
- 07 - $150 - Manufacturer of Firearms Other Than Destructive Devices
- 08 - $150 - Importer of Firearms Other Than Destructive Devices or Ammunition for Firearms Other Than Destructive Devices, or
Ammunition Other Than Armor Piercing Ammunition
- 09 - $3000 - Dealer in Destructive Devices
- 10 - $3000 - Manufacturer of Destructive Devices, Ammunition for Destructive Devices or Armor Piercing Ammunition
- 11 - $3000 - Importer of Destructive Devices, Ammunition for Destructive Devices or Armor Piercing Ammunition

