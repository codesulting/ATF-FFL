# House Disbursement Data - Exploratory

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(scales)

q3.16 <- fread("data/house-data/2016Q3-house-disburse-detail.csv",
               stringsAsFactors = T)
str(q3.16)
summary(q3.16)

# Plot themes -----------------------------------------------------------------

# theme_minimal() with type set in Gill Sans, and italic axis titles in Times

pd.theme <- theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)))

pd.classic <- theme_classic(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)))

# take a look at different variables

# Who is spending? ------------------------------------------------------------

summary(q3.16$OFFICE)
levels(q3.16$OFFICE)

# There are 523 offices from which expenditures were made. 
# Many (approx 2/3 or more) are individual Respresentatives in the House, 
# denoted by the abbrevaition "HON." preceding a given name.
# The rest are broad offices, groups, positions, and commitees, e.g.
# "COMMITTEE ON BENGHAZI", "HOMELAND SECURITY", "VETERANS' AFFAIRS", "OFFICE OF THE SPEAKER"

# What's the reason for spending? ---------------------------------------------

summary(q3.16$CATEGORY)
summary(q3.16$PURPOSE)

# CATEGORY is a high-level, broad variable of 10 levels.
# PURPOSE dives into finer grain, with details on spending over 2529 levels.

q3.category <- q3.16 %>% 
  group_by(CATEGORY) %>%
  count(CATEGORY) %>% 
  arrange(desc(n))

summary(q3.category$n)

q3.16 %>% 
  group_by(CATEGORY) %>%
  count(CATEGORY) %>% 
  arrange(desc(n)) %>%
  ggplot(aes(reorder(CATEGORY, n), n, fill = n)) +
  geom_bar(stat = "identity") + pd.theme +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "gray96", 
                       high = "firebrick4",
                       midpoint = 10000, guide = F) +
  theme(axis.text.x = element_text(angle = 45, size = 12,
                                   hjust = 1, vjust = 1)) +
  labs(title = "2016 Q3: House Office Expenditure Categories",
       x = "category", y = "", fill = "") +
  coord_flip()

# plot spending purpose, counts > 500
q3.16 %>%
  group_by(PURPOSE) %>%
  count(PURPOSE) %>%
  arrange(desc(n)) %>%
  filter(n > 500) %>%
  ggplot(aes(reorder(PURPOSE, n), n, fill = n)) +
  geom_bar(stat = "identity") + pd.theme +
  scale_fill_gradient2(low = "firebrick4", 
                       mid = "gray96", 
                       high = "deepskyblue4",
                       midpoint = 2000, guide = F) +
  theme(axis.text.x = element_text(size = 12)) +
  labs(title = "2016 Q3: House Office Expenditure Purposes (n > 500)",
       x = "purpose", y = "number of expenditures", fill = "") +
  coord_flip()

# Interesting - from the same dataset:
# 'Travel' is the largest spending CATEGORY,
# but 'Student Loans' makes up the largest spending PURPOSE.
# So! A clearer definition of these two variables is needed.
# But first, to round out spending purposes:

# plot spending purpose, counts < 500
q3.16 %>%
  group_by(PURPOSE) %>%
  count(PURPOSE) %>%
  arrange(desc(n)) %>%
  filter(n < 500 & n > 25) %>%
  ggplot(aes(reorder(PURPOSE, n), n, fill = n)) +
  geom_bar(stat = "identity") + pd.theme +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "gray96", 
                       high = "firebrick4",
                       midpoint = 400, guide = F) +
  theme(axis.text.x = element_text(size = 12)) +
  labs(title = "2016 Q3: House Office Expenditure Purposes (25 < n < 500)",
       x = "purpose", y = "number of expenditures", fill = "") +
  coord_flip()

# Some variables will need to be consolidated, e.g.
# `STUDENT LOAN PAYMT` is probably the same as 'STUDENT LOANS'


# Who is being paid? ----------------------------------------------------------

summary(q3.16$PAYEE)
summary(q3.16$`RECIP (orig.)`)

# both PAYEE AND `RECIP (orig.)` have 18450 levels. 
# 12462 fields are left blank, 
# meaning about 13% of payment recipients are unknown

12462/nrow(q3.16) # 0.1346443


# Who's been paid over 100 times? 12462 fields are left blank.
q3.16 %>%
  group_by(PAYEE) %>%
  count(PAYEE) %>%
  filter(n > 100 & n < 8000) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(PAYEE, n), n, fill = n)) + 
    geom_bar(stat = "identity") + pd.theme + 
    scale_fill_gradient2(low = "cadetblue4",
                         mid = "gray96",
                         high = "coral3",
                         midpoint = 2000, guide = F) +
    theme(axis.text.x = element_text(size = 12)) +
  labs(title = "2016 Q3: House Office Expenditures by Payee (n > 100)",
       x = "payee", y = "number of expenditures") +
  coord_flip()
  

# USPS and UPS are the two main ways of deliving things.
# Verizon is the most popular telnet service; 
# although might be tied with AT & T after aggregating. 
# Boise Cascade is the most popular paper or raw lumber supplier to the House. 

# Who's been paid under 100 times? 12462 fields are left blank.
q3.16 %>%
  group_by(PAYEE) %>%
  count(PAYEE) %>%
  filter(n < 100 & n > 25) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(PAYEE, n), n, fill = n)) + 
  geom_bar(stat = "identity") + pd.theme + 
  scale_fill_gradient2(low = "coral4",
                       mid = "gray96",
                       high = "cadetblue4",
                       midpoint = 75, guide = F) +
  theme(axis.text.y = element_text(size = 7.5),
        axis.title.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 0, 0, 0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), unit = "cm")) +
  labs(title = "2016 Q3: House Office Expenditures by Payee (25 < n < 100)",
       x = "", y = "") +
  coord_flip()
    
# A glance at the list shows that 'CITI' has multiple, more specific subcategories.
# Might be worth quickly parsing the text of the of the PAYEE field;
# or at least examining the Citibank payments. 

# Here also appears direct payments to House representatives ('HON')

# Who is using Photoshop?? 
# ('CITI PCARD - ADOBE CREATIVE CLOUD', 'CIIT PCARD - ADOBE PHOTOGRAPHY PLAN')

# Another: 'DEPT OF EDUCATION'

q3.16 %>%
  group_by(PAYEE) %>%
  count(PAYEE) %>%
  filter(n < 10) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(PAYEE, n), n, fill = n)) + 
  geom_bar(stat = "identity") + pd.theme + 
  scale_fill_gradient2(low = "coral4",
                       mid = "gray96",
                       high = "cadetblue4",
                       midpoint = 75, guide = F) +
  theme(axis.text.y = element_text(size = 7.5),
        axis.title.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 0, 0, 0)),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), unit = "cm")) +
  labs(title = "2016 Q3: House Office Expenditures by Payee (10 < n < 50)",
       x = "", y = "") +
  coord_flip()


citibank <- q3.16 %>% 
  select(OFFICE, CATEGORY, PAYEE, AMOUNT) %>%
  mutate(citi = grepl("CITI PCARD", q3.16$PAYEE)) %>%
  filter(citi == T)

summary(citibank)

# How much is being spent? ----------------------------------------------------

summary(q3.16$AMOUNT)



# create a smaller dataframe to work from
q3.small <- q3.16 %>%
  select(OFFICE, CATEGORY, PURPOSE, PAYEE, AMOUNT) %>%
  group_by(CATEGORY)

