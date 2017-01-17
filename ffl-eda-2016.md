# ATF - Federal Firearms Licenses 2016

Here's a walkthrough of initial exploratory plots and analysis for Federal Firearms Licenses in 2016. 

```{r}
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(scales)

f16 <- fread("data/ffl-2016-V2.csv", stringsAsFactors = T)
f16 <- as.data.frame(f16)
```

So the first broad question that comes to mind: Which states had the most firearms licenses? A license count variable for each state was computed while munging.

```{r}
# Broadly: which states had the most firearms licenses? -----------------------

summary(f16$LicCount)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      20   14630   20580   25220   29630   75960

# A look at license count by state
ggplot(f16, aes(reorder(PremiseStateFull, LicCount), fill = LicCount)) + 
  geom_bar() +
  scale_fill_gradient2(low = "deepskyblue2", mid = "antiquewhite2", high = muted("firebrick3"),
                       midpoint = 25220) +
  scale_y_discrete(limits = c(0, 5000, 10000, 20000, 40000, 80000)) +
  theme_minimal(base_size = 13.75, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.x = element_text(family = "Times", face = "italic", size = 12,
                                    margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 12.75, angle = 30, hjust = 1, vjust = 1),
        panel.grid.major = element_line(color = "gray96")) +
  labs(title = "2016: Federal Firearms Licenses by State",
       y = "number of licenses", x = "", fill = "") +
  coord_flip()
```
![count by state 01](plots/2016-LicCountByState.png)

Texas appears to have twice as many license holders than Florida - but it is also the largest state in the continental US. Square mileage per state might be misleading here; but what about each state's population? 




