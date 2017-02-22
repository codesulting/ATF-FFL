# Custom Themes
# simple modifications to ggplot2 themes:
# fonts, sizes, margins

library(ggplot2)

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

pd.pal <- c("deepskyblue4", "antiquewhite1", "firebrick4")
