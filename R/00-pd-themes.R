# Custom Themes
# simple modifications to ggplot2 themes:
# fonts, sizes, margins

library(ggplot2)

pd.theme <- theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)))

pd.scatter <- theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
        axis.line = element_line(color = "black"))

pd.classic <- theme_classic(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)))

pd.facet <- theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
        panel.background = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(fill = NA, color = "black"))
