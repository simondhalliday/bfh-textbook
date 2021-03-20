library(readxl)
library(ggplot2)
library(ggExtra)

OBP_JEBO <- read_excel("indmarketdemand/OBP_JEBO.xlsx")

colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                      "#0072B2", "#D55E00", "#CC79A7", "#999999", "#D55E00", "#000000")

OBP_plot <-
  ggplot(OBP_JEBO, aes(x = lnp99_new, y = hour, color = OBP_JEBO$name)) +
  geom_point() +
  labs(x = "Income inequality",
       y = "Annual average work hours",
       color = "Country") +
  annotate("text", x = 3, y = 3300, label = "Sweden, 1900", size = 4) +
  annotate("text", x = 1.7, y = 1623, label = "Sweden, 2000", size = 4) +
  annotate("text", x = 2.8, y = 2929, label = "Netherlands, 1913", size = 4) +
  annotate("text", x = 1.7, y = 1347, label = "Netherlands, 2000", size = 4) +
  scale_color_manual(values = colors, name = "Country", labels = c("AUS" = "Australia","CAN" = "Canada","FRA" ="France","GER"="Germany","JPN" = "Japan","NET" = "Netherlands","SWE"="Sweden","SWI" = "Switzerland","UK"= "UK", "US"="US")) +
  theme_bw()  + 
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        axis.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 13), 
        legend.position = "top", 
        legend.box="vertical")
OBP_plot
# OBP_plot_year <-
#   ggplot(OBP_JEBO, aes(x = lnp99_new, y = hour, color = as.factor(year), label = name)) +
#   #geom_point() +
#   geom_text(aes(label=name),hjust=0, vjust=0) + 
#   labs(x = "Income Inequality",
#        y = "Annual Average Work Hours",
#        color = "Year") +
#   scale_color_manual(values = colors) +
#   theme_minimal()

ggsave("indmarketdemand/OBP_plot.pdf", OBP_plot, width = 8, height = 6)
#ggsave("OBP_plot_year.pdf", OBP_plot_year, width = 6, height = 4)