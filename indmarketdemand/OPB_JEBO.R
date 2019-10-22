library(readxl)
library(ggplot2)
library(ggExtra)

OBP_JEBO <- read_excel("Desktop/OBP_JEBO.xlsx")

colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                      "#0072B2", "#D55E00", "#CC79A7", "#999999", "#D55E00", "#000000")

OBP_plot <-
  ggplot(OBP_JEBO, aes(x = lnp99_new, y = hour, color = OBP_JEBO$name)) +
  geom_point() +
  labs(x = "Income Inequality",
       y = "Annual Average Work Hours",
       color = "Country") +
  annotate("text", x = 3.2, y = 3300, label = "Sweden, 1900", size = 3) +
  annotate("text", x = 1.7, y = 1623, label = "Sweden, 2000", size = 3) +
  annotate("text", x = 3.0, y = 2929, label = "Netherlands, 1913", size = 3) +
  annotate("text", x = 1.7, y = 1347, label = "Netherlands, 2000", size = 3) +
  scale_color_manual(values = colors) +
  theme_minimal()

# OBP_plot_year <-
#   ggplot(OBP_JEBO, aes(x = lnp99_new, y = hour, color = as.factor(year), label = name)) +
#   #geom_point() +
#   geom_text(aes(label=name),hjust=0, vjust=0) + 
#   labs(x = "Income Inequality",
#        y = "Annual Average Work Hours",
#        color = "Year") +
#   scale_color_manual(values = colors) +
#   theme_minimal()

ggsave("OBP_plot.pdf", OBP_plot, width = 6, height = 4)
#ggsave("OBP_plot_year.pdf", OBP_plot_year, width = 6, height = 4)