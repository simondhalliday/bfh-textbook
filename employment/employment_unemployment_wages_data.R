library(ggplot2)
library(ggrepel)
library(scales)
library(RColorBrewer)

#Import data
wageunions <- read.csv("employment/unions_data_wages.csv")
#Set color palette
COL <- brewer.pal(3, "Set1")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Plot initial data with ggplot
u1 <- ggplot(wageunions,aes(x = UnemploymentAvg, y = RealWageGrowth, 
                            color = factor(Performance), 
                            shape = factor(Performance)))
#Add bells and whistles to plot
u1 + 
  geom_point(size = 3) + 
  coord_cartesian(xlim = c(0, 18), ylim = c(0, 4.5)) +
  labs(x = "Average unemployment rate (1970-2011)",
       y = "Compound average wage rate growth (1970-2011)") + 
  # xlab("Average Unemployment Rate (1970 - 2011)") + 
  # ylab("Compound Average Wage Rate Growth (1970 - 2011)") + 
  scale_color_brewer(palette="Set1", name = "Performance", 
                     labels = c("Low", "Average", "High")) +
  scale_shape_discrete(name = "Performance", 
                       labels = c("Low", "Average", "High")) + 
  # annotate("text", x = 14, y = 1.25, label = "Low", color = COL[1], size = 6) + 
  # annotate("text", x = 14, y = 1, label = "Performers", color = COL[1], size = 6) + 
  # annotate("segment", x = 2.5, xend = 13, y = 0.2, yend = 3.5, color = COL[1], lty = 2) + 
  # annotate("text", x = 10, y = 3.75, label = "Average", color = COL[2], size = 6) + 
  # annotate("text", x = 10, y = 3.5, label = "Performers", color = COL[2], size = 6) + 
  # annotate("text", x = 2.5, y = 3.75, label = "High", color = COL[3], size = 6) +
  # annotate("text", x = 2.5, y = 3.5, label = "Performers", color = COL[3], size = 6) +
  # annotate("segment", x = 1, xend = 9, y = 0.5, yend = 4.5, color = COL[3], lty = 2) + 
  annotate("text", x = 12, y = 1.8, label = "Low", color = COL[1], size = 7) +
  annotate("text", x = 12, y = 1.6, label = "performers", color = COL[1], size = 7) +
  #annotate("segment", x = 2.5, xend = 13, y = 0.2, yend = 3.5, color = grays[20], lty = 2) +
  annotate("text", x = 9.7, y = 3.7, label = "Average", color = COL[2], size = 7) +
  annotate("text", x = 9.7, y = 3.5, label = "performers", color = COL[2], size = 7) +
  annotate("text", x = 2.7, y = 3, label = "High", color = "#3A9519", size = 7) +
  annotate("text", x = 2.7, y = 2.8, label = "performers", color = "#3A9519", size = 7) +
  #annotate("segment", x = 1, xend = 9, y = 0.5, yend = 4.5, color = grays[20], lty = 2) +
  geom_text_repel(aes(label = Country), size = 6,point.padding = 0.09, show.legend = FALSE, color = "black") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 19), 
        axis.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))

ggsave("employment/employment_unemployment_wages_data.pdf", width = 9, height = 7, units = "in")

