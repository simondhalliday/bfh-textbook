require(shape)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)

declining_share <- read_excel("capitalism/declining_share.xlsx")

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824", "#f0027f")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081", "#9e9ac8","#f0027f")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
COLD <- c("#DA3030","#41ae76","#F7DE04", "#4eb3d3","#AE82FF","#386cb0","#F48318","#FA328A", "#DA3030")

# Cleaning the wealth share data. Source: CORE
declining_share_gathered <- declining_share %>% 
  gather(Country, declining_share, c("Denmark", "France", "Germany", "Italy", "Japan", "Netherlands", "Sweden") )

declining_share1 <- declining_share_gathered[-c(1, 112, 223, 334, 445, 556, 667), ]

declining_share2 <- declining_share1 %>%
  mutate(Year = ifelse(Country == "Denmark", ...1, ifelse(Country == "France", ...3, ifelse(Country == "Germany", ...5, ifelse(Country == "Italy", ...7, ifelse(Country == "Japan", ...9, ifelse(Country == "Netherlands", ...11, ifelse(Country == "Sweden", ...13, NA))))))))

declining_share2$declining_share = as.numeric(as.character(declining_share2$declining_share))

declining_share3 <- declining_share2 %>%
  select(-c("...1", "...3", "...5", "...7", "...9", "...11", "...13")) %>%
  filter(!is.na(declining_share)) %>%
  mutate(declining_share_decimal = (declining_share)/100)

#Plotting the data
declining_share_plot <- ggplot(declining_share3, aes(x = Year, y = declining_share_decimal, group = Country, color = Country)) +
  geom_line() +
  ylab("Declining Income Share of the Top 1%") +
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.05), labels = scales::percent_format(accuracy = 1), limits = c(0,0.3)) +
  scale_x_continuous(breaks = seq(1900, 2010, by = 10)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.825, 0.81),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 21, vjust = 1),
        axis.title.x = element_text(size = 21, vjust = -1),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 17, angle = 0, color = "black"),
        axis.text.y = element_text(size = 17, color = "black")
  ) 


print(declining_share_plot) 

#Save plot to PDF
ggsave(declining_share_plot, filename = "declining_share_top_1percent.pdf",
       path = "capitalism",
       width = 9, height = 7, units = "in")


