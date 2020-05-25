require(shape)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)

wealth_share <- read_excel("capitalism/top_1percent_wealth_share.xlsx")

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824", "#f0027f")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081", "#9e9ac8","#f0027f")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
COLD <- c("#DA3030","#41ae76","#F7DE04", "#4eb3d3","#AE82FF","#386cb0","#F48318","#41ae76","#6a51a3", "#DA3030")

#Cleaning the wealth share data. Source: CORE
wealth_share_gathered <- wealth_share %>% 
  gather(Country, Wealth_share, c("Denmark", "Finland", "France", "Norway", "Sweden", "United Kingdom", "United States") )

wealth_share1 <- wealth_share_gathered[-c(1, 69, 137, 205, 273, 341, 409), ] 

wealth_share2 <- wealth_share1 %>%
  mutate(Year = ifelse(Country == "Denmark", ...1, ifelse(Country == "Finland", ...3, ifelse(Country == "France", ...5, ifelse(Country == "Norway", ...7, ifelse(Country == "Sweden", ...9, ifelse(Country == "United Kingdom", ...11, ifelse(Country == "United States", ...13, NA))))))))

wealth_share2$Wealth_share = as.numeric(as.character(wealth_share2$Wealth_share))

wealth_share3 <- wealth_share2 %>%
  select(-c("...1", "...3", "...5", "...7", "...9", "...11", "...13")) %>%
  filter(!is.na(Year)) %>%
  mutate(wealth_share = (Wealth_share)/100)

#Plotting the data 
wealth_share_plot <- ggplot(wealth_share3, aes(x = Year, y = wealth_share, group = Country, color = Country)) +
  geom_line() +
  #geom_line(aes(linetype = Country)) + #if we differentiate the lines without color
  labs(y = "Wealth Share of the Top 1%", color = "Country") + 
  scale_y_continuous(breaks = seq(0, 0.7, by = 0.1), labels = scales::percent_format(accuracy = 1), limits = c(0,0.7)) +
  scale_x_continuous(breaks = seq(1740, 2010, by = 30)) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.88,0.83), 
        legend.title = element_blank(), 
        axis.title.y = element_text(size = 17, vjust = 1),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 90, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),  
        axis.title.x = element_text(size = 17, vjust = -1)) 
  
  

#print(wealth_share_plot)

#Save plot to PDF
ggsave(wealth_share_plot, filename = "wealth_share_top_1percent.pdf", 
       path = "capitalism",
       width = 9, height = 7, units = "in")


# dev.off()

