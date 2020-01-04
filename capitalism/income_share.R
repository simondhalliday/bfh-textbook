require(shape)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)

income_share <- read_excel("capitalism/income_share.xlsx")

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824", "#f0027f")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081", "#9e9ac8","#f0027f")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Cleaning the wealth share data. Source: CORE
income_share_gathered <- income_share %>% 
  gather(Country,income_share, c("Australia", "Argentina", "Canada", "China", "India", "South Africa", "United Kingdom", "United States") )

income_share1 <- income_share_gathered[-c(1, 105, 209, 313, 417, 521, 625, 729), ]

income_share2 <- income_share1 %>%
  mutate(Year = ifelse(Country == "Australia", ...1, ifelse(Country == "Argentina", ...3, ifelse(Country == "Canada", ...5, ifelse(Country == "China", ...7, ifelse(Country == "India", ...9, ifelse(Country == "South Africa", ...11, ifelse(Country == "United Kingdom", ...13, ifelse(Country == "United States", ...15, NA)))))))))

income_share2$income_share = as.numeric(as.character(income_share2$income_share))

income_share3 <- income_share2 %>%
  select(-c("...1", "...3", "...5", "...7", "...9", "...11", "...13", "...15")) %>%
  filter(!is.na(income_share)) %>%
  mutate(income_share_decimal = (income_share)/100)

#Plotting the data
income_share_plot <- ggplot(income_share3, aes(x = Year, y = income_share_decimal, group = Country, color = Country)) +
  geom_line() +
  ylab("Income Share of the Top 1%") +
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.05), labels = scales::percent_format(accuracy = 1), limits = c(0,0.3)) +
  scale_x_continuous(breaks = seq(1910, 2015, by = 5)) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_blank(),
        axis.title.y = element_text(size = 12.5, vjust = 1),
        legend.text=element_text(size=11),
        axis.text.x = element_text(size = 11, angle = 90, color = "black"),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.title.x = element_text(size = 12.5, vjust = -1))


print(income_share_plot)

#Save plot to PDF
ggsave(wealth_share_plot, filename = "income_share_top_1percent.pdf",
       path = "capitalism",
       width = 9, height = 7, units = "in")


dev.off()

