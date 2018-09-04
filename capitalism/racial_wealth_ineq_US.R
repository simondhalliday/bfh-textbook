#Author: Simon Halliday
#Source: Survey of Income and Program Participation, 2014 Panel
#Project: Microeconomics: Coordination, Conflict and Competition
#Collaorators: Samuel Bowles and Duncan Foley
#License of graph: MIT License https://choosealicense.com/licenses/mit/

library(tidyverse)

download.file("https://www2.census.gov/programs-surveys/demo/tables/wealth/2013/wealth-asset-ownership/wealth-tables-2013.xlsx", 
              destfile = "wealth-tables-2013.xlsx",
              method = "curl")

RaceIneq <- read_xlsx("wealth-tables-2013.xlsx", sheet = 1, col_names = FALSE, range = "A7:B13")

RaceIneq <- 
  RaceIneq %>%
  rename(Race = X__1, 
         NetWealth = X__2) %>% 
  mutate(Race = factor(Race,
                       levels = c("White alone", 
                                  ".White alone, not Hispanic", 
                                  "Black alone", "Asian alone", 
                                  "Other (residual)", 
                                  "Hispanic origin (any race)", 
                                  "Not of Hispanic origin"),
                       labels = c("White alone",
           "White, not Hispanic",
           "Black",
           "Asian",
           "Other",
           "Hispanic origin",
           "Not Hispanic")))

#RaceIneq$Race

WIneq <- 
  RaceIneq %>%
  ggplot(aes(x = reorder(Race, NetWealth), y = NetWealth, color = Race, fill = Race)) + 
  geom_bar(stat = "identity") + 
  xlab("Race") + 
  theme_bw() + 
  scale_y_continuous(name = "Median Net Wealth", breaks = seq(0,140000,15000)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size = 18)) + 
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_color_brewer(type = "qual", palette = "Set2") + 
  labs(title = "Median Wealth by Race Group in the US, 2013", 
       subtitle = "Source: Survey of Income and Program Participation, 2014 Panel")

pdf(file = "WIneq_US.pdf", width = 9, height = 7)
WIneq
dev.off()

jpeg(file = "WIneq_US.jpg", width = 640, height = 480, units = "px")
WIneq
dev.off()