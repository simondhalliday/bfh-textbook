#Author: Simon Halliday
#Source: Survey of Income and Program Participation, 2014 Panel
#Project: Microeconomics: Coordination, Conflict and Competition
#Collaorators: Samuel Bowles and Duncan Foley
#License of graph: MIT License https://choosealicense.com/licenses/mit/

library(tidyverse)
library(readxl)
library(forcats)

download.file("https://www2.census.gov/programs-surveys/demo/tables/wealth/2013/wealth-asset-ownership/wealth-tables-2013.xlsx", 
              destfile = "wealth-tables-2013.xlsx",
              method = "curl")

RaceIneqT <- read_xlsx("wealth-tables-2013.xlsx", sheet = 4, col_names = TRUE, range = "A4:K13")

RaceIneqT <- 
  RaceIneqT %>%
  rename(Race = X__1, 
         Observations = X__2) %>%
  filter(complete.cases(.))  %>%
  select(-Observations) %>%
  mutate(Race = factor(Race,
                       levels = c("Total",
                                  "White alone", 
                                  ".White alone, not Hispanic", 
                                  "Black alone", "Asian alone", 
                                  "Other (residual)", 
                                  "Hispanic origin (any race)", 
                                  "Not of Hispanic origin"),
                       labels = c("Total", 
                                  "White alone",
                                  "White, not Hispanic",
                                  "Black",
                                  "Asian",
                                  "Other",
                                  "Hispanic origin",
                                  "Not Hispanic")))
WInN <- 
  RaceIneqT %>%
  gather(Type, Value, -Race) %>% 
  mutate(Type = factor(Type, 
                      levels = c("Zero or Negative",
                                 "$1 to $4,999",
                                 "$5,000 to $9,999",
                                 "$10,000 to $24,999",
                                 "$50,000 to $99,999",
                                 "$25,000 to $49,999",
                                 "$100,000 to $249,999",
                                 "$250,000 to $499,999", 
                                 "$500,000 or over"
                                 )
                      )
         )


#WInN$Type
  
WIneqBand <- 
  WInN %>%
  filter(Race %in% c("White alone", "White, not Hispanic", "Black", "Asian", "Hispanic origin", "Not Hispanic")) %>%
  mutate(Race = fct_relevel(Race, "Asian", "White, not Hispanic", "White alone", "Not Hispanic", "Hispanic origin", "Black")) %>%
  ggplot(aes(x = Race, y = Value, color = Type, fill = Type, order = Type)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  scale_fill_brewer(type = "div", palette = "RdYlBu", name = "Wealth Value") +
  scale_color_brewer(type = "div", palette = "RdYlBu", name = "Wealth Value") + 
  ylab("Percentage") + 
  xlab("Race") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size = 18)) + 
  labs(title = "Wealth Composition by Race Group in the US, 2013", 
       subtitle = "Source: Survey of Income and Program Participation, 2014 Panel")
WIneqBand

pdf(file = "WIneqBand_US.pdf", width = 9, height = 7)
WIneqBand
dev.off()

jpeg(file = "WIneqBand_US.jpg", width = 640, height = 480, units = "px")
WIneqBand
dev.off()