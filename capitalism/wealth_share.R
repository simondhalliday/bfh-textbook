require(shape)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)

wealth_share <- read_excel("capitalism/top_1percent_wealth_share.xlsx")

#Cleaning the wealth share data. Source: CORE
wealth_share_gathered <- wealth_share %>% 
  gather(Country, Wealth_share, c("Denmark", "Finland", "France", "Norway", "Sweden", "United Kingdom", "United States") )

wealth_share1 <- wealth_share_gathered[-c(1, 69, 137, 205, 273, 341, 409), ] 

wealth_share2 <- wealth_share1 %>%
  mutate(Year = ifelse(Country == "Denmark", ...1, ifelse(Country == "Finland", ...3, ifelse(Country == "France", ...5, ifelse(Country == "Norway", ...7, ifelse(Country == "Sweden", ...9, ifelse(Country == "United Kingdom", ...11, ifelse(Country == "United States", ...13, NA))))))))

wealth_share3 <- wealth_share2 %>%
  select(-c("...1", "...3", "...5", "...7", "...9", "...11", "...13")) %>%
  filter(!is.na(Year))

#Plotting the data 
wealth_share_plot <- ggplot(wealth_share3, aes(x = Year, y = Wealth_share, group = Country)) +
  geom_line()

print(wealth_share_plot)
