library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(mosaic)
library(haven)
library(readxl)
library(zoo)
Capital <- read_csv("what_can_markets_do/PIKETTY-TSI_1.csv", col_names = TRUE)
Capital <- 
  Capital %>%
  mutate(USA = Value) %>% 
  select(-Value)
Capital2 <- read_csv("what_can_markets_do/PIKETTY-TS9_3.csv", col_names = TRUE)
Capital2 <- 
  Capital2 %>%
  select(-contains("0.1%")) %>% 
  rename(Date = Yaer)

Capital <-
  Capital %>%
  left_join(Capital2) 

Top1Comp <- 
  Capital %>% 
  rename(Canada = `Canada: Top 1%`, 
         Australia = `Australia: Top 1%`, 
         `New Zealand`= `New Zealand: Top 1%`, 
         Denmark = `Denmark: 1%`, 
         Italy = `Italy: Top 1%`, 
         Holland = `Holland: Top 1%`, 
         Spain = `Spain: Top 1%`
  )

TopComp <- 
  Top1Comp %>% 
  gather(country, value, -Date)


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

Top1 <- Capital %>%
  ggplot(aes(x = Date, y = Value)) + 
  geom_line(color = COLB[5]) + 
  xlab("Date (1910 to 2010)") + 
  ylab("Income share of the top 1% in the USA") +
  theme_bw() 
  

pdf(file = "what_can_markets_do/top_1percent.pdf", width = 8, height = 6)
Top1
dev.off()

Top1_2 <- 
  TopComp %>%
  ggplot(aes(x = Date, y = value, group = country)) + 
  geom_line(color = COLB[5]) + 
  xlab("Date (1910 to 2010)") + 
  ylab("Income share of the top 1% in wealthy countries") +
  theme_bw() 
Top1_2

WID <- read_excel("what_can_markets_do/WID_Data.xls", col_names = TRUE)

WIDinc <- 
  WID %>%
  rename(USA = `sfiinc_z_US
Fiscal income
Top 1% | share
USA`, 
         Netherlands = `sfiinc_z_NL
Fiscal income
Top 1% | share
Netherlands`,
         China = `sfiinc_z_CN
Fiscal income
Top 1% | share
China`,
         Argentina = `sfiinc_z_AR
Fiscal income
Top 1% | share
Argentina`,
         `South Korea` = `sfiinc_z_KR
Fiscal income
Top 1% | share
Korea`,
         `South Africa` = `sfiinc_z_ZA
Fiscal income
Top 1% | share
South Africa`, 
        India = `sfiinc_z_IN
Fiscal income
Top 1% | share
India`
  )

WIDinc <- 
  WIDinc %>%
  #select(-contains("shweal"), -Percentile) %>%
  filter(Year > 1912)

WIDincN <- 
  WIDinc %>%
  select(-Percentile) %>%
  gather(country, value, -Year) %>%
  rename(Country = country) %>% 
  filter(complete.cases(.)) %>% 
  filter(Country != "South Korea")

  
# WIDincN <- 
#   WIDincN %>%
#   mutate(value = na.approx(WIDincN$value))
  
WIDincplot <- 
  WIDincN %>% 
  ggplot(aes(x = Year, y = value, group = Country, color = Country)) + 
  geom_line() + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  ylab("Income share of the top 1%") +
  xlab("Year (1913 to 2012)") +
  theme_bw() 

pdf(file = "what_can_markets_do/top_1income.pdf", width = 4, height = 3)
WIDincplot
dev.off()
