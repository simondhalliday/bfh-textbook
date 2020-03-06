#Graph Designer: Bridget Diana + Scott Cohn + Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(tidyverse)
library(lubridate)
# ----
#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")


# Import Data -------------------------------------------------------------

library(haven)
US_Mexico_data <- read_dta("indmarketdemand/US_Mexico_cleaned_data.dta")


# Graphs ------------------------------------------------------------------

US_elec <- ggplot(data = subset(US_Mexico_data, Country == "USA"), mapping = aes(x = exp_decile, y = E_share)) +  
  labs(x = "Expenditure decile", y = "U.S. Average Electricity Expenditure as Percent of Total Spending") + 
  scale_x_continuous(breaks = seq(0, 10, 1)) + 
  geom_bar(stat = "identity", fill = COLB[4]) +
  #ylim(0, 7) +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank())

ggsave("indmarketdemand/US_electricity.pdf", US_elec, width = 7, height = 7)

Mex_elec <- ggplot(data = subset(US_Mexico_data, Country == "Mexico"), mapping = aes(x = exp_decile, y = E_share)) + 
  labs(x = "Expenditure decile", y = "Mexico Average Electricity Expenditure as Percent of Total Spending") + 
  scale_x_continuous(breaks = seq(0, 10, 1)) + 
  geom_bar(stat = "identity", fill = COLB[4]) +
  #ylim(0, 7) +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank())

ggsave("indmarketdemand/Mexico_electricity.pdf", Mex_elec, width = 7, height = 7)

US_gas <- ggplot(data = subset(US_Mexico_data, Country == "USA"), mapping = aes(x = exp_decile, y = G_share)) + 
  labs(x = "Expenditure decile", y = "U.S. Average Motor Fuels Expenditure as Percent of Total Spending") + 
  scale_x_continuous(breaks = seq(0, 10, 1)) + 
  geom_bar(stat = "identity", fill = COLB[4]) +
  #ylim(0, 7) +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank())

ggsave("indmarketdemand/US_gasoline.pdf", US_gas, width = 7, height = 7)

Mex_gas <- ggplot(data = subset(US_Mexico_data, Country == "Mexico"), mapping = aes(x = exp_decile, y = G_share)) + 
  labs(x = "Expenditure decile", y = "Mexico Average Motor Fuels Expenditure as Percent of Total Spending") + 
  scale_x_continuous(breaks = seq(0, 10, 1)) + 
  geom_bar(stat = "identity", fill = COLB[4]) +
  #ylim(0, 7) +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.grid.minor = element_blank())

ggsave("indmarketdemand/Mexico_gasoline.pdf", Mex_gas, width = 7, height = 7)


