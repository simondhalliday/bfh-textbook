library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(grid)
library(ineq)
library(forcats)
library(ggrepel)


Housing <- read_excel("capitalism/housing.xlsx")

HousingFew <- 
  Housing %>%
  filter(country %in% c("Austria", "Netherlands", "Poland", "Germany", "United Kingdom", "France", 
                        "Switzerland", "Sweden", "Denmark"))

HFnar <- 
  HousingFew %>%
  gather(type, rate, ownernoloan:tenantsubs)

HFnar$type <- factor(HFnar$type, levels = c("tenantsubs", "tenantmarket", "ownerloan", "ownernoloan"))

HFnar <- 
  HFnar %>% 
  arrange(type, rate) %>%
  mutate(country = factor(country))


HFnar$country <- factor(HFnar$country, 
                        levels = c("Switzerland", "Sweden", "Netherlands", 
                                   "Denmark", "Germany",
                                   "United Kingdom", "Austria",
                                   "France","Poland"))   

hplot <- HFnar %>% 
  ggplot(aes(fill = type, y = rate, x = country)) + 
  geom_bar(stat = "identity", position = "fill")

hplot2 <- 
  hplot + 
  xlab("Country") + 
  ylab("Rate") + 
  #scale_fill_discrete() + 
  scale_fill_brewer( type = "qual", palette = "Paired", 
                     breaks = c ("ownernoloan", "ownerloan", "tenantmarket", "tenantsubs"),
                     labels = c ("Own outright (no loan)", 
                                 "Own with mortgage", 
                                 "Tenant at market rent", 
                                 "Tenant with subsidized or zero rent"), 
                     name = "Type of Tenancy") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
hplot2

pdf(file = "capitalism/housing_eu.pdf", width = 8, height = 4)
hplot2 
dev.off()


inGDP <- read_excel("capitalism/ineq_growth.xlsx")

OECDplot <- 
  inGDP %>%
  #filter(country != "Australia") %>%
  ggplot(aes(x = avgini, y = avgdpgrowth)) +
  geom_point(color = COLB[4], size = 3) +
  geom_text_repel(aes(label = country), size = 5) + 
  theme_bw() + 
  ylab("Average GDP per capita growth, 1970-2012, %") + 
  xlab("Long-term average inequality in disposable income") + 
  scale_x_continuous(expand = c(0, .01))

pdf(file = "capitalism/ineq_gdp_oecd.pdf", width = 8, height = 5)
OECDplot
dev.off()




inGDP2 <- read_excel("capitalism/ineq_growth_dev.xlsx")

Devplot <- 
  inGDP2 %>%
  ggplot(aes(x = avgini, y = avgdpgrowth)) +
  geom_point(color = COLA[4], size = 3) +
  geom_text_repel(aes(label = country), size = 5) + 
  theme_bw() + 
  ylab("Average GDP per capita growth, 1980-2012, %") + 
  xlab("Long-term average inequality in disposable income") + 
  scale_x_continuous(expand = c(0, .05)) #+ 
  #scale_y_continuous(expand = c(0, ))
Devplot

pdf(file = "capitalism/ineq_gdp_dev.pdf", width = 8, height = 5)
Devplot
dev.off()

