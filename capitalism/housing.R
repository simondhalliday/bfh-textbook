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
library(xts)


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


