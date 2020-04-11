#' Scott Cohn
#' Markup Ratio Analysis
#' Paper: Minimum costs to manufacture new treatments for COVID-19
#' Authors: Andrew Hill, Junzheng Wang, Jacob Levi, Katie Heath, Joseph Fortunak
#' Journal: Journal of Virus Eradication 2020

# NOTE: List price (rounded to nearest USD$)

library(tidyverse)
library(plyr)

# Functions ---------------------------------------------------------------

estMarkup <- function(preTaxCost, tax, convRate){
  costAll = preTaxCost + (preTaxCost*convRate) + (preTaxCost*convRate*tax)
  
  return(costAll)
}

markupRatioFunc <- function(p, c){
  (p - c) / c
}

avg <- function(x) {
  c(mean = mean(x), median = median(x))
}

# Bowles Estimate ---------------------------------------------------------

tax = 0.27
preTaxCost = 18.01
convRate = 0.1

c_star = estMarkup(preTaxCost, tax, convRate)
print(c_star)

# Data Input --------------------------------------------------------------

## Lopinavir/ritonavir (lopRi)
# 14-day treatment course (400 mg once daily)

lopRi <- tibble(
  "drugID" = 1,
  "country" = c("USA_pharm", "USA_va", "SWE", "TUR", "GBR", "FRA", "IND", "CHN", "ZAF", "gloFund"),
  "cost_lopRi" = c(503, 349, 172, 149, 144, 97, 40, 17, 15, 9),
)

## Hydroxychloroquine (hyd)
# 14-day treatment course (400 mg once daily)

hyd <- tibble(
  "drugID" = 2,
  "country" = c("CHN", "USA_pharm", "MYS", "France", "GBR", "SWE", "BGD", "TUR", "USA_va", "IND"),
  "cost_hyd" = c(19, 18, 7, 5, 4, 3, 3, 3, 3, 2)
)

## Chloroquine (chl)
# 14-day treatment course (155 mg once daily)

chl <- tibble(
  "drugID" = 3,
  "country" = c("USA_pharm", "GBR", "CHN", "ZAF", "SWE", "MYS", "IND", "BGD"),
  "cost_chl" = c(93, 8, 5, 5, 4, 2, 1, 0.2)
)

## Azithromycin (azi)
# 14-day treatment course (500 mg once daily)

azi <- tibble(
  "drugID" = 4,
  "country" = c("USA_pharm", "FRA", "ZAF", "BRA", "USA_va", "SWE", "MYS", "GBR", "CHN", "BGD", "IND"),
  "cost_avi" = c(63, 44, 35, 19, 17, 16, 11, 11, 7, 5, 5)
)

## Sofosbuvir/daclatasvir (sofDa)
# 14-day treatment course (400/60 mg once daily)

sofDa <- tibble(
  "drugID" = 5,
  "country" = c("USA_va", "GBR", "FRA", "BRA", "BGD", "IND", "PAK"),
  "cost_sofDa" = c(18610, 7632, 4662, 4289, 166, 7, 6)
)

## Pirfenidone (pirf)
# 28-day treatment course (801 mg three times daily)

pirf <- tibble(
  "drugID" = 6,
  "country" = c("USA_pharm", "USA_va", "GBR", "ZAF", "FRA", "SWE", "TUR", "CHN", "BGD", "IND"),
  "cost_pirf" = c(9606, 6513, 2561, 2490, 2344, 2196, 1499, 1379, 124, 100)
)

## Tocilizumab (toc)
# Single IV dose (560 mg)

toc <- tibble(
  "drugID" = 7,
  "country" = c("USA_pharm", "CHN", "USA_va", "GBR", "IND", "BGD", "TUR", "EGY", "ZAF", "PAK"),
  "cost_toc" = c(3383, 1950, 1948, 914, 806, 690, 650, 606, 566, 510)
)

## General Estimates
# Single course
genEst <- tibble(
  "drugID" = seq(1:7),
  "drugName" = c("Lopinavir/ritonavir", "Hydroxychloroquine", "Chloroquine", "Azithromycin", "Sofosbuvir/daclatasvir", "Pirfenidone", "Tocilizumab"),
  "cost_genEst" = c(4, 1, 0.3, 1.4, 5, 31, NA)
)

## Combine Dataframes

df <- as_tibble(join_all(list(azi, chl, hyd, lopRi, pirf, sofDa, toc), 
  by = c('country', 'drugID'), type = 'full')
)

dfCost <- left_join(df, genEst, "drugID") %>% 
  gather(3:9, key = "drugKey", value = "drugCostByCountry") %>% 
  drop_na("drugCostByCountry") %>% 
  drop_na("cost_genEst") %>% 
  select("drugID", "drugName", "country", "cost_genEst", "drugCostByCountry")

# Markup ------------------------------------------------------------------

# cbind markup ratio
dfCost <- dfCost  %>% 
  cbind(markupRatio = round(markupRatioFunc(p = dfCost[['drugCostByCountry']], c = dfCost[['cost_genEst']]), digits = 2)) 

# find averages 
dfCostAvg <- as_tibble(cbind(drugID = unique(dfCost$drugID), 
  do.call(rbind, tapply(dfCost$markupRatio, dfCost$drugID, avg))))

# left join averages with dfCost by drugID
dfCostAvg <- left_join(dfCostAvg, dfCost, by = "drugID") %>% 
  gather(2:3, key = "avgType", value = "avgVal")

# Visual ------------------------------------------------------------------

# avg markup by drug across ALL countries

drugNames <- c("Sofosbuvir/daclatasvir", "Azithromycin", "Hydroxychloroquine", "Lopinavir/ritonavir", "Chloroquine")
drugNameDisease <- c("Sofosbuvir/daclatasvir \n (Hepatitis C)", 
                     "Azithromycin \n (Antibiotic)", 
                     "Hydroxychloroquine \n (Malaria)", 
                     "Lopinavir/ritonavir \n (HIV-1)", 
                     "Chloroquine \n (Malaria)")

dfCostAvg %>% 
  filter(drugID != 6) %>%
  mutate(drugName = factor(drugName, levels = drugNames)) %>%
  ggplot(aes(x = factor(drugName), y = avgVal, fill = avgType)) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_x_discrete(labels = str_wrap(drugNameDisease, width = 15)) + # auto-wrap (cluttered)
  scale_x_discrete(label = drugNameDisease) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_brewer(name = "", labels = c("Mean", "Median"), palette = "Set1") +
  labs(y = "Markup ratio", x = "Drug") +
  coord_flip() +
  geom_text(aes(label = round(avgVal, 1)), position = position_dodge(width = 0.9), hjust = -0.25, size = 3) +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 12),
        legend.position = c(.875, .85),
        legend.margin = margin(1, 1, 1, 1)) 

ggsave("avgMarkupAllCountries.pdf", width = 6, height = 3, units = "in")

# # markup by country
# dfCostAvg %>% 
#   filter(country == "USA_pharm") %>%
#   ggplot(aes(x = factor(drugName), y = markupRatio, fill = drugName)) +
#   geom_bar(stat = "identity", position = "dodge") + 
#   coord_flip() +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw()
# 
# ggsave("markupUSA_pharm.pdf", width = 6, height = 3, units = "in")
# 
# dfCostAvg %>% 
#   filter(country == "ZAF") %>%
#   ggplot(aes(x = factor(drugName), y = markupRatio, fill = drugName)) +
#   geom_bar(stat = "identity", position = "dodge") + 
#   coord_flip() +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw()
# 
# ggsave("markupZAF.pdf", width = 6, height = 3, units = "in")
