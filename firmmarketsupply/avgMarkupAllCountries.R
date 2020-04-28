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
  "country" = c("USA_pharm", "USA_va", "SWE", "TUR", "UK", "FRA", "IND", "CHN", "ZAF"),
  "cost_lopRi" = c(503, 349, 172, 149, 144, 97, 40, 17, 15),
)

## Hydroxychloroquine (hyd)
# 14-day treatment course (400 mg once daily)

hyd <- tibble(
  "drugID" = 2,
  "country" = c("CHN", "USA_pharm", "MYS", "France", "UK", "SWE", "BGD", "TUR", "USA_va", "IND"),
  "cost_hyd" = c(19, 18, 7, 5, 4, 3, 3, 3, 3, 2)
)

## Chloroquine (chl)
# 14-day treatment course (155 mg once daily)

chl <- tibble(
  "drugID" = 3,
  "country" = c("USA_pharm", "UK", "CHN", "ZAF", "SWE", "MYS", "IND", "BGD"),
  "cost_chl" = c(93, 8, 5, 5, 4, 2, 1, 0.2)
)

## Azithromycin (azi)
# 14-day treatment course (500 mg once daily)

azi <- tibble(
  "drugID" = 4,
  "country" = c("USA_pharm", "FRA", "ZAF", "BRA", "USA_va", "SWE", "MYS", "UK", "CHN", "BGD", "IND"),
  "cost_avi" = c(63, 44, 35, 19, 17, 16, 11, 11, 7, 5, 5)
)

## Sofosbuvir/daclatasvir (sofDa)
# 14-day treatment course (400/60 mg once daily)

sofDa <- tibble(
  "drugID" = 5,
  "country" = c("USA_va", "UK", "FRA", "BRA", "BGD", "IND", "PAK"),
  "cost_sofDa" = c(18610, 7632, 4662, 4289, 166, 7, 6)
)

## Pirfenidone (pirf)
# 28-day treatment course (801 mg three times daily)

pirf <- tibble(
  "drugID" = 6,
  "country" = c("USA_pharm", "USA_va", "UK", "ZAF", "FRA", "SWE", "TUR", "CHN", "BGD", "IND"),
  "cost_pirf" = c(9606, 6513, 2561, 2490, 2344, 2196, 1499, 1379, 124, 100)
)

## Tocilizumab (toc)
# Single IV dose (560 mg)

toc <- tibble(
  "drugID" = 7,
  "country" = c("USA_pharm", "CHN", "USA_va", "UK", "IND", "BGD", "TUR", "EGY", "ZAF", "PAK"),
  "cost_toc" = c(3383, 1950, 1948, 914, 806, 690, 650, 606, 566, 510)
)

## General Estimates
# Single course
genEst <- tibble(
  "drugID" = seq(1:7),
  "drugName" = c("Lopinavir/ritonavir", "Hydroxychloroquine", "Chloroquine", "Azithromycin", "Sofosbuvir/daclatasvir", "Pirfenidone", "Tocilizumab"),
  "cost_genEst" = c(4, 1, 0.3, 1.4, 5, 31, NA)
  # "cost_genEst" = c(3.92, 1.12, 0.28, 1.4, 5.46, 30.52, NA) # 1/day*14
)

## Combine Dataframes

df <- as_tibble(join_all(list(azi, chl, hyd, lopRi, pirf, sofDa, toc), 
  by = c('country', 'drugID'), type = 'full')
)

dfCost <- left_join(df, genEst, "drugID") %>% 
  gather(3:9, key = "drugKey", value = "drugCostByCountry") %>% 
  drop_na("drugCostByCountry") %>% 
  drop_na("cost_genEst") %>% 
  filter(country != "USA_va") %>% # Remove USA VA
  select("drugID", "drugName", "country", "cost_genEst", "drugCostByCountry")

# Markup ------------------------------------------------------------------

# cbind markup ratio
dfCost <- dfCost  %>% 
  cbind(markupRatio = round(markupRatioFunc(p = dfCost[['drugCostByCountry']], c = dfCost[['cost_genEst']]), digits = 2)) 

# find averages
df_mean <- aggregate(dfCost$markupRatio, list(dfCost$drugName), mean) %>%
  dplyr::rename("mean" = "x") %>%
  dplyr::rename("drugName" = "Group.1") 

df_med <- aggregate(dfCost$markupRatio, list(dfCost$drugName), median) %>%
  dplyr::rename("median" = "x") %>%
  dplyr::rename("drugName" = "Group.1") 

# join averages into df
dfCostAvg <- left_join(df_mean, df_med, by = "drugName")

# left join averages with dfCost by drugID
dfCostAvg <- left_join(dfCostAvg, dfCost, by = "drugName") %>% 
  gather(2:3, key = "avgType", value = "avgVal")

# Visual ------------------------------------------------------------------

# avg markup by drug across ALL countries
drugNames <- c("Lopinavir/ritonavir", 
               "Chloroquine", "Azithromycin")
drugNameDisease <- c("Lopinavir/ritonavir \n (HIV-1)", 
                     "Chloroquine \n (Malaria)",
                     "Azithromycin \n (Antibiotic)")

dfCostAvg %>% 
  filter(drugID != 6) %>% # pirf (not manufactured at scale)
  filter(drugID != 2) %>% # hydrox (not manufactured at scale)
  filter(drugID != 5) %>% # outlier
  mutate(drugName = factor(drugName, levels = drugNames)) %>%
  ggplot(aes(x = factor(drugName), y = avgVal, fill = avgType)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(label = drugNameDisease) +
  scale_y_continuous(limits = c(0, 55)) +
  scale_fill_brewer(name = "", labels = c("Mean", "Median"), palette = "Set1") +
  labs(y = "Markup ratio", x = "Drug") +
  coord_flip() +
  geom_text(aes(label = round(avgVal, 1)), position = position_dodge(width = 0.9), 
            hjust = -0.25, size = 3, check_overlap = TRUE) +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 12),
        legend.position = c(.875, .85),
        legend.margin = margin(1, 1, 1, 1)) 

ggsave("firmmarketsupply/avgMarkupAllCountries.pdf", width = 6, height = 3, units = "in")