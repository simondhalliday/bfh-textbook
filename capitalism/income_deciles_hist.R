library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(grid)
library(ineq)

#Colors: 
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

Ineq <- read_excel("capitalism/data/lorenz_curve_data.xlsx")

df <- 
  Ineq %>% 
  filter(Percentile %in% seq(9, 99, 10))

df$Percentile <- as.factor(df$Percentile)

IneqNL <- df[-c(11:20), ]
  
IneqNL1 <- IneqNL %>% 
  mutate(DiffMI = `Market Income` - lag(`Market Income`)) %>%
  mutate(ValuesMI = ifelse(is.na(DiffMI), `Market Income`, DiffMI)) %>%
  mutate(DiffDI = `Disposable Income` - lag(`Disposable Income`)) %>%
  mutate(ValuesDI = ifelse(is.na(DiffDI), `Disposable Income`, DiffDI)) %>%
  gather(key = IncomeType, value = value, c(13,15))
  
IneqNL1$IncomeType <- factor(IneqNL1$IncomeType, levels = c('ValuesMI', 'ValuesDI'))

plot1 <- ggplot(IneqNL1, aes(x = Percentile, y = value, group = IncomeType, fill = IncomeType)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#1F78B4","#E31A1C"), name = "Income type", labels = c("Market income", "Disposable income")) +
  xlab("Population decile of the Netherlands") + 
  ylab("Share of market income") +
  theme_bw() + 
  ylim(0,0.4) + 
  scale_x_discrete(labels=c("9" = "0-10", "19" = "11-20",
                            "29" = "21-30", "39" = "31-40", "49" = "41-50","59" = "51-60", "69" = "61-70", "79" = "71-80", "89" = "81-90", "99" = "91-100")) + 
  theme(legend.position = c(0.15, 0.9),
        legend.title = element_text(size = 16),
        axis.title.y = element_text(size = 18, vjust = 1),
        legend.text=element_text(size = 16),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18))



IneqUS <- df[-c(1:10), ]


IneqUS1 <- IneqUS %>% 
  mutate(DiffMI = `Market Income` - lag(`Market Income`)) %>%
  mutate(ValuesMI = ifelse(is.na(DiffMI), `Market Income`, DiffMI)) %>%
  mutate(DiffDI = `Disposable Income` - lag(`Disposable Income`)) %>%
  mutate(ValuesDI = ifelse(is.na(DiffDI), `Disposable Income`, DiffDI)) %>%
  gather(key = IncomeType, value = value, c(13,15))

IneqUS1$IncomeType <- factor(IneqUS1$IncomeType, levels = c('ValuesMI', 'ValuesDI'))

plot2 <- ggplot(IneqUS1, aes(x = Percentile, y = value, group = IncomeType, fill = IncomeType)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#1F78B4","#E31A1C"), name = "Income type", labels = c("Market income", "Disposable income")) +
  xlab("Population decile of the USA") + 
  ylab("Share of market income") +
  theme_bw() + 
  ylim(0,0.4) + 
  scale_x_discrete(labels=c("9" = "0-10", "19" = "11-20",
                            "29" = "21-30", "39" = "31-40", "49" = "41-50","59" = "51-60", "69" = "61-70", "79" = "71-80", "89" = "81-90", "99" = "91-100")) + 
  theme(legend.position = c(0.15, 0.9),
        legend.title = element_text(size = 16),
        axis.title.y = element_text(size = 18, vjust = 1),
        legend.text=element_text(size = 16),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18))



ggsave(plot1, filename = "income_decile_NL.pdf",
       path = "capitalism",
       width = 9, height = 7, units = "in")

ggsave(plot2, filename = "income_decile_US.pdf",
       path = "capitalism",
       width = 9, height = 7, units = "in")




