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

Ineq <- read_excel("risk/lorenz_curve_data.xlsx")

IneqSel <- 
  Ineq %>%
  select(-c(`Market Income (100)`, `Disposable Income (100)`, tbeqhMarket, tbeqhdhi, `Cumulative population proportion__1`, CountryPerc)) %>%
  rename(cumprop = `Cumulative population proportion`, markety = `Market Income`, dispy = `Disposable Income`)
  

IneqNar <- 
  IneqSel %>% 
  gather(type, cumy, -CountryDate, -Percentile,-cumprop) %>%
  mutate(equality = Percentile/100)

IneqNar$type <- factor(IneqNar$type, 
                       levels = c("markety", "dispy"),
                       labels = c("Market income", "Disposable income"))
#IneqNar$type_f <- factor(IneqNar$type, levels = c("markety","dispy"))

Lorenz1 <- 
  IneqNar %>% 
  filter(CountryDate == "NL2010") %>%
  ggplot(aes(y = cumy, x = cumprop, group = type)) + 
  geom_line() + 
  facet_grid(. ~type, space = "free") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  #geom_line(aes(y = cumprop, y = cumprop)) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Cumulative population proportion") + 
  ylab("Cumulative income") +
  theme_bw() +
  labs(title = "Lorenz curves and Gini coefficients", subtitle = "Netherlands (2010)") +
  #ggtitle("Lorenz Curves and Gini Coefficient for the Netherlands (2010)") +
  theme(panel.spacing.x = unit(1.5, "lines"))
Lorenz1

Lorenz2 <- 
  Lorenz1 + 
  geom_area(aes(y = cumprop), fill = COLB[1], colour = COLB[5]) +
  geom_area(aes(y = cumy), fill = COLA[1], colour = COLA[5]) +
  geom_text(data = data.frame(x = 0.25, y = 0.75, 
                              label=c("Gini = 0.25", "Gini = 0.47"), 
                              type=c("Disposable income","Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 0.5, y = 0.4, 
                              label=c("A'", "A"), 
                              type=c("Disposable income","Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 0.8, y = 0.2, 
                              label=c("B'", "B"), 
                              type=c("Disposable income","Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
Lorenz2  


pdf(file = "risk/inequality_nl.pdf", width = 8, height = 4)
Lorenz2
dev.off()


LorenzUS <- 
  IneqNar %>% 
  filter(CountryDate == "US2013") %>%
  ggplot(aes(y = cumy, x = cumprop, group = type)) + 
  geom_line() + 
  facet_grid(. ~type, space = "free") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  #geom_line(aes(y = cumprop, y = cumprop)) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Cumulative population proportion") + 
  ylab("Cumulative income") +
  theme_bw() +
  labs(title = "Lorenz curves and Gini coefficients", subtitle = "United States (2013)") +
  #ggtitle("Lorenz Curves and Gini Coefficient for the Netherlands (2010)") +
  theme(panel.spacing.x = unit(1.5, "lines"))

LorenzUS1 <- 
  LorenzUS + 
  geom_area(aes(y = cumprop), fill = COLB[1], colour = COLB[5]) +
  geom_area(aes(y = cumy), fill = COLA[1], colour = COLA[5]) +
  geom_text(data = data.frame(x = 0.25, y = 0.75, 
                              label=c("Gini = 0.39", "Gini = 0.52"), 
                              type=c("Disposable income","Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 0.5, y = 0.4, 
                              label=c("A'", "A"), 
                              type=c("Disposable income","Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 0.8, y = 0.2, 
                              label=c("B'", "B"), 
                              type=c("Disposable income","Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
LorenzUS1  
pdf(file = "risk/inequality_us.pdf", width = 8, height = 4)
LorenzUS1
dev.off()

LorenzComp <- 
  IneqNar %>% 
  filter(type == "Disposable income") %>%
  ggplot(aes(y = cumy, x = cumprop, group = CountryDate)) + 
  geom_line(aes(color = CountryDate, linetype = CountryDate)) + 
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_color_manual(name = "Country", 
                     breaks = c("NL2010", "US2013"), 
                     labels = c("Netherlands", "United States"), 
                     values = c(COLA[5], COLA[6])) +
  scale_linetype_manual(name = "Country", 
                     breaks = c("NL2010", "US2013"), 
                     labels = c("Netherlands", "United States"), 
                     values = c("solid", "dashed")) +
  geom_abline(intercept = 0, slope = 1, color = COLB[5]) +
  xlab("Cumulative population proportion") + 
  ylab("Cumulative income") +
  theme_bw() +
  annotate("text", x = 0.2, y = 0.8, label = "NL Gini = 0.25") + 
  annotate("text", x = 0.2, y = 0.7, label = "US Gini = 0.38") +
  labs(title = "Lorenz curves and Gini coefficients of disposable income", 
       subtitle = "United States (2013) & The Netherlands (2010)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
LorenzComp 


pdf(file = "risk/inequality_comp.pdf", width = 6, height = 4)
LorenzComp
dev.off()


ineqUSdy <- 
  IneqNar %>% 
  filter(CountryDate == "US2013" & type == "Disposable income")
  


Ginis <- read_excel("risk/ginis_comparison.xlsx")

GiniNar <- 
  Ginis %>% 
  gather(type, gini, -Year, -Country) 

GiniNar$Country <- factor(GiniNar$Country, levels(order(GiniNar$gini)))
  


Giniplot <- 
  GiniNar %>%
  ggplot(aes(y = gini, x = reorder(Country, gini), fill = type)) + 
  geom_bar(stat = "identity", position = "identity") + 
  scale_fill_brewer( type = "qual", palette = "Accent", 
                     breaks = c ("DisposableGini", "MarketGini"),
                     labels = c ("Disposable income", 
                                 "Market income"), 
                     name = "Income type") +
  ylab("Gini coefficient (various years, 1992-2013") +
  xlab("Country") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
Giniplot

pdf(file = "risk/gini_comparisons.pdf", width = 8, height = 6)
Giniplot
dev.off()

