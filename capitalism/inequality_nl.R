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

Ineq <- read_excel("capitalism/lorenz_curve_data.xlsx")

colnames(Ineq)[5] <- "Cumulative population proportion1"
colnames(Ineq)[9] <- "Cumulative population proportion2"

IneqSel <- 
  Ineq %>%
  select(-c(`Market Income (100)`, `Disposable Income (100)`, tbeqhMarket, tbeqhdhi, `Cumulative population proportion1`, CountryPerc)) %>%
  rename(cumprop = `Cumulative population proportion2`, markety = `Market Income`, dispy = `Disposable Income`)


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
  xlab("Cumulative population, (%)") + 
  ylab("Cumulative income") +
  theme_bw() +
  labs(title = "Lorenz curves and Gini coefficients", subtitle = "Netherlands (2010)") +
  #ggtitle("Lorenz Curves and Gini Coefficient for the Netherlands (2010)") +
  theme(panel.spacing.x = unit(2, "lines"), title = element_text(size = 14))
Lorenz1

Lorenz2 <- 
  Lorenz1 + 
  geom_area(aes(y = cumprop), fill = COLB[1], colour = COLB[5]) +
  geom_area(aes(y = cumy), fill = COLA[1], colour = COLA[5]) +
  geom_text(data = data.frame(x = 0.25, y = 0.75, 
                              label=c("Gini = 0.25", "Gini = 0.47"), 
                              type=c("Disposable income","Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE, size = 5) +
  geom_text(data = data.frame(x = 0.5, y = 0.4, 
                              label=c("A'", "A"), 
                              type=c("Disposable income","Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE, size = 5) +
  geom_text(data = data.frame(x = 0.8, y = 0.2, 
                              label=c("B'", "B"), 
                              type=c("Disposable income","Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE, size =5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        strip.text = element_text(size=14), 
        plot.margin = margin(1, 12, 0, 0))
Lorenz2  


pdf(file = "capitalism/inequality_nl.pdf", width = 9, height = 4)
Lorenz2
dev.off()


LorenzMarket <- 
  IneqNar %>% 
  filter(CountryDate == "NL2010" & type == "Market income") %>%
  ggplot(aes(y = cumy, x = cumprop, group = type)) + 
  geom_line() + 
  #facet_grid(. ~type, space = "free") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  #geom_line(aes(y = cumprop, y = cumprop)) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Cumulative population, (%)") + 
  ylab("Cumulative income") +
  geom_area(aes(y = cumprop), fill = COLB[1], colour = COLB[5]) +
  geom_area(aes(y = cumy), fill = COLA[1], colour = COLA[5]) +
  geom_text(data = data.frame(x = 0.25, y = 0.75, 
                              label=c("Gini = 0.47"), 
                              type=c("Market income")), 
            aes(x, y, label = label), inherit.aes = FALSE, size = 5) +
  geom_text(data = data.frame(x = 0.5, y = 0.4, 
                              label=c("A")), 
            aes(x, y, label = label), inherit.aes = FALSE, size = 5) +
  geom_text(data = data.frame(x = 0.8, y = 0.2, 
                              label=c("B")), 
            aes(x, y, label = label), inherit.aes = FALSE, size = 5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.margin = margin(6, 12, 6, 6)) 
LorenzMarket
ggsave("capitalism/inequality_nl_market.pdf", LorenzMarket, width = 4, height = 4)

LorenzDisposable <- 
  IneqNar %>% 
  filter(CountryDate == "NL2010" & type == "Disposable income") %>%
  ggplot(aes(y = cumy, x = cumprop, group = type)) + 
  geom_line() + 
  #facet_grid(. ~type, space = "free") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  #geom_line(aes(y = cumprop, y = cumprop)) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Cumulative population, (%)") + 
  ylab("Cumulative income") +
  geom_area(aes(y = cumprop), fill = COLB[1], colour = COLB[5]) +
  geom_area(aes(y = cumy), fill = COLA[1], colour = COLA[5]) +
  geom_text(data = data.frame(x = 0.25, y = 0.75, 
                              label=c("Gini = 0.25"), 
                              type=c("Disposable income")), 
            aes(x, y, label = label), inherit.aes = FALSE, size = 5) +
  geom_text(data = data.frame(x = 0.5, y = 0.4, 
                              label=c("A")), 
            aes(x, y, label = label), inherit.aes = FALSE, size = 5) +
  geom_text(data = data.frame(x = 0.8, y = 0.2, 
                              label=c("B")), 
            aes(x, y, label = label), inherit.aes = FALSE, size = 5) +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        plot.margin = margin(6, 12, 6, 6)) 


ggsave("capitalism/inequality_nl_disposable.pdf", LorenzDisposable, width = 4, height = 4)

#pdf(file = "capitalism/inequality_nl_disposable.pdf", width = 5, height = 4)