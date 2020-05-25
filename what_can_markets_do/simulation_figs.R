library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(grid)
library(ineq)

Sim <- read_excel("what_can_markets_do/simulations2.xlsx")

narSim <- 
  Sim %>%
  gather(trade, round, -trader, -type)

splitdata <- 
  narSim %>% 
  separate(round, into = c("beginning", "middle", "end", "end1", "another"), sep="\\}")  %>% 
  select(trader, type, trade, end)

split2 <- 
  splitdata %>%
  separate(end, into = c("beginning", "x", "y"), ",") %>% 
  select(trader, type, trade, x,y) %>% 
  separate(x, into = c("beginning", "x"), "\\{") %>%
  select(trader, type, trade, x, y)

split2 <- 
  split2 %>%
  mutate(x = as.numeric(x), y = as.numeric(y))

Pareto <- 
  split2 %>%
  mutate(utility = ifelse(type == "A", 
                          y + 100*x - (1/2)*(100/10)*x^2,
                          ifelse(type == "B", 
                          y + 100*x - (1/2)*(100/10)*x^2, NA)
                          )
         ) %>%
  arrange(type, trade, utility)

Pareto <- 
  Pareto %>%
  group_by(trader) %>%
  mutate(x1 = lag(x), 
         y1 = lag(y),
         trade = as.numeric(trade)) %>%
  rename(x2 = x, y2 = y)


Pareto <- 
  Pareto %>%
  mutate(mrs = 100 + 10*x2, 
         wealth = mrs*x2 + y2)

FinalP <- 
  Pareto %>% 
  filter(trade == 67)

Lc(x, n = rep(1,length(x)), plot = FALSE)

Lorenz1 <- Lc(FinalP$utility, n = rep(1,length(FinalP$utility)), plot = TRUE)
LorenzW <- Lc(FinalP$wealth, n = rep(1,length(FinalP$wealth)), plot = TRUE)

pdf(file = "what_can_markets_do/lorenz_utility.pdf", width = 5, height = 4)
plot(Lorenz1, 
     col = "#0868ac",
     general = FALSE, 
     lwd = 2, 
     xlab = "Cumulative Population of Traders", 
     ylab = "Cumulative Utilities",
     main = "Lorenz curve", 
     las = 1
)
dev.off()


pdf(file = "what_can_markets_do/lorenz_wealth.pdf", width = 5, height = 4)
plot(LorenzW, 
     col = "#0868ac",
     general = FALSE, 
     lwd = 2, 
     xlab = "Cumulative Population of Traders", 
     ylab = "Cumulative Wealth",
     main = "Lorenz curve", 
     las = 1
)
dev.off()




pdf(file = "what_can_markets_do/wealth_distribution.pdf", width = 5, height = 4)
FinalP %>% 
  ggplot(aes(x = wealth, fill = type)) + 
  geom_histogram(aes(y =..density..),      # Histogram with density instead of count on y-axis
                 binwidth = 10) + 
  ylab("Density") + 
  xlab("Wealth") + 
  scale_fill_discrete("Trader Type", 
                      breaks = c("A", "B"),
                      labels = c("Type A", "Type B")) + 
  theme_bw()
dev.off()


pdf(file = "what_can_markets_do/wealth_distribution_facet.pdf", width = 5, height = 4)
FinalP %>% 
  ggplot(aes(x = wealth, fill = type)) + 
  facet_grid(. ~type) +
  geom_histogram(aes(y =..density..),      # Histogram with density instead of count on y-axis
                 binwidth = 10) + 
  ylab("Density") + 
  xlab("Wealth") + 
  scale_fill_manual("Trader Type",
                    breaks = c("A", "B"),
                    labels = c("Type A", "Type B"),
                    values = c("#e41a1c", "#377eba")) +
  theme_bw() + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 9))
dev.off()


pdf(file = "what_can_markets_do/utility_distribution_facet.pdf", width = 5, height = 4)
FinalP %>% 
  ggplot(aes(x = utility, fill = type)) + 
  facet_grid(. ~type) +
  geom_histogram(aes(y =..density..),      # Histogram with density instead of count on y-axis
                 binwidth = 10) + 
  ylab("Density") + 
  xlab("Utility") + 
  scale_fill_manual("Trader Type",
                    breaks = c("A", "B"),
                    labels = c("Type A", "Type B"),
                    values = c("#e41a1c", "#377eba")) +
  theme_bw() + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 9))
dev.off()

pdf(file = "what_can_markets_do/utility_distribution_facet.pdf", width = 5, height = 4)
FinalP %>% 
  ggplot(aes(x = utility, fill = type)) + 
  geom_histogram(aes(y =..density..),      # Histogram with density instead of count on y-axis
                 binwidth = 10) + 
  ylab("Density") + 
  xlab("Utility") + 
  scale_fill_discrete("Trader Type", 
                      breaks = c("A", "B"),
                      labels = c("Type A", "Type B")) + 
  theme_bw()
dev.off()


###The content below applied to an old version of the data. 
ParetoFinal <-
  Pareto %>%
  filter(trade == 92)

TopA <- 
  Pareto %>%
  filter(Trader %in% c(157, 260, 525, 599, 198, 364, 57, 630, 412, 839)) %>%
  filter(trade %in% seq(1, 92, 10))

BestWorstA <- 
  Pareto %>%
  filter(Trader == 157 | Trader == 161)
  #filter(Trader == 525 | Trader == 161)

BestWorstApos <- 
  Pareto %>%
  filter(Trader == 525 | Trader == 161)

BestWorstB <- 
  Pareto %>%
  filter(Trader == 1031 | Trader == 1651)
  
Tests <- 
  Pareto %>% 
  filter(Trader == 260 | Trader == 525 | Trader == 599)


BWFewA <- 
  BestWorstApos %>%
  #filter(trade == 2 | trade == 25 | trade == 50 | trade == 75 | trade == 92) %>%
  filter(trade %in% seq(1, 92, 10)) %>%
  arrange(Trader, trade)

pdf(file = "what_can_markets_do/trading_paths.pdf", width = 6, height = 4)
BWFewA %>% 
  ungroup() %>%
  ggplot(aes(x = x2, y = y2, color = , group = as.factor(Trader))) + 
  geom_jitter() + 
  geom_path() + 
  scale_colour_discrete(name = "Trader",
                        breaks = c("157", "161"),
                        #breaks = c("525", "161"),
                        labels = c("Best Performing A", "Worst Performing A")) + 
  scale_x_continuous(breaks = seq(-4, 5.5, 1),
                     labels = seq(-4, 5.5, 1)
                     ) +
  xlab("the good, x") + 
  ylab("money, y") + 
  theme_bw()
dev.off()


pdf(file = "what_can_markets_do/trading_paths_allpoints.pdf", width = 6, height = 4)
BestWorstA %>% 
  arrange(Trader, trade) %>%
  ggplot(aes(x = x2, y = y2, color = as.factor(Trader), group = as.factor(Trader), order = trade)) + 
  geom_jitter() + 
  geom_path() + 
  scale_colour_discrete(name = "Trader",
                        breaks = c("157", "161"),
                        #breaks = c("525", "161"),
                        labels = c("Best Performing A", "Worst Performing A")) + 
  scale_x_continuous(breaks = seq(-4, 5.5, 1),
                     labels = seq(-4, 5.5, 1)
  ) +
  xlab("the good, x") + 
  ylab("money, y") + 
  theme_bw()
dev.off()

pdf(file = "what_can_markets_do/trading_paths_pos.pdf", width = 6, height = 4)
BWFewA %>% 
  arrange(Trader, trade) %>%
  ggplot(aes(x = x2, y = y2, color = as.factor(Trader), group = as.factor(Trader), order = trade)) + 
  geom_jitter() + 
  geom_path() + 
  scale_colour_discrete(name = "Trader",
                        breaks = c("525", "161"),
                        labels = c("Best Performing A", "Worst Performing A")) + 
  scale_x_continuous(breaks = seq(-4, 5.5, 1),
                     labels = seq(-4, 5.5, 1)
  ) +
  xlab("the good, x") + 
  ylab("money, y") + 
  theme_bw()
dev.off()



pdf(file = "what_can_markets_do/trading_paths_arrows.pdf", width = 6, height = 4)
BWFewA %>% 
  ggplot(aes(x = x2, y = y2, color = as.factor(Trader), group = as.factor(Trader))) + 
  geom_jitter() + 
  geom_segment(aes(xend=c(tail(x2, n=-1), NA), yend=c(tail(y2, n=-1), NA), group = as.factor(Trader)),
               arrow=arrow(length=unit(0.3,"cm")), data = BWFewA) + 
  scale_colour_discrete(name = "Trader",
                        breaks = c("157", "161"),
                        labels = c("Best Performing A", "Worst Performing A")) + 
  scale_x_continuous(breaks = seq(-4, 5.5, 1),
                     labels = seq(-4, 5.5, 1)
  ) +
  xlab("the good, x") + 
  ylab("money, y") + 
  theme_bw()
dev.off()


pdf(file = "what_can_markets_do/trading_paths_topA.pdf", width = 6, height = 4)
TopA %>% 
  ggplot(aes(x = x2, y = y2, color = as.factor(Trader))) + 
  geom_jitter() + 
  geom_path() + 
  scale_x_continuous(breaks = seq(-4, 5.5, 1),
                     labels = seq(-4, 5.5, 1)
  ) +
  xlab("the good, x") + 
  ylab("money, y") +
  theme_bw()
dev.off()



  geom_segment(aes(x = x1, y = y1, xend = x, yend = y, color = as.factor(Trader)), data = BWFewA)
  
  
   


