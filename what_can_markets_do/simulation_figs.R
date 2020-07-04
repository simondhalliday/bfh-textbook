library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(grid)
library(ineq)
library(mosaic)


# Import + Tidy -----------------------------------------------------------

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
  mutate(mrs = 100 - 10*x2, 
         wealth = mrs*x2 + y2, 
         wealthwalras = 50*x2 + y2)

FinalP <- 
  Pareto %>% 
  filter(trade == 67)

Lc(x, n = rep(1,length(x)), plot = FALSE)

Lorenz1 <- Lc(FinalP$utility, n = rep(1,length(FinalP$utility)), plot = TRUE)
LorenzW <- Lc(FinalP$wealth, n = rep(1,length(FinalP$wealth)), plot = TRUE)


# lorenz_utility ----------------------------------------------------------

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


# lorenz_wealth -----------------------------------------------------------

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


# wealth_distribution -----------------------------------------------------


# pdf(file = "what_can_markets_do/wealth_distribution.pdf", width = 5, height = 4)
# FinalP %>% 
#   ggplot(aes(x = wealth, fill = type)) + 
#   geom_histogram(aes(y =..density..),      # Histogram with density instead of count on y-axis
#                  binwidth = 10) + 
#   ylab("Density") + 
#   xlab("Wealth") + 
#   scale_fill_discrete("Trader Type", 
#                       breaks = c("A", "B"),
#                       labels = c("Type A", "Type B")) + 
#   theme_bw()
# dev.off()

#SDH Note 2020.07.03
#Checking the data I saw that Duncan inverted 
#The As and Bs (he switched their endowments)
#So we need to make sure the correct ones are labeled below
#utilities are u^A = y + 100x - 5x^2
#mrs = 100 - 10x = p
#x = (100 - p)/10
#Total Demand for x = (100 - p)/10 + (100 - p)/10 
#Total supply of x = 10
#x = (100 - p)/10 + (100 - p)/10  = 10
# => p = 50 
#Therefore walrasian wealth for A at (9,0) = 450
#Therefore walrasian wealth for B at (1,400) = 450

#Checking mean wealth is the Walrasian wealth
#Indeed it is
summary <- 
  FinalP %>%
  group_by(type) %>% 
  summarise(meanwealth = mean(wealth))

FinalP <-
  FinalP %>%
  mutate(type = factor(type, levels = c("B", "A")))

pdf(file = "what_can_markets_do/wealth_distribution_facet.pdf", width = 8, height = 6)
FinalP %>%
  ggplot(aes(x = wealth, fill = type)) +
  facet_grid(. ~type) +
  geom_histogram(aes(y =..density..),      # Histogram with density instead of count on y-axis
                 binwidth = 10) +
  geom_vline(xintercept = 450, linetype="dashed", color = "black") + 
  annotate("text", x = 500, y = 0.01, label = "Mean wealth", size = 6) +
  ylab("Density") +
  xlab("Wealth") +
  scale_fill_manual("Trader Type",
                    breaks = c("B", "A"),
                    labels = c("Type A", "Type B"),
                    values = c("#e41a1c", "#377eba")) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = "top", 
        strip.background = element_blank(),
        strip.text.x = element_blank()
        )

dev.off()

pdf(file = "what_can_markets_do/wealth_distribution_facet_walras.pdf", width = 8, height = 6)
FinalP %>%
  ggplot(aes(x = wealthwalras, fill = type)) +
  facet_grid(. ~type) +
  geom_histogram(aes(y =..density..),      # Histogram with density instead of count on y-axis
                 binwidth = 10) +
  geom_vline(xintercept = 450, linetype="dashed", color = "black") +
  ylab("Density") +
  xlab("Wealth") +
  scale_fill_manual("Trader Type",
                    breaks = c("B", "A"),
                    labels = c("Type A", "Type B"),
                    values = c("#e41a1c", "#377eba")) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = "top", 
        strip.background = element_blank(),
        strip.text.x = element_blank()
  )

dev.off()

# utility_distribution_facet ----------------------------------------------


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
        axis.text = element_text(size = 9),
        legend.position = "top")
dev.off()

# pdf(file = "what_can_markets_do/utility_distribution_facet.pdf", width = 5, height = 4)
# FinalP %>% 
#   ggplot(aes(x = utility, fill = type)) + 
#   geom_histogram(aes(y =..density..),      # Histogram with density instead of count on y-axis
#                  binwidth = 10) + 
#   ylab("Density") + 
#   xlab("Utility") + 
#   scale_fill_discrete("Trader Type", 
#                       breaks = c("A", "B"),
#                       labels = c("Type A", "Type B")) + 
#   theme_bw()
# dev.off()


# old clean up ------------------------------------------------------------


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


# trading paths -----------------------------------------------------------

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

# trading_paths_allpoints -------------------------------------------------

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


# trading_paths_pos -------------------------------------------------------


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


# trading_paths_arrows ----------------------------------------------------


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


# trading_paths_topA ------------------------------------------------------

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


