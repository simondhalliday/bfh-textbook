library(readxl)
library(ggplot2)
library(gridExtra)

fremsted_paul <- read_excel("indmarketdemand/fremsted_paul.xlsx")


plot1 <-
  ggplot(fremsted_paul, aes(as.factor(Decile), cost_per_person)) +
  geom_bar(position = "dodge", stat = "identity", fill = "#0868ac") +
  geom_hline(yintercept = mean(fremsted_paul$cost_per_person), lty = 2, size = 1) +
  annotate("text", x = 1.5, y = 2387, label = "Mean", size = 7) +
  geom_hline(yintercept = median(fremsted_paul$cost_per_person), lty = 2, size = 1) + 
  annotate("text", x = 1.5, y = 1874, label = "Median", size = 7) +
  theme_bw()  +
  labs(y = "Cost in $ per person",
       x = "Decile") +
  theme(axis.text = element_text(size=17),
        axis.title = element_text(size=22),
        text = element_text(size=17),
        panel.grid.minor = element_blank())


plot2 <-
  ggplot(fremsted_paul, aes(as.factor(Decile), cost_percent_inc)) +
  geom_bar(position = "dodge", stat = "identity", fill = "#41ae76") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(NA, 0.15)) +
  theme_bw()  +
  labs(y = "Cost as percent of income",
       x = "Decile") +
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=22),
        text = element_text(size=17),
        panel.grid.minor = element_blank())

ggsave("indmarketdemand/plot_fp_A.pdf", plot1, width = 9, height = 7)
ggsave("indmarketdemand/plot_fp_B.pdf", plot2, width = 9, height = 7)

