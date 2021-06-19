#Graph Designer: Harriet Brookes-Gray
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(tidyverse)
library(digitize)
library(jpeg)
library(scales)
library(openxlsx)
library(cowplot)

trust_game_data <- read.xlsx("public_mechanism/Fig4.1_trust_game_data.xlsx")

colnames(trust_game_data)[1] <- "transfer of investor"
colnames(trust_game_data)[2] <- "incentive condition - fine imposed"
colnames(trust_game_data)[3] <- "trust condition - no fine possible"
colnames(trust_game_data)[4] <- "incentive condition - fine possible but not imposed"

trust_game_data_long <- trust_game_data %>%
  gather("Stat", "Value", -`transfer of investor`)


trust_game_data_long$`transfer of investor` <- factor(trust_game_data_long$`transfer of investor`)

trust_game_data_long$Stat <- factor(trust_game_data_long$Stat,                                    # Change ordering manually
                  levels = c("trust condition - no fine possible", "incentive condition - fine imposed","incentive condition - fine possible but not imposed"))

trust_game_plot <- 
  ggplot(trust_game_data_long, aes(x = `transfer of investor`, y = Value, fill = Stat)) + 
  geom_col(position = "dodge", alpha = 0.9, width = 0.95) + 
  xlab("Transfer by the investor") +
  ylab("Amount returned") + 
  scale_y_continuous(breaks = seq(0, 14, by = 2), limits = c(0, 14)) + 
  labs(fill = "Treatment") +
  scale_fill_manual(values=c("#4DAF4A","#E41A1C","#377EB8")) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16), 
        legend.position = c(0.36, 0.86)
        )
print(trust_game_plot)
  
ggsave("public_mechanism/trust_game_figure.pdf", width = 9, height = 7, units = "in")

# trust_game_plot1 <- 
#   ggplot(trust_game_data_long, aes(x = `transfer of investor`, y = Value, fill = Stat, group = Value)) + 
#   geom_col(position = "dodge", alpha = 0.9) + 
#   xlab("Transfer by the investor") +
#   ylab("Amount returned") + 
#   scale_y_continuous(breaks = seq(0, 16, by = 2), limits = c(0,16)) + 
#   theme_bw() + 
#   labs(fill = "Treatment") +
#   scale_fill_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A"),labels = c("Incentive condition - fine imposed", "Incentive condition - fine possible 
# but not imposed", "Trust condition - no fine possible"))
# group = desc(-Value))
# print(trust_game_plot1)
