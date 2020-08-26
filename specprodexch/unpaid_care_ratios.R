library(readxl)
library(tidyverse)

#Data upload and filter
Data <- read_csv("female-to-male-ratio-of-time-devoted-to-unpaid-care-work.csv")

data_final <- Data %>%
  filter(Entity %in% c("United States", "India", "China", "France", "Turkey","Cuba", "Brazil"))

colnames(data_final)[4] <- "Ratio"
#Create figure 

CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

plot <- ggplot(data_final, aes(x = reorder(Entity, Ratio), y = Ratio)) + 
  geom_bar( stat = "identity", position = position_dodge(), fill = CBCols[2], width = 0.75) + 
  ylab("Female to male ratio of time devoted to unpaid care work") +
  theme_bw() +
  theme(
        axis.title = element_text(size = 40),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size =40),
        #legend.title = element_text(size = 16),
        axis.title.x = element_text(size = 40),
        axis.text.x  = element_text(vjust = 0.5, size = 40)) + 
  geom_text(
    aes(x = Entity, y = Ratio, label = round(Ratio, digits =1)), 
    hjust = -0.1,size = 12,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) + 
  coord_flip()


plot

ggsave("specprodexch/unpaid_care_ratios.pdf", plot, width = 19, height = 10)