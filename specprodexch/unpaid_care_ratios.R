library(readxl)
library(tidyverse)

#Data upload and filter
Data <- read_csv("female-to-male-ratio-of-time-devoted-to-unpaid-care-work.csv")

data_final <- Data %>%
  filter(Entity %in% c("USA", "India", "China", "Italy", "Turkey","Cuba", "Brazil"))

colnames(data_final)[4] <- "Ratio"
#Create figure 

CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

plot <- ggplot(data_final, aes(x = Entity, y = Ratio)) + 
  geom_bar( stat = "identity", position = position_dodge(), fill = CBCols[2]) + 
  xlab("Country") + 
  ylab("Female to male ratio of time devoted to unpaid care work") +
  theme_bw() +
  theme(
        axis.title = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  coord_flip()
               
plot

ggsave("specprodexch/unpaid_care_ratios.pdf", plot, width = 11, height = 7)