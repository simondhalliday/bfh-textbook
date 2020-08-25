library(readxl)
library(tidyverse)

#Data upload and filter
Data <- read_csv("female-to-male-ratio-of-time-devoted-to-unpaid-care-work.csv")

data_final <- Data %>%
  filter(Entity %in% c("USA", "India", "China", "Italy", "Turkey","Cuba", "Brazil"))

colnames(data_final)[4] <- "Ratio"
#Create figure 

plot <- ggplot(data_final, aes(x = Entity, y = Ratio)) + 
  geom_bar( stat = "identity", position = position_dodge()) + 
  coord_flip()
               
plot
