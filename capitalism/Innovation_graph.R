library(tidyverse)
library(readxl)

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824", "#f0027f")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081", "#9e9ac8","#f0027f")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
COLD <- c("#DA3030","#41ae76","#F7DE04", "#4eb3d3","#AE82FF","#386cb0","#F48318","#41ae76","#6a51a3", "#DA3030")

 #---------------------------------------------
Data <- read_excel("capitalism/2020_Bloomberg_Innovation_Index.xlsx")


#drop unnecessary columns
Data_select <- Data %>%
  select(-c(`2020 Rank`, `2019 Rank`,`YoY Change`, `R&D Intensity`, `Manufacturing Value-added`, `Productivity`, `High-tech Density`,`Tertiary Efficiency`,`Reseacrher Concentration`,`Patent Activity`)) %>%
  desc(`Total Score`)

Data_select$Economy <- factor(Data_select$Economy, levels = Data_select$Economy [order(Data_select$`Total Score`)])

plot <- Data_select %>% 
  ggplot(aes(x = Economy, y = `Total Score`)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = COLB[4]) + 
  ylab("Total Score") +
  #geom_text(aes(x=point,y=Utility,label=Utility),vjust=90) + 
  #geom_text(aes(x = point, y = Utility, label = Utility)) +
  #scale_x_discrete(labels = xaxislabs) + 
  #scale_x_discrete(labels=c("z" = x1, "ta" = x2,
  #                         "tb" = x3, "b" = expression(paste("Legislated hours and wages, ",bold(b))), "a" = x5)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.text.align = 0,
        panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x = element_text(size = 15))+
  geom_text(
    aes(x = Economy, y = `Total Score`, label = `Total Score`), 
    size = 4, hjust = -0.1,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) + coord_flip()
plot

ggsave("capitalism/innovation_graph_v2.pdf", plot, width = 11, height = 7)

# scale_fill_discrete(name = "", 
#                     breaks = c("ua", "ub", "totalu"), 
#                     labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
# ) +
#   scale_color_discrete(name = "", 
#                        breaks = c("ua", "ub", "totalu"), 
#                        labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
#   ) +