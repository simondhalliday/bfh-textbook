require(ggplot2)
require(shape)
require(gridExtra)

COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

pie_1 <- data.frame(
  group = c("A", "B"),
  value = c(50, 50)
)

pie_2 <- data.frame(
  group = c("A", "B"),
  value = c(35,65)
)

pie_3 <- data.frame(
  group = c("A", "B"),
  value = c(7.5,92.5)
)

pie_4 <- data.frame(
  group = c("A", "B"),
  value = c(NA, 100)
)


pie_plot1 <- ggplot(pie_1, aes(x = " ",
                       y = value,
                       fill = group)) +
  geom_bar(width = 1,
           stat = "identity") +
  coord_polar("y", start = 0) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values=c("#2b8cbe",
                             "#41ae76")) +
  geom_text(aes(label = paste(round(value / sum(value) * 100, 1),"%")),
            position = position_stack(vjust = 0.5), size = 4.5) 
  
pie_plot1


pie_plot2<- ggplot(pie_2, aes(x = "",
                              y = value,
                              fill = group)) +
  geom_bar(width = 1,
           stat = "identity") +
  coord_polar("y") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values=c("#2b8cbe",
                             "#41ae76"))  + 
  geom_text(aes(label = paste(round(value / sum(value) * 100, 1),"%")),
            position = position_stack(vjust = 0.5), size = 4.5) 
pie_plot2


pie_plot3<- ggplot(pie_3, aes(x = " ",
                              y = value,
                              fill = group)) +
  geom_bar(width = 1,
           stat = "identity") +
  coord_polar("y") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values=c("#2b8cbe",
                             "#41ae76")) + 
  #scale_y_continuous(breaks=c(pie_3$value), labels = c(pie_3$value)) + 
  geom_text(aes(label = paste(round(value / sum(value) * 100, 1),"%")),
            position = position_stack(vjust = 0.6), size = 4.5) 
pie_plot3

pie_plot4<- ggplot(pie_4, aes(x = "",
                              y = value,
                              fill = group)) +
  geom_bar(width = 1,
           stat = "identity") +
  coord_polar("y", start = 0) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),) +
  scale_fill_manual(values=c(
                             "#41ae76")) + 
  geom_text(label = paste(100,"%"),
            position = position_stack(vjust = 0), size = 4.5) 
pie_plot4

plot_final <- grid.arrange(pie_plot1, pie_plot2, pie_plot3, pie_plot4, ncol=4)

ggsave(plot_final, filename = "piecharts.pdf", 
       path = "capitalism",
       width = 8, height = 6, units = "in")

