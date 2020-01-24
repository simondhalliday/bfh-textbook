library(ggplot2)
library(ggthemes)

#Need to add actual values
GR_2000 <- data.frame("weeknum" = 1:20, 
                      "fine" = c(9, 8, 12, 10, 6.5, 9.5, 9, 11, 8, 10.8,
                                 7, 7.75, 9, 9, 12, 6.5, 6, 9.5, 7, 9 ), 
                      "control" = c(7.5, 7, 7.5, 7, 6.2, 11.5, 14, 17.5, 21, 14,
                                    18, 18, 19, 20, 19.5, 19, 16, 19, 21, 18.5)
)

BHcolors <- c("#41ae76", "#0868ac")

fines <- ggplot(data = GR_2000, aes(x = weeknum)) +
  geom_point(aes(y = fine, color = BHcolors[1])) + 
  geom_line(aes(y = fine, color = BHcolors[1])) +
  geom_point(aes(y = control, color = BHcolors[2])) + 
  geom_line(aes(y = control, color = BHcolors[2])) +
  scale_color_manual("Fines", values = BHcolors, labels=c("Group with fine","Control group")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  xlab("Week Number") + 
  ylab("Late Arrivals") +
  theme_classic() + 
  theme(axis.title.y = element_text(size = 16),
        legend.position = c(0.2, 0.9),
        legend.text=element_text(size=16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.title =element_text(size=16)) 
print(fines)

ggsave("people/Gneezy_Rustichini_2000.pdf", width = 10, height = 5)
