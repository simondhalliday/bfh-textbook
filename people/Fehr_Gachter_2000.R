library(ggplot2)
library(ggthemes)

FG_2000 <- data.frame("p" = 1:20, 
                     "contrib" = c(6.5, 4.9, 6.5, 5.8, 5.5, 4.5, 4.7, 2.9, 3.5, 2.0,
                                   7.7, 9.5, 9.9, 10.5, 10.2, 10.3, 11.7, 12.4, 12.7, 13.2), 
                     "Treatment" = c("without punishment", "without punishment", "without punishment", "without punishment", "without punishment", "without punishment", "without punishment", "without punishment", "without punishment", "without punishment",
                                   "with punishment", "with punishment", "with punishment", "with punishment", "with punishment", "with punishment", "with punishment", "with punishment", "with punishment", "with punishment")
                     )

BHcolors <- c("#41ae76", "#0868ac")

tx3 <- ggplot(data = FG_2000, aes(x = p, y = contrib, col=Treatment)) +
  geom_point() + 
  geom_line() +
  ylim(0, 20) +
  scale_color_manual(values = BHcolors) +
  #scale_shape_manual( values = c(1, 19)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  #scale_fill_discrete("Treatment", breaks=c("wo", "w"),
  #                    labels = c("without punishment", "with punishment")) +
  xlab("Periods") + 
  ylab("Average Contributions") +
  theme_classic()

  
ggsave("people/Fehr_Gachter_2000.pdf", width = 10, height = 5)