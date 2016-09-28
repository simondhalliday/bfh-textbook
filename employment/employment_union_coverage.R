require(ggplot2)
require(ggrepel)
require(dplyr)

pdf(file = "employment/employment_union_coverage.pdf", width = 9, height = 7)
UnionCoverage <- read.csv("employment/union_coverage.csv")
UnionCoverage <- 
  UnionCoverage %>% 
  arrange(Coverage)

UnionCoverage$Country <- factor(UnionCoverage$Country, 
                                levels = UnionCoverage$Country[order(UnionCoverage$Coverage)])
u1 <- ggplot(UnionCoverage, aes(x = Country, y = Coverage))
u1 + geom_bar(stat = "identity", aes(fill = Coverage), show.legend = FALSE) + 
  scale_fill_distiller(palette = "Spectral", 
                       trans = "reverse", #This command reverse the order of the colors
                       guide = "legend") + #This is so I can supress the legend
  ylab("% of workers covered by collective bargaining") + 
  theme_bw() +
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 14), 
        axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 8 ))
dev.off()

