require(ggplot2)
require(ggrepel)
require(dplyr)

COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")

pdf(file = "capitalism/employment_union_coverage.pdf", width = 9, height = 7)
UnionCoverage <- read.csv("employment/union_coverage.csv")
UnionCoverage <- 
  UnionCoverage %>% 
  arrange(Coverage)

UnionCoverage$Country <- 
  factor(UnionCoverage$Country, levels = UnionCoverage$Country[order(UnionCoverage$Coverage)])
u1 <- ggplot(UnionCoverage, aes(x = Country, y = Coverage)) + 
  geom_bar(stat = "identity", 
              aes(fill = Coverage), 
              fill = COLA[4],
              show.legend = FALSE) + 
  #scale_fill_manual(guide = "legend") + #This is so I can supress the legend
  ylab("Percentage of workers covered by collective bargaining, %") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 13 )
        ) + 
  coord_flip()

print(u1)
dev.off()

