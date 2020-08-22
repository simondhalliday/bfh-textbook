require(ggplot2)
require(ggrepel)
require(dplyr)
library(countrycode)

COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")

UnionCoverage <- read.csv("employment/union_coverage.csv")
UnionCoverage <- 
  UnionCoverage %>% 
  arrange(Coverage)
UnionCoverage$Country <- countrycode(UnionCoverage$Country, "iso3c", "country.name")

UnionCoverage$Country <- 
  factor(UnionCoverage$Country, levels = UnionCoverage$Country[order(UnionCoverage$Coverage)])



u1 <- ggplot(UnionCoverage, aes(x = Country, y = Coverage)) + 
  geom_bar(stat = "identity", 
              aes(fill = Coverage), 
              fill = "#009E73",
              show.legend = FALSE) + 
  #scale_fill_manual(guide = "legend") + #This is so I can supress the legend
  labs(y = "Percentage of workers covered by collective bargaining, %") + 
  geom_text(
    aes(x = Country, y = Coverage, label = round(Coverage, 0)), 
    size = 5, hjust = -0.1,
    position = position_dodge(width = 0.9),
    inherit.aes = TRUE
  ) +
  xlab("")+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 20),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size = 15 )
        ) + 
  coord_flip()

ggsave("capitalism/employment_union_coverage.pdf", width = 9, height = 7, units = "in")


