library(tidyverse)

Ginis <- read_excel("capitalism/ginis_comparison.xlsx")

GiniNar <- 
  Ginis %>% 
  gather(type, gini, -Year, -Country) %>%
  select(type, gini, Country)

GiniDf1 <- 
  GiniNar %>% 
  filter(type == "MarketGini") %>%
  mutate(Country = factor(Country), 
         Country = fct_reorder(Country, gini, .desc = TRUE)) 
GiniDf2 <- 
  GiniNar %>% 
  filter(type == "DisposableGini") %>%
  mutate(Country = factor(Country))

GiniPlotDf <- bind_rows(GiniDf1, GiniDf2)

Giniplot <- 
  GiniPlotDf %>%
  ggplot(aes(y = gini, x = Country, fill = type)) + 
  geom_bar(stat = "identity", position = "identity") + 
  scale_fill_brewer( type = "qual", palette = "Accent", 
                     breaks = c ("DisposableGini", "MarketGini"),
                     labels = c ("Disposable income", 
                                 "Market income"), 
                     name = "") +
  ylab("Gini coefficient (various years, 1992-2013") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text.align = 0,
        panel.grid = element_blank(),
        axis.title = element_text(size = 22),
        axis.text.y = element_text(size = 11),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15)) +
  coord_flip()
Giniplot

pdf(file = "capitalism/gini_comparisons.pdf", width = 8, height = 6)
Giniplot
dev.off()


