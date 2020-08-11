library(tidyverse)

Ginis <- read_excel("capitalism/ginis_comparison.xlsx")

#List 1
filter(!Country %in% c("Greece", "Poland", "Slovenia"))
filter(!Country %in% c("South Africa", "Uruguay", "Guatemala", "Brazil",  "Peru", "Colombia", "Egypt", "Mexico", "China", "India"))
# Dropped from both: "Greece", "Poland", "Slovenia"

GiniNar <- 
  Ginis %>% 
  gather(type, gini, -Year, -Country) %>%
  select(type, gini, Country) %>% 
  filter(!Country %in% c("Greece", "Poland", "Slovenia")) %>% #Dropped from both figures
  filter(!Country %in% c("South Africa", "Uruguay", "Guatemala", "Brazil",  "Peru", "Colombia", "Egypt", "Mexico", "China", "India"))

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
  ylab("Gini coefficient (various years, 1992-2013)") +
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

pdf(file = "capitalism/gini_comparisons_wealthy.pdf", width = 8, height = 6)
Giniplot
dev.off()


GiniNar2 <- 
  Ginis %>% 
  gather(type, gini, -Year, -Country) %>%
  select(type, gini, Country) %>% 
  filter(!Country %in% c("Greece", "Poland", "Slovenia")) %>% #Dropped from both figures
  filter(Country %in% c("South Africa", "Uruguay", "Guatemala", "Brazil",  "Peru", "Colombia", "Egypt", "Mexico", "China", "India"))

GiniDf1a <- 
  GiniNar2 %>% 
  filter(type == "MarketGini") %>%
  mutate(Country = factor(Country), 
         Country = fct_reorder(Country, gini, .desc = TRUE)) 
GiniDf2a <- 
  GiniNar2 %>% 
  filter(type == "DisposableGini") %>%
  mutate(Country = factor(Country))

GiniPlotDf2 <- bind_rows(GiniDf1a, GiniDf2a)

Giniplot2 <- 
  GiniPlotDf2 %>%
  ggplot(aes(y = gini, x = Country, fill = type)) + 
  geom_bar(stat = "identity", position = "identity") + 
  scale_fill_brewer( type = "qual", palette = "Accent", 
                     breaks = c ("DisposableGini", "MarketGini"),
                     labels = c ("Disposable income", 
                                 "Market income"), 
                     name = "") +
  ylab("Gini coefficient (various years, 1992-2013)") +
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
Giniplot2

pdf(file = "capitalism/gini_comparisons_lowmiddle.pdf", width = 8, height = 6)
Giniplot2
dev.off()

GiniNar <- 
  Ginis %>% 
  gather(type, gini, -Year, -Country) %>%
  select(type, gini, Country) %>% 
  filter(!Country %in% c("Greece", "Poland", "Slovenia")) #Dropped from both figures
 

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
  ylab("Gini coefficient (various years, 1992-2013)") +
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