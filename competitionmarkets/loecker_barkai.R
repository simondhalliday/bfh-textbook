require(shape)
library(tidyverse)
library(lubridate)
#pdf(file = "competitionmarkets/loecker_barkai.pdf", width = 7, height = 7)
# ----
#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
# functions
# ----
sub_1 <- function(x){
  x - 1
}

date_max <- function(x, y){
  max(x, y)
}
# ----


#----
# pull in data
# ----
library(readr)
barkai_2018 <- read_csv("competitionmarkets/loecker_barkai/barkai_2018.csv")
loecker_2018 <- read_csv("competitionmarkets/loecker_barkai/loecker_2018.csv")

# adjust / clean
loecker_2018$y_adj<- sub_1(loecker_2018["y"])


barkai_2018 <- barkai_2018 %>% 
  rename("year_b" = x, "b_share" = y)

loecker_2018 <- loecker_2018 %>% 
  rename("year_l" = x, "l_markup" = y_adj) 

loecker_2018$y <- NULL

loecker_2018$year <- floor(loecker_2018$year_l)
barkai_2018$year <- floor(barkai_2018$year_b)

# ----
# merge data
loecker_barkai <- full_join(loecker_2018, barkai_2018, by = "year")

#The ggplot had issues with read l_markup.y
#So I re-did it and just mutated it
#as  you'd coded it it was trying 
#to read l_markup as a df itself

LBdf <- 
  loecker_barkai %>%
  mutate(markup = l_markup$y) %>% 
  select(year, markup, b_share)

#loecker_barkai$year_l <- NULL
#loecker_barkai$year_b <- NULL

#loecker_barkai <- loecker_barkai[, c(2, 1, 3)]
# ----


# graph
# ----

p <-  ggplot(data = LBdf) +
  geom_line(aes(x=year, y=markup), color = COLB[4]) +
  geom_line(aes(x=year, y=b_share), color = COLA[4]) +
  theme_classic() +
  ylab("Year") + 
  xlab("Value") 


#Save plot to PDF
ggsave(p, filename = "loecker_barkai.pdf", 
       path = "competitionmarkets",
       width = 7, height = 7, units = "in")

# ----



dev.off()