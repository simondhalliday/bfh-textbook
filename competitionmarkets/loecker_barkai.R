#Graph Designer: Bridget Diana + Scott Cohn + Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics


require(shape)
library(tidyverse)
library(lubridate)
library(openxlsx)
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

loecker_barkai <- 
  loecker_barkai %>%
  mutate(year_l = coalesce(year_l, 0),
         year_b = coalesce(year_b, 0)
         )

loecker_barkai$year <- 
  pmax(loecker_barkai$year_l, loecker_barkai$year_b,
       na.rm = FALSE
       )

# manually inputting missing values: 

#Loecker:
#missing year values
loecker_barkai$year<- ifelse(loecker_barkai$year == 0, loecker_barkai$year, loecker_barkai$year)
#missing markup values (estimated from original data):
#1983:
loecker_barkai[53, 2] <- 0.193
#1984:
loecker_barkai[54, 2] <- 0.21
#1987:
loecker_barkai[55, 2] <- 0.2796
#1993:
loecker_barkai[56, 2] <- 0.34
#1995:
loecker_barkai[57, 2] <- 0.365
#1996:
loecker_barkai[58, 2] <- 0.375
#1998 
loecker_barkai[59, 2]<- 0.3936267

#Barkai:
#missing year values 
loecker_barkai$year_b<- ifelse(loecker_barkai$year_b == 0, ifelse(loecker_barkai$year >= 1985,loecker_barkai$year, loecker_barkai$year_b),  loecker_barkai$year_b)
#missing b_share values (estimated from original data)
#2001:
loecker_barkai[40, "b_share"] <- .03
#2011:
loecker_barkai[50, "b_share"] <- 0.12


#The ggplot had issues with read l_markup.y
#So I re-did it and just mutated it
#as  you'd coded it it was trying 
#to read l_markup as a df itself
LBdf <- 
  loecker_barkai %>%
  mutate(markup = l_markup$y) %>% 
  select(year, markup, b_share)

write.xlsx(LBdf, 'competitionmarkets/loecker_barkai/LBdf_data.xlsx')

# graph
# ----

#library(readxl)
#LBdf <- read_excel("competitionmarkets/loecker_barkai/LBdf_data.xlsx")

p <-  ggplot(data = LBdf) +
  geom_line(aes(x = year, y = markup, color = "Markup ratio"), na.rm = TRUE) +
  geom_line(aes(x = year, y = b_share, color = "Share of economic profit"), na.rm = TRUE) +
  ylab("Share of economic profit and markup ratio") + 
  xlab("Year") +
  #scale_x_continuous() +
  theme_bw() +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(legend.position = c(0.01, 0.96), 
        legend.justification = 'left',
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        text = element_text(size = 19),
        axis.text = element_text(size = 17),
        axis.title = element_text(size = 22),
        panel.grid.minor = element_blank()) + 
  scale_colour_manual(values = c("#0868ac","#41ae76"))
  

#Save plot to PDF
ggsave(p, filename = "loecker_barkai.pdf", 
       path = "competitionmarkets",
       width = 7, height = 7, units = "in")

# ----

#dev.off()