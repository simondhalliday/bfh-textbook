#Graph Designer: Bridget Diana + Scott Cohn + Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics


require(shape)
library(tidyverse)
library(lubridate)
#pdf(file = "nyt_wallet_by_country.pdf", width = 7, height = 7)
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

#----
# pull in data
# ----
library(readr)
behavioral_data <- read_csv("people/behavioral_data_nyt.csv")

# adjust / clean

response_rate<-
  behavioral_data %>%
    group_by(Country, cond) %>%
    summarize(response_rate=mean(response))

response_rate_subset <- subset(response_rate, cond<2)

response_rate_subset$NoMoneyRate <- ifelse(response_rate_subset$cond==0, response_rate_subset$response_rate, 0)

response_rate_subset$CountryOrd<-reorder(response_rate_subset$Country, response_rate_subset$NoMoneyRate)

response_rate_subset$condFactor<- as.factor(response_rate_subset$cond)
response_rate_subset$condFactor<- ifelse(response_rate_subset$condFactor==0, "No money", "Money")

response_rate_subset <- 
  response_rate_subset %>%
  filter(!Country %in% c("UAE", "China", "Romania", "Greece","Serbia","Croatia","Peru"))

# graph
# ----

nytfig <- ggplot(data=response_rate_subset, aes(x=response_rate, y=CountryOrd, color=condFactor, group=CountryOrd)) + 
  geom_point(size = 3) + 
  geom_line() + 
  labs(x = "Reporting rate", y = "Country", colour = "Treatment") +  
  scale_color_brewer(type = "qual", 
                     palette = "Set1") + 
  #scale_color_manual(values=c(COLB[4], COLA[4])) + 
  theme_bw() + 
  theme(axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 20),
        legend.text=element_text(size = 16),
        legend.title = element_text(size = 16), 
        legend.position = c(0.85, 0.08)) 

ggsave("people/nyt_wallet_by_country.pdf", nytfig, width = 7, height = 8)

# ----

#dev.off()
