# Graph Designer: Scott Cohn
# Authors: Bowles and Halliday
# Title: Microeconomics: Competition, Conflict and Coordination

library(tidyverse) # ggplot
library(ggsci)     # d3 colors
library(readxl)    # import

# Import Data
JEBO_chart_data <- read_excel("~/Desktop/JEBO chart data.xlsx")


europe <- c("FRA", "GER", "NET", "SWE", "UK", "SWI")

# Euro --------------------------------------------------------------------

p1 <- JEBO_chart_data %>% 
  filter(name %in% europe) %>% 
  ggplot(aes(x = year,y = hour, color = name)) +
  geom_line() + 
  geom_point() +
  labs(x = "Year", y = "Average annual work hours of production workers", color = "Country") +
  scale_color_d3() +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        text = element_text(size = 15),
        legend.position = c(0.9, 0.84),
        panel.grid.minor = element_blank()
        )

ggsave("indmarketdemand/veblen_bowles_euro.pdf", plot = p1, width = 7, height = 7)
  
# Non-Euro ----------------------------------------------------------------

p2 <- JEBO_chart_data %>% 
  filter(!name %in% europe) %>% 
  ggplot(aes(x = year,y = hour, color = name)) +
  geom_line() + 
  geom_point() +
  labs(x = "Year", y = "Average annual work hours of production workers", color = "Country") +
  scale_color_d3() +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        text = element_text(size = 15),
        legend.position = c(0.9, 0.875),
        panel.grid.minor = element_blank()
  )

ggsave("indmarketdemand/veblen_bowles_noneuro.pdf", plot = p2, width = 7, height = 7)
