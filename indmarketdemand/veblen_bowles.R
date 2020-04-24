# Graph Designer: Scott Cohn
# Authors: Bowles and Halliday
# Title: Microeconomics: Competition, Conflict and Coordination

library(tidyverse) # ggplot
library(ggsci)     # d3 colors
library(readxl)    # import

# Import Data
JEBO_chart_data <- read_excel("~/Downloads/JEBO chart data.xlsx")

# Note countries codes are not iso3c or iso2c consistent/nor accurate
# Changed manually
JEBO_chart_data$fullname[JEBO_chart_data$name == 'AUS'] <- 'Australia'
JEBO_chart_data$fullname[JEBO_chart_data$name == 'CAN'] <- 'Canada'
JEBO_chart_data$fullname[JEBO_chart_data$name == 'FRA'] <- 'France'
JEBO_chart_data$fullname[JEBO_chart_data$name == 'US'] <- 'United States'
JEBO_chart_data$fullname[JEBO_chart_data$name == 'SWE'] <- 'Sweden'
JEBO_chart_data$fullname[JEBO_chart_data$name == 'SWI'] <- 'Switzerland'
JEBO_chart_data$fullname[JEBO_chart_data$name == 'JPN'] <- 'Japan'
JEBO_chart_data$fullname[JEBO_chart_data$name == 'NET'] <- 'Netherlands'
JEBO_chart_data$fullname[JEBO_chart_data$name == 'GER'] <- 'Germany'
JEBO_chart_data$fullname[JEBO_chart_data$name == 'UK'] <- 'United Kingdom'

europe <- c("FRA", "GER", "NET", "SWE", "UK", "SWI")

# Euro --------------------------------------------------------------------

p1 <- JEBO_chart_data %>% 
  filter(name %in% europe) %>% 
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = hour, group = fullname, color = fullname)) +
  geom_point() +
  geom_line() + 
  labs(x = "Year", y = "Average annual work hours of production workers", color = "Country") +
  #scale_color_manual(values = c("France", "Germany", "Netherlands", "Sweden", "Switzerland", "United Kingdom")) +
  scale_color_d3() +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        text = element_text(size = 15),
        legend.position = c(0.85, 0.84),
        panel.grid.minor = element_blank()
        )

ggsave("indmarketdemand/veblen_bowles_euro.pdf", plot = p1, width = 7, height = 7)
  
# Non-Euro ----------------------------------------------------------------

p2 <- JEBO_chart_data %>% 
  filter(!name %in% europe) %>% 
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year,y = hour, group = fullname, color = fullname)) +
  geom_line() + 
  geom_point() +
  labs(x = "Year", y = "Average annual work hours of production workers", color = "Country") +
  scale_color_d3() +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        text = element_text(size = 15),
        legend.position = c(0.85, 0.875),
        panel.grid.minor = element_blank()
        )

ggsave("indmarketdemand/veblen_bowles_noneuro.pdf", plot = p2, width = 7, height = 7)
