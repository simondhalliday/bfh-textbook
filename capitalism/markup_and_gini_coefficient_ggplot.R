#' Graph Designer: Harriet Brookes-Gray, Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("tidyverse")
library("ggplot2")
library("dplyr")
library("patchwork") # To display 2 charts together
library("readxl")

markup_data <- read_excel("competitionmarkets/loecker_barkai/LBdf_data.xlsx")
gini_data <- read_excel("competitionmarkets/Gini Index Data/gini_data_USA.xlsx")


# Clean -------------------------------------------------------------------

gini_data$gini <- as.numeric(as.character(gini_data$gini))

markup_data1 <- markup_data[-c(53:61), ]

markup_data2 <- markup_data1 %>%
  filter(year >= 1959.023)

gini_data1 <- gini_data %>%
  filter(year >= 1960)


# Plot --------------------------------------------------------------------

coeff <- 1 # used to transform data

p <- ggplot() +
  geom_line(data = markup_data2, aes(x = year, y = markup/coeff, color = "Markup"), na.rm = TRUE, color = "#377EB8") +
  geom_line(data = gini_data1, aes(x = year, y = gini, color = "Gini"), color = "#E41A1C") +
  scale_y_continuous(
    
    # Name of the first axis
    name = "Markup",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name = "Gini")
  )

p1 <- p + 
  labs(color = "") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14))
p1
# Save
ggsave(p, "markup_and_gini_coefficient.pdf", width = 6, height = 6, units = "in")
