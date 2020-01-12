library(tidyverse)
library(readxl)

# Import
Unit_17_data_file_for_charts <- read_excel("capitalism/data/Unit-17-data-file-for-charts.xlsx", 
                                           sheet = "D - Fig 17 - Wages and product", 
                                           skip = 13)

# Clean, wide-to-long
wage_prod <- 
  Unit_17_data_file_for_charts[-c(1:12), -c(2:8, 10, 12:14)] %>%
  rename(productivity = `Combined manufacturing productivity series`, 
         real_wages = `For chart: Manufacturing real wages`,
         year = Variable) %>% 
  gather(measure, index, 2:3)

# colors 

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

# for every nth year
every_nth <- function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

# plot

wage_prod <- 
  wage_prod %>% 
  mutate(year = parse_number(year), 
         index = as.double(index))

wage_prod_plot <- 
  wage_prod %>%
  ggplot() +
  geom_line(aes(
    x = year,
    y = index,
    group = measure,
    color = measure
  )) +
  #scale_x_discrete(breaks = every_nth(n = 5)) +
  scale_color_manual(values = c(COLA[4], COLB[4]),
                     labels = c("Productivity", "Real Wages")) +
  labs(x = "Year",
       y = "Index (1949 = 100)") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 18),
    legend.title = element_blank(),
    #legend.position = c(0.15, 0.85),
    legend.background = element_rect(
      linetype = 1,
      size = 0.25,
      color = 1
    )
  ) 

wage_prod_plot <- 
  wage_prod_plot + 
  annotate("text", x = 1960, y = 680, label = "Golden age")
wage_prod_plot 

ggsave(
  plot = wage_prod_plot,
  filename = "capitalism/wage_prod.pdf",
  device = "pdf",
  width = 9,
  height = 7
)


  