#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("tidyverse")
library("gridExtra")
library("forcats")
library("RColorBrewer")

# dataframe ---------------------------------------------------------------

df <- tibble(
  pr_ag = c("Borrower", "Lender"),
  val_sm_bar = c(0.65, 25),
  val_sm_pie = c(62.5, 25),
  val_lg_bar = c(1.225, 5), 
  val_lg_pie = c(122.5, 5)
)

df_long <- df %>% 
  #select(c("pr_ag", "val_sm_pie", "val_lg_pie")) %>% 
  gather(key = "group", value = "value", 2:5) %>% 
  mutate(condition = c("Low competition", "Low competition", "sm", "sm", "High competition", "High competition", "lg", "lg"))


# plots -------------------------------------------------------------------

# Fig a -- nonebar
# p_sm_bar <- ggplot(df, aes(pr_ag, val_sm_bar)) +
#   geom_col(aes(fill = pr_ag)) + 
#   geom_text(aes(label = val_sm_bar), vjust = -0.5, size = 5) + 
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw() + 
#   theme(legend.position = "none",
#         axis.title = element_blank(),
#         axis.text = element_text(size = 15))
# 
# ggsave("credit/effects_of_comp_nonebar_a.pdf", width = 3, height = 5)

# Fig b -- nonepie
# p_sm_pie <- ggplot(df, aes("", y = val_sm_pie, fill = pr_ag))+
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y") +
#   geom_text(aes(label = val_sm_pie), 
#             position = position_stack(vjust = 0.5)) +
#   scale_fill_brewer(palette = "Set1") + 
#   # labs(caption = "p_sm_pie") +
#   theme_blank() +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         legend.title = element_blank(),
#         plot.margin=grid::unit(c(0,0,0,0), "mm"))
# 
# ggsave("credit/effects_of_comp_nonepie_b.pdf", width = 6, height = 6)

# Fig c -- comppie
# p_lg_pie <- ggplot(df, aes("", y = val_lg_pie, fill = pr_ag))+
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y") +
#   geom_text(aes(label = val_lg_pie), 
#             position = position_stack(vjust = 0.5)) +
#   scale_fill_brewer(palette = "Set1") + 
#   # labs(caption = "p_lg_pie") +
#   theme_blank() +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         legend.title = element_blank(),
#         plot.margin=grid::unit(c(0,0,0,0), "mm"))
# 
# ggsave("credit/effects_of_comp_comppie_c.pdf", width = 6, height = 6)

# Fig d -- compbar
# p_lg_bar <- ggplot(df, aes(pr_ag, val_lg_bar)) +
#   geom_col(aes(fill = pr_ag)) + 
#   geom_text(aes(label = val_lg_bar), vjust = -0.5, size = 5) + 
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw() + 
#   theme(legend.position = "none",
#         axis.title = element_blank(),
#         axis.text = element_text(size = 15))
# 
# ggsave("credit/effects_of_comp_compbar_d.pdf", width = 3, height = 5)


# stacked bar (replace pie?) ----------------------------------------------

# Middle Fig
stk_bar <- df_long %>% 
  filter(condition %in% c("sm", "lg")) %>% 
  mutate(b_l_label = c("62.5\nfor 100\nborrowers", "25 for 1\nlender", 
                       "122.5\nfor 100\nborrowers", "5 for 1 lender")) %>% 
  ggplot(aes(x = forcats::fct_rev(condition), y = value, fill = pr_ag)) + 
  geom_bar(position = "stack", stat = "identity") + 
  geom_text(aes(label = b_l_label), position = position_stack(vjust = 0.5)) + 
  # geom_text(aes(label = value)) +  
  scale_fill_brewer(palette = "Set1") + 
  scale_x_discrete(labels=c("sm" = "Low\ncompetition",
                            "lg" = "High\ncompetition")) + 
  ylab("Expected income") +
  # coord_flip() +
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        legend.position = "top",
        panel.grid.minor = element_blank())

ggsave("credit/effects_of_comp_stk_bar.pdf", width = 3, height = 5)

# Left Fig
low_comp_bar <- df_long %>% 
  filter(condition %in% c("Low competition")) %>% 
  ggplot(aes(x = pr_ag, y = value, fill = pr_ag)) + 
  geom_col() +
  geom_text(aes(label = value), vjust = -0.5, size = 4) +
  ylab("Expected income") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels=c("Lender" = "The lender",
                            "Borrower" = "One of 100\nborrowers")) + 
  ylim(0, 25) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        panel.grid.minor = element_blank()
        )

ggsave("credit/effects_of_comp_low_bar.pdf", width = 3, height = 5)

# Right Fig
high_comp_bar <- df_long %>% 
  filter(condition %in% c("High competition")) %>% 
  ggplot(aes(x = pr_ag, y = value, fill = pr_ag)) + 
  geom_col() +
  geom_text(aes(label = value), vjust = -0.5, size = 4) +
  ylab("Expected income") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels=c("Lender" = "The lender",
                            "Borrower" = "One of 100\nborrowers")) + 
  ylim(0, 25) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        panel.grid.minor = element_blank()
  )

ggsave("credit/effects_of_comp_high_bar.pdf", width = 3, height = 5)

# save panel --------------------------------------------------------------

# ggsave("credit/effects_of_comp_sm.pdf", arrangeGrob(p_sm_bar, p_sm_pie))
# ggsave("credit/effects_of_comp_lg.pdf", arrangeGrob(p_lg_bar, p_lg_pie))

# ^^ looks gross.


