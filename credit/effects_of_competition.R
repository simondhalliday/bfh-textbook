#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("tidyverse")
library("RColorBrewer")

# dataframe ---------------------------------------------------------------

df <- tibble(
  pr_ag = c("Borrower", "Lender"),
  val_sm_bar = c(25, 0.65),
  val_sm_pie = c(62.5, 25),
  val_lg_bar = c(122.5, 5), 
  val_lg_pie = c(5, 1.225)
)

# plots -------------------------------------------------------------------

# Fig a
p_sm_bar <- ggplot(df, aes(pr_ag, val_sm_bar)) +
  geom_col(aes(fill = pr_ag)) + 
  scale_fill_brewer(palette = "Set1") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("credit/effects_of_competition_a.pdf", width = 9, height = 7)

# Fig b
p_sm_pie <- ggplot(df, aes("", y = val_sm_pie, fill = pr_ag))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = val_sm_pie), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") + 
  labs(caption = "p_sm_pie") +
  theme_blank() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank())

ggsave("credit/effects_of_competition_b.pdf", width = 9, height = 7)

# Fig c
p_lg_pie <- ggplot(df, aes("", y = val_lg_pie, fill = pr_ag))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = val_lg_pie), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") + 
  labs(caption = "p_lg_pie") +
  theme_blank() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank())

ggsave("credit/effects_of_competition_c.pdf", width = 9, height = 7)

# Fig d
p_lg_bar <- ggplot(df, aes(pr_ag, val_lg_bar)) +
  geom_col(aes(fill = pr_ag)) + 
  scale_fill_brewer(palette = "Set1") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("credit/effects_of_competition_d.pdf", width = 9, height = 7)



