#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics
#' 
#' TODO: 
#' 1. the colors should be consistent the bar graphs should be much smaller and thinner.
#' 
#' 2. the size of thepie should equal the total payoffs, 
#' e.g. in the pie on the left (no competition) 62.5+25 = 87.5 
#'  
#' 3. the numbers are screwed up. in the pie on the right the 
#' shares are 122.5 for the borrowers, 5 for the lender
#' 
#' 4. the slices of the pies should have labels like “100 borrowers 
#' (122.5)” and ” the lender (5) ” and similarly for the other pie.  
#' 
#' 5. no need for a vertical axis label on the bar graphs. 
#' 
#' 6. there should be numbers at the top of the horizontal bars (but they 
#' should be 15 and 0.625 in one case and 5 and 1.225 in the other ( your second one is wrong)
#' 
#' 7. it might help of ou have the figs informative names, like from left to right 
#' in my sktech below: Nonebar, Nonepie, CompPie and Compbar.  
#' 
#' 8. they should eventually be laid out with the bar and the pie charts side 
#' by side, either arrayed from L to R s in my sketch, or in two rows. 

library("tidyverse")
library("RColorBrewer")

# dataframe ---------------------------------------------------------------

df <- tibble(
  pr_ag = c("Borrower", "Lender"),
  val_sm_bar = c(25, 0.65),
  val_sm_pie = c(62.5, 25),
  val_lg_bar = c(5, 1.225), 
  val_lg_pie = c(5, 122.5)
)

# plots -------------------------------------------------------------------

# Fig a -- nonebar
p_sm_bar <- ggplot(df, aes(pr_ag, val_sm_bar)) +
  geom_col(aes(fill = pr_ag)) + 
  scale_fill_brewer(palette = "Set1") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("credit/effects_of_competition_a.pdf", width = 6, height = 6)

# Fig b -- nonepie
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

ggsave("credit/effects_of_competition_b.pdf", width = 6, height = 6)

# Fig c -- comppie
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

ggsave("credit/effects_of_competition_c.pdf", width = 6, height = 6)

# Fig d -- compbar
p_lg_bar <- ggplot(df, aes(pr_ag, val_lg_bar)) +
  geom_col(aes(fill = pr_ag)) + 
  scale_fill_brewer(palette = "Set1") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("credit/effects_of_competition_d.pdf", width = 6, height = 6)



