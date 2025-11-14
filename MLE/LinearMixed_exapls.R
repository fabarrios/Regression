library(tidyverse)

# read data
BPRS <- read_csv("~/Dropbox/GitHub/Regression/DataDavis/Woolson1.csv", 
                 show_col_types = FALSE)

# Convert ot factor the Treatment and Subjerct col
BPRS <- BPRS %>% mutate(Subject = factor(Subject))
BPRS <- BPRS %>% mutate(Group = factor(Group))

BPRS_long <- BPRS %>% pivot_longer(cols = c("Week1", "Week2", "Week3", "Week4", "Week5", "Week6", "Week7", "Week8"), 
                                   names_to = "Week", values_to = "bprs")

# To plot the data over the weeks including the Week0 and line per Subject
BPRSL_new <- BPRS %>% pivot_longer(cols = c("Week0", "Week1", "Week2", "Week3", "Week4", "Week5", "Week6", "Week7", "Week8"),
                                   names_to = "Week", values_to = "bprs")

BPRSL_new %>% ggplot(aes(x = Week, y = bprs, color = Subject, group = Subject)) +
  geom_line() + scale_linetype_manual(values = rep(1:10, times = 4)) +
  facet_grid(. ~ Treatment, labeller = label_both) +
  theme(legend.position = "none") + theme_bw() +
  theme(panel.grid.minor.x = element_blank()) +
  scale_y_continuous(limits = c(min(BPRSL_new$bprs), max(BPRSL_new$bprs)))
                                  
BPRSL_new %>% ggplot(aes(x= Week, y= bprs, linetype = Subject, color = Subject)) +
                     geom_boxplot(position = position_dodge(width = 0.9)) +
                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                     theme(legend.position = c(0.8, 0.8)) + theme_bw() +
                     labs(title = "BPRS data")
                     
