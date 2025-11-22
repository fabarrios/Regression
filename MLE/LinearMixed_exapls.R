library(tidyverse)

# read data
BPRS <- read_csv("~/Dropbox/GitHub/Regression/DataDavis/Woolson1.csv", 
                 show_col_types = FALSE)

# Convert ot factor the Treatment and Subjerct col
BPRS <- BPRS %>% mutate(Subject = factor(Subject))
BPRS <- BPRS %>% mutate(Group = factor(Group))

BPRS_long <- BPRS %>% pivot_longer(cols = c("Week1", "Week2", "Week3", "Week4", "Week5", "Week6", "Week7", "Week8"), 
                                   names_to = "Week", values_to = "bprs")

# Elongated BPRS to graph the data over the weeks including the Week0 and line per Subject
BPRSL_graph <- BPRS %>% pivot_longer(cols = c("Week0", "Week1", "Week2", "Week3", "Week4", "Week5", "Week6", "Week7", "Week8"),
                                   names_to = "Week", values_to = "bprs")

BPRSL_graph %>% ggplot(aes(x = Week, y = bprs, color = Subject, group = Subject)) +
  geom_line() + scale_linetype_manual(values = rep(1:10, times = 4)) +
  facet_grid(. ~ Treatment, labeller = label_both) +
  theme(legend.position = "none") + theme_bw() +
  theme(panel.grid.minor.x = element_blank()) +
  scale_y_continuous(limits = c(min(BPRSL_graph$bprs), max(BPRSL_graph$bprs)))
                                  
BPRSL_graph %>% ggplot(aes(x= Week, y= bprs, fill = Treatment)) +
          geom_boxplot(position = position_dodge(width = 0.8)) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          theme_bw() + theme(legend.position = c(0.9, 0.9)) +
          labs(title = "Brief Psychiatric Rating Scale (BPRSL) data")
                     
