# Survival example Daniel 14.3.1
library(survival)
library(survminer)
library(car)
library(tidyverse)

tumorG <- read_csv("~/Dropbox/GitHub/Regression/DataSets/ch14_11/EXA_C14_S03_01.csv",
                  show_col_types = FALSE)

# VITAL should not be factor the type flag does not work
tumorG <- tumorG %>% mutate( TUMOR = TUMOR %>% fct_relevel("L","H") )
tumorG_n <- tumorG %>% mutate( VITAL = case_when( 
                                            VITAL == "ned" ~ 0,
                                            VITAL == "dod" ~ 1,   
                                            VITAL == "dpo" ~ 1) )

plot(survfit(Surv(TIME, VITAL, type = "right") ~ TUMOR, data = tumorG_n), mark.time = TRUE,
     main = "Malignant tumors of the sternum", lty = c(1,1), col = c(2,4),
     cex = 1, ylab = "Probability", xlab = "Time (months)")

tumorFit <- survfit(Surv(TIME, VITAL, type = "right") ~ TUMOR, data = tumorG_n)
survdiff(Surv(TIME, VITAL) ~ TUMOR , data = tumorG_n)

# Using ggsurvplot
ggsurvplot(
  tumorFit, 
  data = tumorG_n, 
  size = 0.5,                 # change line size
  linetype = c("solid", "dashed"), # different line type
  palette = c("lancet"), # color red, blue or custom palettes lancet
  title    = "Malignant tumors of the sternum", # plot main title
  xlab = "Time (months)",   # customize X axis label.
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value from log-rank test
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",                  # Risk table color by groups
  legend.labs = c("Low-grade", "High-grade"), # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  surv.median.line = "hv",  # add the median survival pointer.
  ggtheme = theme_bw()      # Change ggplot2 theme
)

