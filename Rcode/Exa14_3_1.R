# Survival example Daniel 14.3.1
library(survival)
library(survminer)
library(car)
library(tidyverse)

tumorG <- read_csv("~/Dropbox/GitHub/Regression/DataSets/ch14_11/EXA_C14_S03_01_mod.csv",
                  show_col_types = FALSE)
tumorG_n <- tumorG %>% mutate( TUMOR = TUMOR %>% fct_relevel("L","H") )

plot(survfit(Surv(TIME, VITAL) ~ TUMOR, data = tumorG_n), mark.time = TRUE,
     main = "Malignant tumors of the sternum", lty = c(1,1), col = c(2,4),
     cex = 1, ylab = "Probability", xlab = "Time (months)")

SurvivalModel_tumor <- survfit(Surv(TIME, VITAL) ~ TUMOR, data = tumorG_n)
survdiff(Surv(TIME, VITAL) ~ TUMOR, data = tumorG_n)

# Other possibility
tumorG2 <- tumorG %>% mutate( TUMOR = TUMOR %>% fct_relevel("L","H") )
tumorG_n2 <- tumorG2 %>% mutate( VITAL = factor(VITAL) %>% 
                                  fct_recode("0" = "ned",
                                             "1" = "dod",
                                             "1" = "dpo") )
plot(survfit(Surv(TIME, VITAL, type = "right") ~ TUMOR, data = tumorG_n2), mark.time = TRUE,
     main = "Malignant tumors of the sternum", lty = c(1,1), col = c(2,4),
     cex = 1, ylab = "Probability", xlab = "Time (months)")
SurvivalModel_tumor <- survfit(Surv(TIME, VITAL, type = "right") ~ TUMOR, data = tumorG_n2)