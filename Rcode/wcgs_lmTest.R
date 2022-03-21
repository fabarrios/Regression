# Examples for more linear models WCGS data
# Loading data  
library(emmeans)
library(multcomp)
library(here)
library(tidyverse)

#
WCGS <- read_csv("https://raw.githubusercontent.com/fabarrios/Regression/master/DataRegressBook/Chap2/wcgs.csv",
                 show_col_types = FALSE)

# A1 personality trit
names(WCGS)

# Model of the cholesterol vs BMI in middle aged men

cholBmi_mod1 <- lm(chol ~ bmi, data = WCGS)
summary(cholBmi_mod1)


cholBmi_mod2 <- lm(chol ~ bmi + arcus + smoke, data = WCGS)
summary(cholBmi_mod2)