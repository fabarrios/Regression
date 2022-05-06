# Logistic regression using rms
# 
library(rms)
library(car)
library(tidyverse)

# Example from the WCGS data

wcgs <- read_csv("~/Dropbox/GitHub/Regression/DataRegressBook/Chap2/wcgs.csv",
                 show_col_types = FALSE)

wcgs <- mutate(wcgs, chd69 = factor(chd69))
wcgs_n <- wcgs %>% filter(chol < 645)

datadist(wcgs_n)
dd_CHDwcgs_n <- datadist(wcgs_n)
options(datadist = "dd_CHDwcgs_n", na.action = "na.delete")

CHD_lrmMod <- lrm(chd69 ~ age + chol + bmi + sbp + smoke, data = wcgs_n)
print(CHD_lrmMod)
summary(CHD_lrmMod)

wcgs_n <- mutate(wcgs_n, PredCHD_lrm = predict(CHD_lrmMod))

ggplot(wcgs_n, aes(x = age, y = chd69, color = smoke)) +
  geom_smooth(aes(y = PredCHD_lrm, fill = smoke), size = 0.75, alpha = .15)