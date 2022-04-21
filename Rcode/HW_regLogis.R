# HW
#
# Prob 1
library(car)
library(tidyverse)

wcgs <- read_csv("https://raw.githubusercontent.com/fabarrios/Regression/master/DataRegressBook/Chap2/wcgs.csv",
                 show_col_types = FALSE)

wcgs <- mutate(wcgs, chd69 = factor(chd69))
wcgs_n <- wcgs %>% filter(chol < 645)

CHD_glm02 <- glm(chd69 ~ age + chol + bmi + sbp + smoke, 
                 family = binomial(link = logit), data = wcgs_n)
S(CHD_glm02)
coefficients(CHD_glm02)
DataPoints <- data.frame(age=60, chol=c(250,200), bmi=20, sbp=150, smoke="Yes")
# In log-odds scale
predict(CHD_glm02, newdata = DataPoints, se.fit = TRUE)
# now by hand 
DataPoint_dir <- -12.31098987 + 0.06444760*60 + 
  0.01074125*250 + 0.05743605*20 + 0.01929379*150 + 0.63447782
# the difference in log-odds are -1.081554 + 1.618616 and beta_chol*50 is 0.01074125*50

# In linear predictor scale
predict(CHD_glm02, newdata = DataPoints, type = "response", se.fit = TRUE)
DataPoints_dir_ex <- 1/(1+exp(-DataPoint_dir))

# odds change as exp
exp(0.01074125*50) # the chances increase 1.710973 times for a 50 increase in chol

# ggplot the model CHD_glm02
wcgs_n$predCHDglm02 <- predict(CHD_glm02)

ggplot(wcgs_n, aes(x = age, y = chd69, color = smoke)) +
  geom_smooth(aes(y = predCHDglm02, fill = smoke), size = 0.75, alpha = .15)

ggplot(wcgs_n, aes(x = sbp, y = chd69, color = smoke)) +
  geom_smooth(aes(y = predCHDglm02, fill = smoke), size = 0.75, alpha = .15)

# Problem 2
# center all the variables and scale
wcgs_n <- mutate(wcgs_n, age_c = age - mean(age, na.rm = TRUE))
wcgs_n <- mutate(wcgs_n, age_10 = age_c/10)

wcgs_n <- mutate(wcgs_n, chol_c = chol - mean(chol, na.rm = TRUE))
wcgs_n <- mutate(wcgs_n, chol_50 = chol_c/50)

wcgs_n <- mutate(wcgs_n, bmi_c = bmi - mean(bmi, na.rm = TRUE))
wcgs_n <- mutate(wcgs_n, bmi_10 = bmi_c/10)

wcgs_n <- mutate(wcgs_n, sbp_c = sbp - mean(sbp, na.rm = TRUE))
wcgs_n <- mutate(wcgs_n, sbp_50 = sbp_c/50)

CHD_glm03 <- glm(chd69 ~ age_10 + chol_50 + bmi_10 + sbp_50 + smoke, 
                family = binomial(link = logit), data = wcgs_n)
S(CHD_glm03)
coefficients(CHD_glm03)
# scaled points
age_60_scal <- ( 60 - mean(wcgs_n$age,na.rm=TRUE) )/10
chol_200_scal <- ( 200 - mean(wcgs_n$chol, na.rm=TRUE) )/50
chol_250_scal <- ( 250 - mean(wcgs_n$chol, na.rm=TRUE) )/50
bmi_20_scal <- ( 20 - mean(wcgs_n$bmi,na.rm=TRUE) )/10
sbp_150_scal <- ( 150 - mean(wcgs_n$sbp,ra.rm=TRUE) )/50
# to pass the predict points
DataPoints_scal <- data.frame(age_10 = age_60_scal, chol_50 = c(chol_250_scal, chol_200_scal), 
                              bmi_10 = bmi_20_scal, sbp_50 = sbp_150_scal, smoke="Yes")
# estimated odds for this model for the smoker 60 yo 
predict(CHD_glm03, newdata = DataPoints_scal, type = "response", se.fit = TRUE)

CHD_glm04 <- glm(chd69 ~ age_10 + chol_50 + bmi_10 + sbp_50 + smoke + behpat, 
                 family = binomial(link = logit), data = wcgs_n)
S(CHD_glm04)