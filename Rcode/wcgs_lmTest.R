# Examples for more linear models WCGS data
# Loading data  
library(multcomp)
library(here)
library(lmtest)
library(tidyverse)

#
WCGS <- read_csv("https://raw.githubusercontent.com/fabarrios/Regression/master/DataRegressBook/Chap2/wcgs.csv",
                 show_col_types = FALSE)

# A1 personality trit
names(WCGS)
WCGS_n <- WCGS %>% mutate( arcus <- factor(arcus))
WCGS_n <- WCGS_n %>% 
    mutate(smoke = smoke %>% fct_relevel("No", "Yes"))

# Model of the cholesterol vs BMI in middle aged men
cholBmi_mod1 <- lm(chol ~ bmi + arcus, data = WCGS_n)
summary(cholBmi_mod1)

# Plot of model1
ggplot(data = WCGS_n, mapping = aes(x = bmi, y = chol)) + 
  labs(x = "Body Mass Index kg/m^2", 
       y = "Total Cholesteron mg/dL", 
       title = "Linear model") + 
  geom_point(aes(colour = arcus), shape = 20, alpha = 0.3) + 
  geom_smooth(method = "lm", formula = y ~ x)

# Multipredictor model2
cholBmi_mod2 <- lm(chol ~ bmi + arcus + age, data = WCGS)
summary(cholBmi_mod2)

# Non linear testing the model between chol ~ age 
chol_age_mod <- lm(chol ~ age, data = WCGS_n)
raintest(cholBmi_mod1, fraction = 0.5, order.by = "mahalanobis", data = WCGS_n)

# Comparing model2 (fullmodel) with model1 (nested linear model)
# likelihood ratios test
lrtest(cholBmi_mod2, cholBmi_mod1)
