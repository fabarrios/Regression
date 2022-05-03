# Logistic regression using rms
# 

# Example from the WCGS data

wcgs <- read_csv("~/Dropbox/GitHub/Regression/DataRegressBook/Chap2/wcgs.csv",
                 show_col_types = FALSE)

wcgs <- mutate(wcgs, chd69 = factor(chd69))
wcgs_n <- wcgs %>% filter(chol < 645)

datadist(wcgs_n)
dd_CHDwcgs_n <- datadist(wcgs_n)
options(datadist = 'dd_CHDwcgs_n')

CHD_lrmMod <- lrm(chd69 ~ age + chol + bmi + sbp + smoke, data = wcgs_n)
print(CHD_lrmMod)
summary(CHD_lrmMod)

# wcgs_n <- mutate(wcgs_n, PredCHD_lrm = Predict(CHD_lrmMod))

