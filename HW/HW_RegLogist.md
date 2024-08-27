# Regression in Biostatistics 2024
## F.A. Barrios  
### Logistic regression.  

1. For the fitted logistic regression model `chd69 ~ age + chol + bmi + sbp + smoke`, calculate the log odds for a 60-year-old smoker with cholesterol, SBP, and BMI values of 250 mg/dL, 150 mmHg, and 20, respectively. Now calculate the log odds for an individual with a cholesterol level of 200 mg/dL, holding the values of the other predictors fixed. Use these two calculations to estimate with identical values of the other predictors. Comment on any differences between the two estimated odds ratios.
2. Estimate a model to produce odds ratios for ten-year increases in `age`, rescaling `age_10 = age/10` and also "center" the variables (substracting the mean, before scaling), centering and scaling `chol` by 50, `bmi` by 10, `sbp` by 50. Estimate and describe the `chd69 ~ age_10 + chol_50 + bmi_10 + sbp_50 + smoke` model and its properties.
3. Estimate the `chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + behpat`, adding the WCGS behavior pattern variable and compare the model estimated in problem 2 as nested models using a lakelihood ratio test. Note that the behpat is a four-level categorical variable.
