# Regression in Biostatistics
## F.A. Barrios  
### Linear model.  

1. Using the `WCGS` data for middle-aged men at risk for heart disease, fit a multipredictor model for total cholesterol (`chol`) that includes the binary predictor `arcus`, which is coded *1* for the group with arcus senilis, a milky ring in the iris associated with high cholesterol levels, and *0* for the reference group. Save the fitted values. Now refit the model with the code for the reference group changed to *2*. Compare the coefficients, standard errors, P-values, and fitted values from the two models. The `WCGS` data are available at ../DataRegressBook/Chap2/wcgs.cvs  
2. Using the `WCGS` data referenced in problem 1, extract the fitted values from the multipredictor linear regression model for cholesterol and show that the square of the sample correlation between the fitted values and the outcome variable is equal to *R^2*.  
Estimate using R, the model  `chol ~ bmi + age + smoking + arcus`  Estimate *sqrt(R^2)* comparing the correlation between *Y* and *Yhat*
3. Using R and the `WCGS` data set referenced above in Problem 1, verify that you get equivalent results from:  
A t-Test and a simple linear model with one binary predictor (use `chor ~ arcus`), and One-way ANOVA and a linear model with one multilevel categorical predictor, (use `chor ~ agec`) age category.  