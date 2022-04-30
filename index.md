# Regression in Biostatistics  
## Universidad Nacional Autónoma de México, Instituto de Neurobiología  
### Michael C. Jeziorski, PhD. & Fernando A. Barrios, PhD.  
#### contact: jeziorsk@unam.mx & barrios@inb.unam.mx     

## Introduction  
This is the repository for the materials, homework and other files for the Regression class, almost all the examples that I will deposit here are examples using R and you can find some in my GitHub account and some Rmarkdown versions of them. These pages contain some solved problems and advanced examples from statistics and different regression models, many examples are taken from the Daniel's Biostatistics for the first subjects, nevertheless the problems for different regression models are taken from the more advanced books. All the examples are solved using R ([CRAN](https://cran.r-project.org/))and different R packages, running over [RStudio](https://rstudio.com/products/rstudio/), the idea is to use this site as support of the course I teach at UNAM. If you need a refreshment in general statistics you can check my undergraduate [statistics course](https://fabarrios.github.io/ProbEstad/) in the WEB.  I also have some [notes](https://fabarrios.github.io/AdvExamples/Notes/Notes) that have links to different pages and very useful material in particuar a list of useful [R packages](https://fabarrios.github.io/Regression/Libraries). For the problems presented here we will start with data manipulation and some t-test, ANOVA and regression examples. Then we will continue with more complex ideas.  

## Program  
### I Data manipulation (data wrangling and visualisation in R (Jeziorski)  
After loading data in R, the first step is to understand the structure of and format the data is, and restructure its order if needed to use the 
different R functions and the needed libraries, for the *post hoc* analysis.  Therefore we will start the course with "tidy" gymnastics, the pipe, filters and funcitons to work with "tibbles" and then visualization with ggplot.
1. Tidyverse, tibbles  
3. Pipe, filters, functions (on tibbles)  
4. ggplot. Data wrangling and visualization  

### II Introduction (Barrios)  
Revisiting some concepts of classical statistics  
We will cover some examples of t-Test problem solving with R, using some of the R libraries and then we will cover different ANOVA models in R too, to finish the refresh of statistics we will solve some simple linear regression examples, in R.
1. [t-Test and ANOVA](https://fabarrios.github.io/Regression/Exampl_t_ANOVA.html)  
2. [Linear Regression](https://fabarrios.github.io/Regression/LinearModel/LinearModel.html)  

### III Multiple Linear Regression (Barrios)  
1. [Multiple Linear Regression](https://fabarrios.github.io/Regression/MultipleLinearRegression/MultipleLinearRegression.html)
2. [Multiple Linear Models Comparisons](https://fabarrios.github.io/Regression/LinModTests/LinModTests.html)
3. [Multiple Linear Regression and Interactions](https://fabarrios.github.io/Regression/MultipredictorInter/MultipredictorInter.html)

### IV Logistic Regression (Barrios)  
1. [Introduction to logistic regression](https://fabarrios.github.io/Regression/LogisticReg/LogisticReg.html)  
2. [Logistic regression HSAUR example](https://fabarrios.github.io/Regression/LogisticExamp02/LogisticExamp02.html)

### V Survival Analysis (Barrios)  
1. [Introduction to Survival Analysis](https://fabarrios.github.io/Regression/Survival/IntroSurvival.html)
2. [Survival Analysis HSAUR example](https://fabarrios.github.io/Regression/Survival/Survival.html)

### VI Clustering (Jeziorski)  

### VII Maximum Likelihood Estimation (Barrios)  

### Homework  
1. ANOVA Refresher homework [HW_ANOVA](https://fabarrios.github.io/Regression/HW/HW_ANOVA)
2. Linear Model Homework [HW_LM](https://fabarrios.github.io/Regression/HW/HW_LM)
3. Logistic Regression Homework [HW_LogstReg](https://fabarrios.github.io/Regression/HW/HW_RegLogist)
4. Survival Regression Homework [HW_HW_Survival](https://fabarrios.github.io/Regression/HW/HW_Survival)


## Bibliography  
1. Harrell, F.E. (2015) Regression Modeling Strategies (2nd edition), Springer Series in Statistics, Springer.  
2. Vittinghoff, E., Glidden, D.V., Shiboski, S.C., & McCulloch, C.E. (2012) Regression Methods in Biostatistics (2nd edition), Springer Series in Statistics, Springer.  
3. Fox J. & Weisberg, S. (2019) An R Companion to Applied Regression, 3rd Ed. SAGE.  
4. Vehkalahti, K. & Everitt, B.S. (2020) Multivariate Analysis for the Behavioral Sciences, 2nd Ed. CRC Press, Taylor & Francis.  
5. Hothorn, T. & Everitt, B.S. (2014) A Handbook of Statistical Analyses Using R, 3rd Ed. CRC Press, Chapman & Hall.  
6. Harrison, E. & Pius, R. (2021) R for Health Data Science, CRC Press, Taylor and Francis Group.  
7. Crawley, M.J. (2015) Statistics An Introduction using R, Second Edition, John Wiley & Sons Ltd.  
