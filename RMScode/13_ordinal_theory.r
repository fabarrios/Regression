## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('tordinal')


## ----po-assumpts-support,h=3.5,w=3.5,ps=7,cap='Checking PO assumption separately for a series of predictors.  The circle, triangle, and plus sign correspond to $Y \\geq 1, 2, 3$, respectively.  PO is checked by examining the vertical constancy of distances between any two of these three symbols.  Response variable is the severe functional disability scale \\co{sfdm2} from the $1000$-patient SUPPORT dataset, with the last two categories combined because of low frequency of coma/intubation.',scap='Simple method for checking PO assumption using stratification'----
require(Hmisc)
getHdata(support)
sfdm <- as.integer(support$sfdm2) - 1
sf <- function(y)
  c('Y>=1'=qlogis(mean(y >= 1)), 'Y>=2'=qlogis(mean(y >= 2)),
    'Y>=3'=qlogis(mean(y >= 3)))
s <- summary(sfdm ~ adlsc + sex + age + meanbp, fun=sf,
             data=support)
plot(s, which=1:3, pch=1:3, xlab='logit', vnames='names',
     main='', width.factor=1.5)   # Figure (*\ref{fig:tordinal-po-assumpts-support}*)


## ----glyhb,h=3.25,w=6.5,cap='Transformed empirical cumulative distribution functions stratified by body frame in the \\co{diabetes} dataset.  Left panel: checking all assumptions of the parametric ANOVA.  Right panel: checking all assumptions of the PO model (here, Kruskal--Wallis test).',scap='Checking assumptions of PO and parametric model'----
getHdata(diabetes)
a <- Ecdf(~ log(glyhb), group=frame, fun=qnorm,
       xlab='log(HbA1c)', label.curves=FALSE, data=diabetes,
       ylab=expression(paste(Phi^-1, (F[n](x))))) # Fig. (*\ref{fig:tordinal-glyhb}*)
b <- Ecdf(~ log(glyhb), group=frame, fun=qlogis,
          xlab='log(HbA1c)', label.curves=list(keys='lines'),
          data=diabetes, ylab=expression(logit(F[n](x))))
print(a, more=TRUE, split=c(1,1,2,1))
print(b, split=c(2,1,2,1))


## ----eval=FALSE---------------------------------------------------------------
## y ~ cohort + X1 + X2 + X3 + ...


## ----eval=FALSE---------------------------------------------------------------
## y ~ cohort*(X1 + X2) + X3


## ----eval=FALSE---------------------------------------------------------------
## u <- cr.setup(Y)         # Y=original ordinal response
## attach(mydata[u$subs,])  # mydata is the original dataset
##                          # mydata[i,] subscripts input data,
##                          # using duplicate values of i for
##                          # repeats
## y      <- u$y            # constructed binary responses
## cohort <- u$cohort       # cohort or risk set categories
## f <- lrm(y ~ cohort*age + sex)

