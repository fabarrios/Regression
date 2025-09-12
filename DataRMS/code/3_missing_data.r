## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet()


## ----eval=FALSE---------------------------------------------------------------
## a <- aregImpute(~ age + sex + bp + death +
##                 heart.attack.before.death,
##                 data=mydata, n.impute=5)
## f <- fit.mult.impute(death ~ rcs(age,3) + sex +
##                      rcs(bp,5), lrm, a, data=mydata)


## ----eval=FALSE---------------------------------------------------------------
## support <- transform(support,
##                      totcst = ifelse(is.na(totcst),
##                        (expression_in_charges), totcst))

