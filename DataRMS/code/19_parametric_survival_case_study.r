## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('support')


## ----results='asis'-----------------------------------------------------------
require(rms)
options(prType='latex')    # for print, summary, anova
getHdata(support)          # Get data frame from web site
acute <- support$dzclass %in% c('ARF/MOSF','Coma')
latex(describe(support[acute,]), file='')


## ----naclus,w=7,h=5,ps=11,cap='Cluster analysis showing which predictors tend to be missing on the same patients',scap='Cluster analysis of missingness in SUPPORT'----
plot(naclus(support[acute,]))      # Figure (*\ref{fig:support-naclus}*)


## ----varclus,w=6.7,cap='Hierarchical clustering of potential predictors using Hoeffding $D$ as a similarity measure.  Categorical predictors are automatically expanded into dummy variables.',scap='Clustering of predictors in SUPPORT using Hoeffding $D$'----
ac <- support[acute,]
ac$dzgroup <- ac$dzgroup[drop=TRUE]    # Remove unused levels
label(ac$dzgroup) <- 'Disease Group'
attach(ac)
vc <- varclus(~ age + sex + dzgroup + num.co + edu + income +
              scoma + race + meanbp + wblc + hrt + resp +
              temp + pafi + alb + bili + crea + sod + ph +
              glucose + bun + urine + adlsc, sim='hoeffding')
plot(vc)                           # Figure (*\ref{fig:support-varclus}*)


## ----nikm,cap='$\\Phi^{-1}(S_{KM}(t))$ stratified by \\co{dzgroup}.  Linearity and semi-\\index{hypothesis test!equal slopes}parallelism indicate a reasonable fit to the log-normal accelerated failure time model with respect to one predictor.',scap='$\\Phi^{-1}(S_{KM}(t))$ stratified by \\co{dzgroup}'----
dd <- datadist(ac)
# describe distributions of variables to rms
options(datadist='dd')

# Generate right-censored survival time variable
years <- d.time/365.25
units(years) <- 'Year'
S <- Surv(years, death)

# Show normal inverse Kaplan-Meier estimates
# stratified by dzgroup
survplot(npsurv(S ~ dzgroup), conf='none',
         fun=qnorm,logt=TRUE)   # Figure (*\ref{fig:support-nikm}*)


## ----lognorm-resid, w=5.5, h=4, ps=7, lwd=1, top=2, mfrow=c(2,2), cap='Kaplan-Meier estimates of distributions of normalized, right-censored residuals from the fitted log-normal survival model.  Residuals are stratified by important variables in the model (by quartiles of continuous variables), plus a random variable to depict the natural variability (in the lower right plot).  Theoretical standard Gaussian distributions of residuals are shown with a thick solid line.',scap='Distributions of residuals from log-normal model'----
f <- psm(S ~ dzgroup + rcs(age,5) + rcs(meanbp,5), 
         dist='lognormal', y=TRUE)
r <- resid(f)

survplot(r, dzgroup, label.curve=FALSE)
survplot(r, age,     label.curve=FALSE)
survplot(r, meanbp,  label.curve=FALSE)
random <- runif(length(age)); label(random) <- 'Random Number'
survplot(r, random, label.curve=FALSE)  # Fig. (*\ref{fig:support-lognorm-resid}*)


## ----spearman, w=4.5, top=1, ps=10, rt=3, cap='Generalized Spearman $\\rho^2$ rank correlation between predictors and truncated survival time'----
shortest.follow.up <- min(d.time[death==0], na.rm=TRUE)
d.timet <- pmin(d.time, shortest.follow.up)

w <- spearman2(d.timet ~ age + num.co + scoma + meanbp + 
             hrt + resp + temp + crea + sod + adlsc + 
             wblc + pafi + ph + dzgroup + race, p=2)
plot(w, main='')        # Figure (*\ref{fig:support-spearman}*)


## ----rcorrcens, w=4.5, top=1, ps=10, rt=3, cap="Somers' $D_{xy}$ rank correlation between predictors and original survival time.  For \\co{dzgroup} or \\co{race}, the correlation coefficient is the maximum correlation from using a dummy variable to represent the most frequent or one to represent the second most frequent category.',scap='Somers' $D_{xy}$ rank correlation between predictors and original survival time"----
w <- rcorrcens(S ~ age + num.co + scoma + meanbp + hrt + resp +
               temp + crea + sod + adlsc + wblc + pafi + ph +
               dzgroup + race)
plot(w, main='')          # Figure (*\ref{fig:support-rcorrcens}*)


## -----------------------------------------------------------------------------
# Compute number of missing values per variable
sapply(llist(age,num.co,scoma,meanbp,hrt,resp,temp,crea,sod,
             adlsc,wblc,pafi,ph), function(x) sum(is.na(x)))
# Can also do naplot(naclus(support[acute,]))
# Can also use the Hmisc naclus and naplot functions
# Impute missing values with normal or modal values
wblc.i <- impute(wblc, 9)
pafi.i <- impute(pafi, 333.3)
ph.i   <- impute(ph,   7.4)
race2  <- race 
levels(race2) <- list(white='white',other=levels(race)[-1])
race2[is.na(race2)] <- 'white'
dd <- datadist(dd, wblc.i, pafi.i, ph.i, race2)


## -----------------------------------------------------------------------------
redun(~ crea + age + sex + dzgroup + num.co + scoma + adlsc +
      race2 + meanbp + hrt + resp + temp + sod + wblc.i +
      pafi.i + ph.i, nk=4)


## ----anovaSat,cap='Partial $\\chi^{2}$ statistics for association of each predictor with response from saturated main effects model, penalized for d.f.',scap='Partial $\\chi^{2}$ statistics from saturated main effects model'----
k <- 4
f <- psm(S ~ rcs(age,k)+sex+dzgroup+pol(num.co,2)+scoma+
         pol(adlsc,2)+race+rcs(meanbp,k)+rcs(hrt,k)+
         rcs(resp,k)+rcs(temp,k)+rcs(crea,3)+rcs(sod,k)+
         rcs(wblc.i,k)+rcs(pafi.i,k), dist='lognormal')
plot(anova(f))   # Figure (*\ref{fig:support-anovaSat}*)


## ----results='asis'-----------------------------------------------------------
f <- psm(S ~ rcs(age,5)+sex+dzgroup+num.co+
             scoma+pol(adlsc,2)+race2+rcs(meanbp,5)+
             rcs(hrt,3)+rcs(resp,3)+temp+
             rcs(crea,4)+sod+rcs(wblc.i,3)+rcs(pafi.i,4),
         dist='lognormal')
print(f, coefs=FALSE)
a <- anova(f)


## ----plot, h=7, w=7, cap='Effect of each predictor on log survival time.  Predicted values have been centered so that predictions at predictor reference values are zero.  Pointwise 0.95 confidence bands are also shown.  As all $y$-axes have the same scale, it is easy to see which predictors are strongest.',scap='Effect of predictors on log survival time in SUPPORT'----
ggplot(Predict(f, ref.zero=TRUE), vnames='names',
       sepdiscrete='vertical', anova=a)  # Figure (*\ref{fig:support-plot}*)


## ----results='asis'-----------------------------------------------------------
print(a, size='tsz')

## ----anova,cap='Contribution of variables in predicting survival time in log-normal model'----
plot(a)            # Figure (*\ref{fig:support-anova}*)

## ----summary, w=6.5, h=3.5, top=1, ps=11, cap='Estimated survival time ratios for default settings of predictors.  For example, when age changes from its lower quartile to the upper quartile (47.9y to 74.5y), median survival time decreases by more than half.  Different shaded areas of bars indicate different confidence levels (.9, 0.95, 0.99).',scap='Survival time ratios from fitted log-normal model'----
options(digits=3)
plot(summary(f), log=TRUE, main='')   # Figure (*\ref{fig:support-summary}*)


## ----results='asis',cache=TRUE------------------------------------------------
# First add data to model fit so bootstrap can re-sample
#  from the data
g <- update(f, x=TRUE, y=TRUE)
set.seed(717)
latex(validate(g, B=300), digits=2, size='Ssize')


## ----cal, cache=TRUE, cap='Bootstrap validation of calibration curve.  Dots represent apparent calibration accuracy; $\\times$ are bootstrap estimates corrected for overfitting, based on binning predicted survival probabilities and and computing Kaplan-Meier estimates.  Black curve is the estimated observed relationship using \\co{hare} and the blue curve is the overfitting-corrected \\co{hare} estimate.  The gray-scale line depicts the ideal relationship.',scap='Bootstrap validation of calibration curve for log-normal model'----
set.seed(717)
cal <- calibrate(g, u=1, B=300)
plot(cal, subtitles=FALSE)
cal <- calibrate(g, cmethod='KM', u=1, m=60, B=120, pr=FALSE)
plot(cal, add=TRUE)     # Figure (*\ref{fig:support-cal}*)


## -----------------------------------------------------------------------------
Z <- predict(f)    # X*beta hat
a <- ols(Z ~ rcs(age,5)+sex+dzgroup+num.co+
             scoma+pol(adlsc,2)+race2+
             rcs(meanbp,5)+rcs(hrt,3)+rcs(resp,3)+
             temp+rcs(crea,4)+sod+rcs(wblc.i,3)+
             rcs(pafi.i,4), sigma=1)
# sigma=1 is used to prevent sigma hat from being zero when
# R2=1.0 since we start out by approximating Z with all 
#  component variables
fastbw(a, aics=10000)    # fast backward stepdown


## -----------------------------------------------------------------------------
f.approx <- ols(Z ~ dzgroup + rcs(meanbp,5) + rcs(crea,4) +
                rcs(age,5) + rcs(hrt,3) + scoma +
                rcs(pafi.i,4) + pol(adlsc,2)+
                rcs(resp,3), x=TRUE)
f.approx$stats


## -----------------------------------------------------------------------------
V <- vcov(f,regcoef.only=TRUE)      # var(full model)
X <- cbind(Intercept=1, g$x)        # full model design
x <- cbind(Intercept=1, f.approx$x) # approx. model design
w <- solve(t(x) %*% x, t(x)) %*% X  # contrast matrix
v <- w %*% V %*% t(w)


## -----------------------------------------------------------------------------
f.sub <- psm(S ~ dzgroup + rcs(meanbp,5) + rcs(crea,4) +
             rcs(age,5) + rcs(hrt,3) + scoma + rcs(pafi.i,4) +
             pol(adlsc,2)+ rcs(resp,3), dist='lognormal')

diag(v)/diag(vcov(f.sub,regcoef.only=TRUE))
r <- diag(v)/diag(vcov(f.sub,regcoef.only=TRUE))
r[c(which.min(r), which.max(r))]


## ----results='asis'-----------------------------------------------------------
f.approx$var <- v
print(anova(f.approx, test='Chisq', ss=FALSE), size='tsz')


## ----results='asis'-----------------------------------------------------------
# Typeset mathematical form of approximate model
latex(f.approx)


## ----results='asis'-----------------------------------------------------------
# Derive S functions that express mean and quantiles
# of survival time for specific linear predictors 
# analytically
expected.surv <- Mean(f)
quantile.surv <- Quantile(f)
latex(expected.surv, file='', type='Sinput')
latex(quantile.surv, file='', type='Sinput')
median.surv   <- function(x) quantile.surv(lp=x)


## ----nomogram, h=6, w=6, ps=10, cap='Nomogram for predicting median and mean survival time, based on approximation of full model',scap='Nomogram for simplified log-normal model'----
# Improve variable labels for the nomogram
f.approx <- Newlabels(f.approx, c('Disease Group','Mean Arterial BP',
          'Creatinine','Age','Heart Rate','SUPPORT Coma Score',
          'PaO2/(.01*FiO2)','ADL','Resp. Rate'))
nom <-
  nomogram(f.approx, 
           pafi.i=c(0, 50, 100, 200, 300, 500, 600, 700, 800, 900),
           fun=list('Median Survival Time'=median.surv,
                   'Mean Survival Time'  =expected.surv),
           fun.at=c(.1,.25,.5,1,2,5,10,20,40))
plot(nom, cex.var=1, cex.axis=.75, lmgp=.25)
# Figure (*\ref{fig:support-nomogram}*)

