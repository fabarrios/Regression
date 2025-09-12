## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('coxcase')


## -----------------------------------------------------------------------------
require(rms)
options(prType='latex')   # for print, summary, anova
getHdata(prostate)
levels(prostate$ekg)[levels(prostate$ekg) %in%
                     c('old MI','recent MI')] <- 'MI'
# combines last 2 levels and uses a new name, MI

prostate$pf.coded <- as.integer(prostate$pf)
# save original pf, re-code to 1-4
levels(prostate$pf)  <- c(levels(prostate$pf)[1:3],
                          levels(prostate$pf)[3]) 
# combine last 2 levels

w <- transcan(~ sz + sg + ap + sbp + dbp + age +
              wt + hg + ekg + pf + bm + hx,
              imputed=TRUE, data=prostate, pl=FALSE, pr=FALSE)

attach(prostate)
sz  <- impute(w, sz, data=prostate)
sg  <- impute(w, sg, data=prostate)
age <- impute(w, age,data=prostate)
wt  <- impute(w, wt, data=prostate)
ekg <- impute(w, ekg,data=prostate)

dd <- datadist(prostate); options(datadist='dd')

units(dtime) <- 'Month'
S <- Surv(dtime, status != 'alive')

f <- cph(S ~ rx + rcs(age,4) + rcs(wt,4) + pf + hx + 
         rcs(sbp,4) + rcs(dbp,4) + ekg + rcs(hg,4) + 
         rcs(sg,4) + rcs(sz,4) + rcs(log(ap),4) + bm)

## ----results="asis"-----------------------------------------------------------
print(f, coefs=FALSE)


## ----results="asis"-----------------------------------------------------------
heart <- hx + ekg %nin% c('normal','benign')
label(heart) <- 'Heart Disease Code'
map   <- (2*dbp + sbp)/3
label(map) <- 'Mean Arterial Pressure/10'
dd <- datadist(dd, heart, map)

f <- cph(S ~ rx + rcs(age,4) + rcs(wt,3) + pf.coded + 
         heart + rcs(map,3) + rcs(hg,4) + 
         rcs(sg,3) + rcs(sz,3) + rcs(log(ap),5) + bm,
         x=TRUE, y=TRUE, surv=TRUE, time.inc=5*12)
print(f, coefs=3)
# x, y for predict, validate, calibrate;
# surv, time.inc for calibrate
print(anova(f), table.env=TRUE, label='tab:coxcase-anova1')# Table (*\ref{tab:coxcase-anova1}*)


## ----rx-ph,cap='Raw and spline-smoothed scaled Schoenfeld residuals for dose of estrogen, nonlinearly coded from the Cox model fit, with $\\pm$ 2 standard errors.',scap='Schoenfeld residuals for dose of estrogen in Cox model'----
phtest <- cox.zph(f, terms=TRUE)
phtest
plot(phtest, var='rx')   # Figure (*\ref{fig:coxcase-rx-ph}*)


## ----results='asis'-----------------------------------------------------------
z <- predict(f, type='terms')
z.dose <- z[,"rx"]  # same as saying z[,1] - get first column
z.other <- z[,-1]   # all but the first column of z
f.ia <- cph(S ~ z.dose * z.other)   # Figure (*\ref{tab:coxcase-anova2}*):
print(anova(f.ia), table.env=TRUE, label='tab:coxcase-anova2')


## ----cox-shapes,h=6,w=6.75,cap='Shape of each predictor on log hazard of death.  $Y$-axis shows $X\\hat{\\beta}$, but the predictors not plotted are set to reference values.  Note the highly non-monotonic relationship with \\co{ap}, and the increased slope after age 70 which occurs in outcome models for various diseases.',scap='Shapes of predictors for log hazard in prostate cancer'----
ggplot(Predict(f), sepdiscrete='vertical', nlevels=4,
       vnames='names')  # Figure (*\ref{fig:coxcase-cox-shapes}*)


## ----results='asis'-----------------------------------------------------------
set.seed(1)  # so can reproduce results
v <- validate(f, B=300)
latex(v, file='')


## ----cal-cox,cache=TRUE,cap='Bootstrap estimate of calibration accuracy for 5-year estimates from the final Cox model, using adaptive linear spline hazard regression~\\cite{koo95haz}.  The line nearer the ideal line corresponds to apparent predictive accuracy.  The blue curve corresponds to bootstrap-corrected estimates.',scap='Bootstrap estimates of calibration accuracy in prostate cancer model'----
cal <- calibrate(f, B=300, u=5*12, maxdim=4)
plot(cal, subtitles=FALSE)   # Figure (*\ref{fig:coxcase-cal-cox}*)


## ----summary-cox,top=1,cap='Hazard ratios and multi-level confidence bars for effects of predictors in model, using default ranges except for \\co{ap}',scap='Hazard ratios for prostate survival model'----
plot(summary(f, ap=c(1,20)), log=TRUE, main='')  # Figure (*\ref{fig:coxcase-summary-cox}*)


## ----cox-nomogram,w=6,h=7,ps=8,cap='Nomogram for predicting death in prostate cancer trial'----
surv  <- Survival(f)
surv3 <- function(x) surv(3*12,lp=x)
surv5 <- function(x) surv(5*12,lp=x)
quan  <- Quantile(f)
med   <- function(x) quan(lp=x)/12
ss    <- c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95)

nom <- nomogram(f, ap=c(.1,.5,1,2,3,4,5,10,20,30,40),
                fun=list(surv3, surv5, med),
                funlabel=c('3-year Survival','5-year Survival',
                  'Median Survival Time (years)'),
                fun.at=list(ss, ss, c(.5,1:6)))
plot(nom, xfrac=.65, lmgp=.35)   # Figure (*\ref{fig:coxcase-cox-nomogram}*)

