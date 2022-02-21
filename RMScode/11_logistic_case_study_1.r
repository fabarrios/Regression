## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('lrcase1')


## -----------------------------------------------------------------------------
require(rms)
options(prType='latex')
getHdata(prostate)
prostate <-
  within(prostate, {
    levels(ekg)[levels(ekg) %in%
                c('old MI','recent MI')] <- 'MI'
    ekg.norm <- 1*(ekg %in% c('normal','benign'))
    levels(ekg) <- abbreviate(levels(ekg))
    pfn <- as.numeric(pf)
    levels(pf)  <- levels(pf)[c(1,2,3,3)]
    cvd <- status %in% c("dead - heart or vascular",
                         "dead - cerebrovascular")
    rxn = as.numeric(rx) })
# Use transcan to compute optimal pre-transformations
ptrans <-   # See Figure (*\ref{fig:prostate-transcan}*)
  transcan(~ sz + sg + ap + sbp + dbp + 
           age + wt + hg + ekg + pf + bm + hx + dtime + rx,
           imputed=TRUE, transformed=TRUE,
           data=prostate, pl=FALSE, pr=FALSE)
# Use transcan single imputations
imp <- impute(ptrans, data=prostate, list.out=TRUE)
NAvars <- all.vars(~ sz + sg + age + wt + ekg)
for(x in NAvars) prostate[[x]] <- imp[[x]]
subset <- prostate$status %in% c("dead - heart or vascular",
    "dead - cerebrovascular","dead - prostatic ca")
trans <- ptrans$transformed[subset,]
psub  <- prostate[subset,]


## -----------------------------------------------------------------------------
# Function to compute the first k PCs
ipc <- function(x, k=1, ...)
  princomp(x, ..., cor=TRUE)$scores[,1:k]
# Compute the first 8 PCs on raw variables then on
# transformed ones
pc8 <- ipc(~ sz + sg + log(ap) + sbp + dbp + age +
           wt + hg + ekg.norm + pfn + bm + hx + rxn + dtime,
           data=psub, k=8)
f8   <- lrm(cvd ~ pc8, data=psub)
pc8t <- ipc(trans, k=8)
f8t  <- lrm(cvd ~ pc8t, data=psub)
# Fit binary logistic model on original variables
f <- lrm(cvd ~ sz + sg + log(ap) + sbp + dbp + age +
         wt + hg + ekg + pf + bm + hx + rx + dtime, data=psub)
# Expand continuous variables using splines
g <- lrm(cvd ~ rcs(sz,4) + rcs(sg,4) + rcs(log(ap),4) +
         rcs(sbp,4) + rcs(dbp,4) + rcs(age,4) + rcs(wt,4) +
         rcs(hg,4) + ekg + pf + bm + hx + rx + rcs(dtime,4),
         data=psub)
# Fit binary logistic model on individual transformed var.
h <- lrm(cvd ~ trans, data=psub)


## -----------------------------------------------------------------------------
c(f8=AIC(f8), f8t=AIC(f8t), f=AIC(f), g=AIC(g), h=AIC(h))


## ----full,h=2.25,w=2.5,ps=8,results='asis',cap='Ranking of apparent importance of predictors of cause of death'----
f
an <- anova(f)
an
plot(an)   # Figure (*\ref{fig:lrcase1-full}*)
s <- f$stats
gamma.hat <- (s['Model L.R.'] - s['d.f.'])/s['Model L.R.']

## ----fullpeffects,h=6.5,w=6,cap='Partial effects (log odds scale) in full model for cause of death, along with vertical line segments showing the raw data distribution of predictors',scap='Partial effects in cause of death model'----
dd <- datadist(psub); options(datadist='dd')
ggplot(Predict(f), sepdiscrete='vertical', vnames='names',
       rdata=psub,
       histSpike.opts=list(frac=function(f) .1*f/max(f) )) # Figure (*\ref{fig:lrcase1-fullpeffects}*)

## ----fullor,h=4,w=5,top=2,cap='Interquartile-range odds ratios for continuous predictors and simple odds ratios for categorical predictors.  Numbers at left are upper quartile : lower quartile or current group : reference group.  The bars represent $0.9, 0.95, 0.99$ \\index{confidence intervals}confidence limits.  The intervals are drawn on the log odds ratio scale and labeled on the odds ratio scale.  Ranges are on the original scale.',scap='Interquartile-range odds ratios and confidence limits'----
plot(summary(f), log=TRUE)   # Figure (*\ref{fig:lrcase1-fullor}*)


## -----------------------------------------------------------------------------
fastbw(f)

## ----nom,results='asis',w=5,h=3.25,ps=8,cap='Nomogram calculating $X\\hat{\\beta}$ and $\\hat{P}$ for \\co{cvd} as the cause of death, using the step-down model.  For each predictor, read the points assigned on the 0--100 scale and add these points.  Read the result on the \\texttt{Total Points} scale and then read the corresponding predictions below it.',scap='Nomogram for obtaining $X\\hat{\\beta}$ and $\\hat{P}$ from step-down model'----
fred <- lrm(cvd ~ sz + log(ap) + age + hx, data=psub)
latex(fred, file='')
nom <- nomogram(fred, ap=c(.1, .5, 1, 5, 10, 50),
                fun=plogis, funlabel="Probability", 
                fun.at=c(.01,.05,.1,.25,.5,.75,.9,.95,.99))
plot(nom, xfrac=.45)   # Figure (*\ref{fig:lrcase1-nom}*)


## ----results='hide'-----------------------------------------------------------
f <- update(f, x=TRUE, y=TRUE)
v <- validate(f, B=200, bw=TRUE)

## ----results='asis'-----------------------------------------------------------
latex(v, B=20, digits=3)


## ----cal,results='hide',h=3,w=4.75,ps=9,bot=1,cap='Bootstrap overfitting--corrected calibration curve estimate for the backwards step-down cause of death logistic model, along with a rug plot showing the distribution of predicted risks.  The smooth nonparametric calibration estimator (\\co{loess})\\index{nonparametric!regression} is used.',scap='Bootstrap nonparametric calibration curve for reduced cause of death model'----
cal <- calibrate(f, B=200, bw=TRUE)
plot(cal)   # Figure (*\ref{fig:lrcase1-cal}*)


## ----results='asis'-----------------------------------------------------------
vfull <- validate(f, B=200)
latex(vfull, digits=3)


## -----------------------------------------------------------------------------
v5 <- validate(f, bw=TRUE, sls=0.5, type='individual', B=200)

## ----results='asis'-----------------------------------------------------------
latex(v5, digits=3, B=0)


## ----approxr2,h=3,w=3.5,bty='l',cap='Fraction of explainable variation (full model LR $\\chi^2$) in \\co{cvd} that was explained by approximate models, along with approximation accuracy ($x$--axis)',scap='Model approximation vs.\\ LR $\\chi^2$ preserved'----
lp <- predict(f)   # Compute linear predictor from full model
# Insert sigma=1 as otherwise sigma=0 will cause problems
a <- ols(lp ~ sz + sg + log(ap) + sbp + dbp + age + wt +
         hg + ekg + pf + bm + hx + rx + dtime, sigma=1,
         data=psub)
# Specify silly stopping criterion to remove all variables
s <- fastbw(a, aics=10000)
betas <- s$Coefficients   # matrix, rows=iterations
X     <- cbind(1, f$x)    # design matrix
# Compute the series of approximations to lp
ap <- X %*% t(betas)
# For each approx. compute approximation R^2 and ratio of
# likelihood ratio chi-square for approximate model to that
# of original model
m <- ncol(ap) - 1   # all but intercept-only model
r2 <- frac <- numeric(m)
fullchisq <- f$stats['Model L.R.']
for(i in 1:m) {
  lpa <- ap[,i]
  r2[i] <- cor(lpa, lp)^2
  fapprox <- lrm(cvd ~ lpa, data=psub)
  frac[i] <- fapprox$stats['Model L.R.'] / fullchisq
  }   # Figure (*\ref{fig:lrcase1-approxr2}*):
plot(r2, frac, type='b',
     xlab=expression(paste('Approximation ', R^2)),
     ylab=expression(paste('Fraction of ',
         chi^2, ' Preserved')))
abline(h=.95, col=gray(.83)); abline(v=.95, col=gray(.83))
abline(a=0, b=1, col=gray(.83))


## ----nomapprox,results='asis',h=5,w=5,ps=8,cap='Nomogram for predicting the probability of \\co{cvd} based on the approximate model',scap='Approximate nomogram for predicting cause of death'----
fapprox <- ols(lp ~ sz + sg + log(ap) + age + ekg + pf + hx +
               rx, data=psub)
fapprox$stats['R2']   # as a check
latex(fapprox, file='')
nom <- nomogram(fapprox, ap=c(.1, .5, 1, 5, 10, 20, 30, 40),
                fun=plogis, funlabel="Probability", 
                lp.at=(-5):4,
           fun.lp.at=qlogis(c(.01,.05,.25,.5,.75,.95,.99)))
plot(nom, xfrac=.45)   # Figure (*\ref{fig:lrcase1-nomapprox}*)

