## ----echo=FALSE---------------------------------------------------------------
require(Hmisc)
knitrSet('longit')
knitr::read_chunk('~/doc/rms/Longitudinal/shared.R')


## ----spaghetti,h=5,w=7,ps=7,cap='Time profiles for individual subjects, stratified by study site and dose'----
require(rms)
options(prType='latex')    # for model print, summary, anova
getHdata(cdystonia)  
attach(cdystonia)

# Construct unique subject ID
uid <- with(cdystonia, factor(paste(site, id)))

# Tabulate patterns of subjects' time points
table(tapply(week, uid,
             function(w) paste(sort(unique(w)), collapse=' ')))

# Plot raw data, superposing subjects
xl <- xlab('Week'); yl <- ylab('TWSTRS-total score')
ggplot(cdystonia, aes(x=week, y=twstrs, color=factor(id))) +
       geom_line() + xl + yl + facet_grid(treat ~ site) +
       guides(color=FALSE) # Fig. (*\ref{fig:longit-spaghetti}*)


## ----quartiles,cap='Quartiles of \\co{TWSTRS} stratified by dose',w=5,h=4-----
# Show quartiles
require(data.table)
cdystonia <- data.table(cdystonia)
cdys <- cdystonia[, j=as.list(quantile(twstrs, (1 : 3)/4)),
                  by = list(treat, week)]
cdys <- upData(cdys, rename=c('25%'='Q1', '50%'='Q2', '75%'='Q3'), print=FALSE)
ggplot(cdys, aes(x=week, y=Q2)) + xl + yl + ylim(0, 70) +
  geom_line() + facet_wrap(~ treat, nrow=2) +
  geom_ribbon(aes(ymin=Q1, ymax=Q3), alpha=0.2)   # Fig. (*\ref{fig:longit-quartiles}*)


## ----e------------------------------------------------------------------------
baseline <- subset(data.frame(cdystonia,uid), week == 0,
                   -week)
baseline <- upData(baseline, rename=c(twstrs='twstrs0'),
                   print=FALSE)
followup <- subset(data.frame(cdystonia,uid), week > 0,
                   c(uid,week,twstrs))
rm(uid)
both     <- merge(baseline, followup, by='uid')

dd       <- datadist(both)
options(datadist='dd')


## ----k------------------------------------------------------------------------
require(nlme)
cp <- list(corCAR1,corExp,corCompSymm,corLin,corGaus,corSpher)
z  <- vector('list',length(cp))
for(k in 1:length(cp)) {
  z[[k]] <- gls(twstrs ~ treat * rcs(week, 3) +
                rcs(twstrs0, 3) + rcs(age, 4) * sex, data=both,
                correlation=cp[[k]](form = ~week | uid))
}
anova(z[[1]],z[[2]],z[[3]],z[[4]],z[[5]],z[[6]])


## ----l,results='asis'---------------------------------------------------------
a <- Gls(twstrs ~ treat * rcs(week, 3) + rcs(twstrs0, 3) +
         rcs(age, 4) * sex, data=both,
         correlation=corCAR1(form=~week | uid))
a


## ----variogram,cap='Variogram, with assumed correlation pattern superimposed'----
v <- Variogram(a, form=~ week | uid)
plot(v)  # Figure (*\ref{fig:longit-variogram}*)


## ----resid,h=6,w=7.5,cap='Three residual plots to check for absence of trends in central tendency and in variability.  Upper right panel shows the baseline score on the $x$-axis.  Bottom left panel shows the mean $\\pm 2\\times$SD.  Bottom right  panel is the QQ plot for checking normality of residuals from the GLS fit.',scap='Residual plots for GLS model'----
both$resid <- r <- resid(a); both$fitted <- fitted(a)
yl <- ylab('Residuals')
p1 <- ggplot(both, aes(x=fitted, y=resid)) + geom_point() +
      facet_grid(~ treat) + yl
p2 <- ggplot(both, aes(x=twstrs0, y=resid)) + geom_point()+yl
p3 <- ggplot(both, aes(x=week, y=resid)) + yl + ylim(-20,20) +
      stat_summary(fun.data="mean_sdl", geom='smooth')
p4 <- ggplot(both, aes(sample=resid)) + stat_qq() +
      geom_abline(intercept=mean(r), slope=sd(r)) + yl
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)   # Figure (*\ref{fig:longit-resid}*)


## ----anova,results='asis',w=3.25,h=2.25,cap='Results of \\fu{anova} from generalized least squares fit with continuous time AR1 correlation structure.  As expected, the baseline version of $Y$ dominates.',scap='ANOVA for GLS model'----
plot(anova(a))     # Figure (*\ref{fig:longit-anova}*)


## ----pleffects,h=5,w=7,cap='Estimated effects of time, baseline TWSTRS, age, and sex'----
ylm <- ylim(25, 60)
p1 <- ggplot(Predict(a, week, treat, conf.int=FALSE),
             adj.subtitle=FALSE, legend.position='top') + ylm
p2 <- ggplot(Predict(a, twstrs0), adj.subtitle=FALSE) + ylm
p3 <- ggplot(Predict(a, age, sex), adj.subtitle=FALSE,
             legend.position='top') + ylm
gridExtra::grid.arrange(p1, p2, p3, ncol=2)   # Figure (*\ref{fig:longit-pleffects}*)


## ----o,results='asis'---------------------------------------------------------
summary(a)  # Shows for week 8


## ----p------------------------------------------------------------------------
# To get results for week 8 for a different reference group
# for treatment, use e.g. summary(a, week=4, treat='Placebo')

# Compare low dose with placebo, separately at each time
k1 <- contrast(a, list(week=c(2,4,8,12,16), treat='5000U'),
                  list(week=c(2,4,8,12,16), treat='Placebo'))
options(width=80)
print(k1, digits=3)


## ----q------------------------------------------------------------------------
# Compare high dose with placebo
k2 <- contrast(a, list(week=c(2,4,8,12,16), treat='10000U'),
                  list(week=c(2,4,8,12,16), treat='Placebo'))
print(k2, digits=3)

## ----contrasts,h=3,w=5.5,cap='Contrasts and 0.95 confidence limits from GLS fit'----
k1 <- as.data.frame(k1[c('week', 'Contrast', 'Lower', 'Upper')])
p1 <- ggplot(k1, aes(x=week, y=Contrast)) + geom_point() +
      geom_line() + ylab('Low Dose - Placebo') +
      geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0)
k2 <- as.data.frame(k2[c('week', 'Contrast', 'Lower', 'Upper')])
p2 <- ggplot(k2, aes(x=week, y=Contrast)) + geom_point() +
      geom_line() + ylab('High Dose - Placebo') +
      geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0)
gridExtra::grid.arrange(p1, p2, ncol=2)   # Figure (*\ref{fig:longit-contrasts}*)


## ----nomogram,h=5,w=6,cap='Nomogram from GLS fit.  Second axis is the baseline score.',scap='Nomomgram from GLS fit.'----
n <- nomogram(a, age=c(seq(20, 80, by=10), 85))
plot(n, cex.axis=.55, cex.var=.8, lmgp=.25)  # Figure (*\ref{fig:longit-nomogram}*)

