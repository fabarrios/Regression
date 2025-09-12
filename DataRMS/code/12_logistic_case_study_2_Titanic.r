## @knitr desc
require(rms)
options(prType='latex')   # for print, summary, anova
getHdata(titanic3)        # get dataset from web site
# List of names of variables to analyze
v <- c('pclass','survived','age','sex','sibsp','parch')
t3 <- titanic3[, v]
units(t3$age) <- 'years'
latex(describe(t3), file='')


## @knitr summary
dd <- datadist(t3)
# describe distributions of variables to rms
options(datadist='dd')
s <- summary(survived ~ age + sex + pclass +
             cut2(sibsp,0:3) + cut2(parch,0:3), data=t3)
plot(s, main='', subtitles=FALSE)   # Figure (*\ref{fig:titanic-summary}*)


## @knitr dot
tn <- transform(t3,
  agec = ifelse(age < 21, 'child', 'adult'),
  sibsp= ifelse(sibsp == 0, 'no sib/sp', 'sib/sp'),
  parch= ifelse(parch == 0, 'no par/child', 'par/child'))

g <- function(y) if(length(y) < 25) NA else mean(y)
s <- with(tn, summarize(survived, 
           llist(agec, sex, pclass, sibsp, parch), g))
# llist, summarize in Hmisc package
# Figure (*\ref{fig:titanic-dot}*):
ggplot(subset(s, agec != 'NA'),
  aes(x=survived, y=pclass, shape=sex)) +
  geom_point() + facet_grid(agec ~ sibsp * parch) +
  xlab('Proportion Surviving') + ylab('Passenger Class') +
  scale_x_continuous(breaks=c(0, .5, 1))


## @knitr plsmoa
# Figure (*\ref{fig:titanic-plsmoa}*)
b  <- scale_size_discrete(range=c(.1, .85))
yl <- ylab(NULL)
p1 <- ggplot(t3, aes(x=age, y=survived)) +
      histSpikeg(survived ~ age, lowess=TRUE, data=t3) +
      ylim(0,1) + yl
p2 <- ggplot(t3, aes(x=age, y=survived, color=sex)) +
      histSpikeg(survived ~ age + sex, lowess=TRUE,
                 data=t3) + ylim(0,1) + yl
p3 <- ggplot(t3, aes(x=age, y=survived, size=pclass)) +
      histSpikeg(survived ~ age + pclass, lowess=TRUE,
                 data=t3) + b + ylim(0,1) + yl
p4 <- ggplot(t3, aes(x=age, y=survived, color=sex,
       size=pclass)) +
      histSpikeg(survived ~ age + sex + pclass,
                 lowess=TRUE, data=t3) +
      b + ylim(0,1) + yl
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)   # combine 4


## @knitr plsmounused
frq <- ggplot(t3, aes(x=age, color=sex, size=pclass)) +
       geom_line(stat='density', adjust=.7) + b
p1 <- ggplot(t3, aes(x=age, y=survived)) + stat_plsmo() +
      ylim(0,1) + yl
p1 <- gridExtra::arrangeGrob(frq, p1, ncol=1)  # combine 2


## @knitr plsmob
# Figure (*\ref{fig:titanic-plsmob}*)
top <- theme(legend.position='top')
p1 <- ggplot(t3, aes(x=age, y=survived, color=cut2(sibsp,
       0:2))) + stat_plsmo() + b + ylim(0,1) + yl + top +
      scale_color_discrete(name='siblings/spouses')
p2 <- ggplot(t3, aes(x=age, y=survived, color=cut2(parch,
       0:2))) + stat_plsmo() + b + ylim(0,1) + yl + top +
      scale_color_discrete(name='parents/children')
gridExtra::grid.arrange(p1, p2, ncol=2)


## @knitr anova3
f1 <- lrm(survived ~ sex*pclass*rcs(age,5) +
          rcs(age,5)*(sibsp + parch), data=t3)   # Table (*\ref{titanic-anova3}*)
print(anova(f1), table.env=TRUE, label='titanic-anova3', size='small')


## @knitr ia2
f <- lrm(survived ~ (sex + pclass + rcs(age,5))^2 + 
         rcs(age,5)*sibsp, data=t3)
print(f)


## @knitr anova2
print(anova(f), table.env=TRUE, label='titanic-anova2',size='small') #(*\ref{titanic-anova2}*)


## @knitr plot1
p <- Predict(f, age, sex, pclass, sibsp=0, fun=plogis)
ggplot(p)       # Fig. (*\ref{fig:titanic-plot1}*)


## @knitr plot2
ggplot(Predict(f, sibsp, age=c(10,15,20,50), conf.int=FALSE))
## Figure (*\ref{fig:titanic-plot2}*)


## @knitr val
f <- update(f, x=TRUE, y=TRUE)
# x=TRUE, y=TRUE adds raw data to fit object so can bootstrap
set.seed(131)                  # so can replicate re-samples
latex(validate(f, B=200), digits=2, size='Ssize')


## @knitr calibrate
cal <- calibrate(f, B=200)      # Figure (*\ref{fig:titanic-calibrate}*)
plot(cal, subtitles=FALSE)


## @knitr napatterns
na.patterns <- naclus(titanic3)
require(rpart)      # Recursive partitioning package
who.na <- rpart(is.na(age) ~ sex + pclass + survived +
                sibsp + parch, data=titanic3, minbucket=15)
naplot(na.patterns, 'na per var')
plot(who.na, margin=.1); text(who.na) # Figure (*\ref{fig:titanic-napatterns}*)
plot(na.patterns)


## @knitr summary-na
plot(summary(is.na(age) ~ sex + pclass + survived +
             sibsp + parch, data=t3))  # Figure (*\ref{fig:titanic-summary-na}*)


## @knitr nalrm
m <- lrm(is.na(age) ~ sex * pclass + survived + sibsp + parch,
         data=t3)
print(m, needspace='3.5in')


## @knitr anova.na
print(anova(m), table.env=TRUE, label='titanic-anova.na') # Table (*\ref{titanic-anova.na}*)


## @knitr transcan
xtrans <- transcan(~ I(age) + sex + pclass + sibsp + parch, 
                   imputed=TRUE, pl=FALSE, pr=FALSE, data=t3)
summary(xtrans)
# Look at mean imputed values by sex,pclass and observed means
# age.i is age, filled in with conditional mean estimates
age.i <- with(t3, impute(xtrans, age, data=t3))
i <- is.imputed(age.i)
with(t3, tapply(age.i[i], list(sex[i],pclass[i]), mean))
with(t3, tapply(age, list(sex,pclass), mean, na.rm=TRUE))


## @knitr fit.si
dd   <- datadist(dd, age.i)
f.si <- lrm(survived ~ (sex + pclass + rcs(age.i,5))^2 + 
            rcs(age.i,5)*sibsp, data=t3)
print(f.si, coefs=FALSE)


## @knitr nasingle
p1 <- Predict(f,    age,   pclass, sex, sibsp=0, fun=plogis)
p2 <- Predict(f.si, age.i, pclass, sex, sibsp=0, fun=plogis)
p  <- rbind('Casewise Deletion'=p1, 'Single Imputation'=p2,
            rename=c(age.i='age'))   # creates .set. variable
ggplot(p, groups='sex', ylab='Probability of Surviving')
# Figure (*\ref{fig:titanic-nasingle}*)


## @knitr anova.si
print(anova(f.si), table.env=TRUE, label='titanic-anova.si')   # Table (*\ref{titanic-anova.si}*)


## @knitr aregi
set.seed(17)         # so can reproduce random aspects
mi <- aregImpute(~ age + sex + pclass + 
                 sibsp + parch + survived,
                 data=t3, n.impute=20, nk=4, pr=FALSE)
mi
# Print the first 10 imputations for the first 10 passengers
#  having missing age
mi$imputed$age[1:10, 1:10]


## @knitr ageDist
plot(mi)
Ecdf(t3$age, add=TRUE, col='gray', lwd=2,
     subtitles=FALSE)#Fig. (*\ref{fig:titanic-ageDist}*)


## @knitr anova.mi
f.mi <- fit.mult.impute(
  survived ~ (sex + pclass + rcs(age,5))^2 +
  rcs(age,5)*sibsp,
  lrm, mi, data=t3, pr=FALSE)
print(anova(f.mi), table.env=TRUE, label='titanic-anova.mi',
      size='small')   # Table (*\ref{titanic-anova.mi}*)


## @knitr namult
p1 <- Predict(f.si,  age.i, pclass, sex, sibsp=0, fun=plogis)
p2 <- Predict(f.mi,  age,   pclass, sex, sibsp=0, fun=plogis)
p  <- rbind('Single Imputation'=p1, 'Multiple Imputation'=p2,
            rename=c(age.i='age'))
ggplot(p, groups='sex', ylab='Probability of Surviving')
# Figure (*\ref{fig:titanic-namult}*)


## @knitr namult2
p1 <- Predict(f,     age, pclass, sex, sibsp=0, fun=plogis)
p2 <- Predict(f.mi,  age, pclass, sex, sibsp=0, fun=plogis)
p  <- rbind('Casewise Deletion'=p1, 'Multiple Imputation'=p2)
ggplot(p, groups='sex', ylab='Probability of Surviving')
# Figure (*\ref{fig:titanic-namult2}*)


## @knitr ors
# Get predicted values for certain types of passengers
s <- summary(f.mi, age=c(1,30), sibsp=0:1)  
# override default ranges for 3 variables
plot(s, log=TRUE, main='')             # Figure (*\ref{fig:titanic-ors}*)


## @knitr phat
phat <- predict(f.mi,
                combos <-
         expand.grid(age=c(2,21,50),sex=levels(t3$sex),
                     pclass=levels(t3$pclass),
                     sibsp=0), type='fitted')
# Can also use Predict(f.mi, age=c(2,21,50), sex, pclass,
#                      sibsp=0, fun=plogis)$yhat
options(digits=1)
data.frame(combos, phat)
options(digits=5)


## @knitr pred.logit
pred.logit <- Function(f.mi)
# Note: if don't define sibsp to pred.logit, defaults to 0
# normally just type the function name to see its body
latex(pred.logit, file='', type='Sinput', size='small',
      width.cutoff=49)


## @knitr plogis
# Run the newly created function
plogis(pred.logit(age=c(2,21,50), sex='male', pclass='3rd'))


