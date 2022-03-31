# Logistic function
# plot of the logistic function

f_logistic <- function(x) {
  return (1/(1 + exp(-x)))
  }
  
curve(f_logistic,
  from = -4, 
  to = 4,
  ylab = "P",
  xlab = "X",
  main = "Logistic Function")
  
