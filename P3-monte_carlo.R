knitr::opts_chunk$set(echo = TRUE)

## # Author:  Ben Lehmann
## # Date:    2024-05-07
## # Purpose: Portfolio 3: Working with Simulations
## #-------------------------------------------------------------------------------

library(ggplot2)

#Instead of making many functions, I can use an if loop in 1 function to get the type and value of the type
mse_value <- function(nval,pval,type){
  y1 <- rbinom(1e3, nval, pval)   
  if (type == "frequentist") {  #If the type is frequentist, we will use the phat for the frequentist method
    p_hat1 <- y1 / nval
  } else if (type == "bayesian") {  #Compute the phat for bayesian method
    p_hat1 <- (0.5 + y1) / (1 + nval)
  }
  mse <- mean((p_hat1 - pval)^2)  #We return the MSE of which ever type
  return(mse)
}

#Instead of creating multiple functions, We can create another function that can do the same thing with an if loop
calc_cover <- function(nvalue, pvalue, type) {
  alpha <- 0.05
  y2 <- rbinom(1e3, nvalue, pvalue)
  if (type == "frequentist") {  #If we call back frequentist approach, the function will return the frequentist interval
    p_hat <- y2 / nvalue  #get the phat
    ci_low <- p_hat - qnorm(1 - alpha/2) * sqrt(p_hat * (1 - p_hat) / nvalue)  #Calculate the normal interval
    ci_up <- p_hat + qnorm(1 - alpha/2) * sqrt(p_hat * (1 - p_hat) / nvalue)
  } else if (type == "bayesian") { #If we call back bayes approach, the function will return the frequentist interval
    p_hat <- (0.5 + y2) / (1 + nvalue)
    ci_low <- qbeta(alpha/2, 0.5 + y2, 0.5 + nvalue - y2)
    ci_up <- qbeta(1 - alpha/2, 0.5 + y2, 0.5 + nvalue - y2)  #From the PDF
  }
  coverage <- mean(ci_low <= pvalue & pvalue <= ci_up)
  return(coverage)
}

#Now we can create multiple n x p Matrices of 0's, then we can add the new distribution values to them
mse_frequent <- matrix(0, nrow = 6, ncol = 9)
mse_bayes <- matrix(0, nrow = 6, ncol = 9)
coverage_frequent <- matrix(0, nrow = 6, ncol = 9)
coverage_bayes <- matrix(0, nrow = 6, ncol = 9)

#Create a Simulation looping through the Matrices
nvlues <- c(5, 10, 15, 20, 25, 30)
pvlues <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
for (i in 1:length(nvlues)) {  #Iterate throught the matrices which is the size of the number of n values and p values
  for (j in 1:length(pvlues)) {
    n <- c(5, 10, 15, 20, 25, 30)[i]  #Index through the n values
    p <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)[j]  #Index through the p values
    mse_frequent[i, j] <- mse_value(n, p, "frequentist")  #Call back the function
    mse_bayes[i, j] <- mse_value(n, p, "bayesian")
    coverage_frequent[i, j] <- calc_cover(n, p, "frequentist")
    coverage_bayes[i, j] <- calc_cover(n, p, "bayesian")
  }
}

mse_data <- data.frame(n_vals = rep(c(5, 10, 15, 20, 25, 30), each = 9),
                       p_vals = rep(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), times = 6),
                       approachtype = rep(c("frequentist", "bayesian"), each = 54),
                       mse = c(mse_frequent, mse_bayes))  

ggplot(mse_data, aes(x = p_vals, y = mse, color = approachtype, linetype = approachtype)) +
  geom_line() +
  facet_wrap(~n_vals) +
  xlab("p") +
  ylab("MSE") + ggtitle("Comparison of MSE for Frequentist and Bayesian type")

coverage_df <- data.frame(n_vals = rep(c(5, 10, 15, 20, 25, 30), each = 9),
                            p_vals = rep(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), times = 6),
                            approachtype = rep(c("frequentist", "bayesian"), each = 54),
                            coveragevals = c(coverage_frequent, coverage_bayes))

 ggplot(coverage_df, aes(x = p_vals, y = coveragevals, color = approachtype, linetype = approachtype)) +
  geom_line() +
  facet_wrap(~n_vals) +
  xlab("p") +
  ylab("Coverage MSE") +
  ggtitle("Comparison of Coverage for Frequentist and Bayesian type")

