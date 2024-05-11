knitr::opts_chunk$set(echo = TRUE)

## # Author:  Ben Lehmann
## # Date:    2024-05-07
## # Purpose: Portfolio 2: Working with Baseball Data and Multivariable regression
## #-------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(kableExtra)
library(dplyr)
library(knitr)
library(rmarkdown)

baseball <- read_csv('0519_baseball_reference.csv',show_col_types = FALSE)  #show_col_types to remove message
baseball$year <- as.character(baseball$year)
baseball <- baseball |> select(-playerid) |> filter(year>=2010)  #Remove Player ID, nothing useful about the variable

#Add the Mean,Median,SD and Upper and Lower intervals
baseball_summary <- baseball |> group_by(year) |> summarise(n=n(), mean_sal = mean(sal), med_sal=median(sal) ,sd_sal=sd(sal),war_mean=mean(war), war_median=median(war),war_sd=sd(war), exp_mean=mean(exp), exp_median=median(exp),exp_sd=sd(exp))|>mutate(
    war_lower = war_mean - qt(.975, df = n-1) * war_sd / sqrt(n),
    war_upper = war_mean + qt(.975, df = n-1) * war_sd / sqrt(n),
    exp_lower = exp_mean - qt(.975, df = n-1) * exp_sd / sqrt(n),
    exp_upper = exp_mean - qt(.975, df = n-1) * exp_sd / sqrt(n)) #Add the Intervals to the data

knitr::kable(baseball_summary) |> kable_styling()  #Use a Table

baseball_overall <- baseball |> select(sal,war,exp) |> summarise(n=n(), war_mean=mean(war), war_median=median(war),war_sd=sd(war), exp_mean=mean(exp), exp_median=median(exp),exp_sd=sd(exp))|>mutate(
    war_lower = war_mean - qt(.975, df = n-1) * war_sd / sqrt(n),
    war_upper = war_mean + qt(.975, df = n-1) * war_sd / sqrt(n),
    exp_lower = exp_mean - qt(.975, df = n-1) * exp_sd / sqrt(n),
    exp_upper = exp_mean - qt(.975, df = n-1) * exp_sd / sqrt(n))

knitr::kable(baseball_overall) |> kable_styling()  #Use a Table

ggplot(baseball, aes(x=war, y = sal, color=exp)) +   #Went on color as experience
  geom_point() + geom_jitter(
    data = baseball,
    width = 0.5, height = 0.8   #Add Jitter to spread out
  )  + theme_minimal() + labs(x = "WAR", y = "Salary",title = "Salary based on Experience and WAR")

model <- lm(sal~war+exp,data=baseball)  #Use y=sal, x1=war, x2=exp

  ggplot(baseball,mapping = aes(x = war, y = sal,color=exp)) +
  geom_point() +
  geom_smooth(method = "lm",se=TRUE) +
  xlab("Wins Above Replacement (WAR)") +
  ylab("Salary")  +
  ggtitle("MLB Player Salary vs. WAR Model") + ylim(0e+00,4e+07)


ggplot(baseball, aes(x = war, y = sal,color=exp)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "blue") +  # Regression line
  geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color = "green") +  # Line for hits coefficient
  geom_abline(intercept = coef(model)[1], slope = coef(model)[3], color = "red") +  # Line for homeruns coefficient
  labs(x = "WAR", y = "Salary",title = "Intercept and Slope Graph") +  # Axes labels
  theme_minimal()  # Minimal theme

cint <- bind_cols(          
  baseball,
  predict(model,
          newdata = baseball,
          interval = "confidence") |>
    as.data.frame()   #Create a New Dataframe that add the predicted values, the confidence values to the original table
)

predictions <- predict(model, interval = "predict",level=0.95)
all_data <- cbind(baseball, predictions)

ggplot(all_data, aes(x = war, y = sal, color=exp)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = 'lm') + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = fit), col = "green", linetype = "dashed") +
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") +
  labs(x = "WAR", y = "Salary",
       title = "Predictive Values Interval Graph")            #upr pred interval

conf_int_95 <- predict(model, interval = "confidence",level = 0.95)  #Create a Confidence Interval

plot_data <- cbind(baseball,conf_int_95)  #Add together with baseball dataset

# Plot with 95% confidence intervals
ggplot(plot_data, aes(x = war, y = sal,color=exp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_line(aes(y = lwr), color = "red") + 
  geom_line(aes(y = upr), color = "red") +
  labs(title = "MLR with 95% Confidence Intervals",
       x = "Experience",
       y = "Salary")

new <- expand.grid(
  war  = range(baseball$war),
  exp = unique(baseball$exp)
)

# Perform predictions
p <- new |>
  mutate(
    sal = predict(model,newdata = new)  #Add a new column to the dataset
  )


ggplot(baseball, aes(x = war, y = sal, color=exp)) + #define x and y axis variables
  geom_point() + geom_smooth(method="lm",se=TRUE)+ geom_line(
    data = p,aes(group = exp))+labs(x = "WAR", y = "Salary",
       title = "Predictive CI Graph (Group=Exp)")

ggplot(baseball, aes(x = war, y = sal, color=exp)) + #define x and y axis variables
  geom_point() + geom_smooth(method="lm",se=TRUE)+ geom_line(
    data = p,aes(group = war))+labs(x = "WAR", y = "Salary",
       title = "Predictive Graph (Group=war)")

ggplot(
  mapping = aes(sample = model$residuals)) +
  geom_qq() +
  geom_qq_line() + xlab("WAR") + ylab("Salary in $")

ggplot(
  mapping = aes(x = model$fitted.values, 
                y = model$residuals)) +
  geom_point()

ggplot(
  mapping = aes(x = 1:length(model$residuals),
                y = model$residuals)) +
  geom_point()
