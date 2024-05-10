# Use echo = FALSE for Portfolio assignments
knitr::opts_chunk$set(echo = FALSE)

## # Author:  Ben Lehmann
## # Date:    2024-05-07
## # Purpose: Example R Markdown file with Portfolio-style analysis
## #-------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(kableExtra)
library(dplyr)
library(knitr)
library(rmarkdown)

#Start of Portfolio-1
gas_data <- read.csv('gas_mileage_data.csv')  #Load the Data
gas_data <- gas_data[!is.na(gas_data$mpg) & !is.na(gas_data$ethanol), ]  #Do the same thing, make sure all the NA for ethanol and mpg is gone
gas_data$ethanol <- (if_else(gas_data$ethanol > 0, "Yes", "No"))  #Get the Yes or No Ethanol


ethanol_types <- gas_data |> group_by(ethanol) |> summarise(n=n(), mpg_mean = mean(mpg), mpg_median=median(mpg), mpg_sd=sd(mpg),mpg_var = var(mpg),mpg_min = min(mpg),max_mpg = max(mpg)) |> mutate(
    lower = mpg_mean - qt(.975, df = n-1) * mpg_sd / sqrt(n),
    upper = mpg_mean + qt(.975, df = n-1) * mpg_sd / sqrt(n),
  )  
knitr::kable(ethanol_types) |> kable_styling()   #Get the Summary Statistics, Mean, Median, Mode, 95% intveral, Max, Min

ggplot(gas_data, aes(x = ethanol, y = mpg)) +
  geom_boxplot() +
  labs(x = "Ethanol more than 10%", y = "Miles per Gallon (mpg)",
       title = "Boxplot of Gas Mileage with and without Ethanol") +
  theme_minimal()

ggplot(
  mapping = aes(
      x = ethanol,
      y = mpg
    )) +
  geom_jitter(
    data = gas_data,
    width = 0.1
  ) +labs(x = "Ethanol more than 10%", y = "Miles per Gallon (mpg)",
       title = "Scatterplot of Gas Mileage with and without Ethanol")

model <- lm(mpg~ethanol,data=gas_data)  #Linear Regression Model

confidence <- confint(model,level = 0.95)  #Get the Confidence Interval
conf_int <- confidence["ethanolYes",]   #get the Inverval Values

knitr::kable(confidence) |> kable_styling()  #Use a Table for the Interval

cint <- bind_cols(   #We need to add the Predicted Values, upper and lower values
  gas_data,
  predict(model,
          newdata = gas_data,
          interval = "confidence") |>
    as.data.frame()
)

new <- gas_data |>
  select(ethanol) |>   #Select the Categorical Values
  unique()

cintval <- bind_cols(
  new,  #Combine the New data with the predicted values 
  predict(model,
          newdata = new,
          interval = "confidence") |>        #Create the Confidence Interval
    as.data.frame() |>
    rename(mpg = fit)   #Make it as fit
)
 ggplot(
  mapping = aes(
      x = ethanol,
      y = mpg
    )) +
  geom_jitter(
    data = gas_data,
    width = 0.1
  )+geom_pointrange(
    data = cintval,
    aes(ymin = lwr,ymax = upr),col = "red") + labs(y="Miles Per Gallon(MPG)", x="Ethanol", title = "Fit Model of Gas Mileage with and without Ethanol")

ggplot(
  bind_rows(
    cint |> mutate(Interval = "Confidence")),  #We can get the Confidence Values
  aes(
    x     = ethanol,
    y     = fit,
    ymin  = lwr,
    ymax  = upr,
    color = Interval
  )
) +
  geom_pointrange(
    position = position_dodge(width = .1)
  ) + labs(y="Miles Per Gallon(MPG)", x="Ethanol", title = "95% C.I of Gas Mileage with and without Ethanol")

ggplot(
  mapping = aes(sample = model$residuals)) +
  geom_qq() +
  geom_qq_line() + labs(y="Miles Per Gallon(MPG)", x="Ethanol", title = "qq Plot Gas Mileage with and without Ethanol")

plot(model)

