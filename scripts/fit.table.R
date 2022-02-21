#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(xlsx)
library(here)


#### Functions -----------------------------


#### Data Input -----------------------------
here::here()

data_in <- read_excel("data/Ethanol.xlsx")

#### Data Cleaning -----------------------------

n <- nrow(data_in)

df <- data.frame(Run_set = as.character(),
                 Slope = as.numeric(), 
                 Intercept = as.numeric(), 
                 R2 = as.numeric(),
                 SE = as.numeric(),
                 stringsAsFactors=FALSE) 
m=n-3

for(i in 1:m) {
   
        data_in2 <- data_in[c(i:n),]
        
        fit <- lm(data_in2$Ratio~data_in2$Standard)
        fit_summary <- summary(fit)
        slope_fit <- fit_summary$coefficients[2]
        slope_intercept <- fit_summary$coefficients[1]
        R2_fit <- fit_summary$r.squared
        SE_fit <- fit_summary$sigma
        Run_Set = paste(i,"to",n)

df <- rbind(df, data.frame(Run_set = Run_Set, Slope = slope_fit, Intercept = slope_intercept, R2 = R2_fit, SE = SE_fit))
}
df

m=n-3

for(i in 1:m) {
        j=i+2
        data_in2 <- data_in[c(1:j),]
        
        fit <- lm(data_in2$Ratio~data_in2$Standard)
        fit_summary <- summary(fit)
        slope_fit <- fit_summary$coefficients[2]
        slope_intercept <- fit_summary$coefficients[1]
        R2_fit <- fit_summary$r.squared
        SE_fit <- fit_summary$sigma
        Run_Set = paste("1 to",j)
        
        df <- rbind(df, data.frame(Run_set = Run_Set, Slope = slope_fit, Intercept = slope_intercept, R2 = R2_fit, SE = SE_fit))
}
df

write.xlsx(df, "curve_table.xlsx")
