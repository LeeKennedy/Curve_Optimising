#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(writexl)
library(here)


#### Functions -----------------------------


#### Data Input -----------------------------
here::here()

data_in <- read_excel("data/Ethanol_Run2.xlsx", 
                      sheet = "Sheet2")

#### Data Cleaning -----------------------------

n <- nrow(data_in)

df <- data.frame(Run_set = as.character(),
                 Points = as.numeric(),
                 Slope = as.numeric(), 
                 Intercept = as.numeric(), 
                 R2 = as.numeric(),
                 SE = as.numeric(),
                 Ratio = as.numeric(),
                 stringsAsFactors=FALSE) 
m=n-3

for(i in 1:m) {
   
        data_in2 <- data_in[c(i:n),]
        Points = as.numeric(nrow(data_in2))
        
        fit <- lm(data_in2$Ratio~data_in2$Standard)
        fit_summary <- summary(fit)
        slope_fit <- fit_summary$coefficients[2]
        slope_intercept <- fit_summary$coefficients[1]
        R2_fit <- fit_summary$r.squared
        SE_fit <- fit_summary$sigma
        Run_Set = paste(i,"to",n)
        l = as.numeric(data_in2[1,1])
        u = as.numeric(data_in2[Points,1])
        validx = ((slope_fit*u+slope_intercept)/(slope_fit*l+slope_intercept))/(u/l)

df <- rbind(df, data.frame(Run_set = Run_Set, Points, Slope = slope_fit, Intercept = slope_intercept, R2 = R2_fit, SE = SE_fit, Ratio = validx))
}
df

m=n-3

for(i in 1:m) {
        j=i+2
        data_in2 <- data_in[c(1:j),]
        Points = as.numeric(nrow(data_in2))
        
        fit <- lm(data_in2$Ratio~data_in2$Standard)
        fit_summary <- summary(fit)
        slope_fit <- fit_summary$coefficients[2]
        slope_intercept <- fit_summary$coefficients[1]
        R2_fit <- fit_summary$r.squared
        SE_fit <- fit_summary$sigma
        Run_Set = paste("1 to",j)
        l = as.numeric(data_in2[1,1])
        u = as.numeric(data_in2[j,1])
        validx = ((slope_fit*u+slope_intercept)/(slope_fit*l+slope_intercept))/(u/l)
        
        
        df <- rbind(df, data.frame(Run_set = Run_Set, Points, Slope = slope_fit, Intercept = slope_intercept, R2 = R2_fit, SE = SE_fit, Ratio = validx))
}
df

# write_xlsx(df, "Ethanol2_table.xlsx")
