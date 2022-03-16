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

data_in <- read_excel("data/Ethanol_Run3.xlsx", 
                      sheet = "Sheet2")

#### Data Cleaning -----------------------------



data_in <- data_in %>% mutate(data_in, ID = row_number())

df <- data.frame(Run_set = as.character(),
                 Points = as.numeric(),
                 Slope = as.numeric(), 
                 Intercept = as.numeric(), 
                 R2 = as.numeric(),
                 SE = as.numeric(),
                 stringsAsFactors=FALSE) 


n <- nrow(data_in)
m=n-3

for(i in 1:m) {
   
        data_in2 <- data_in[c(i:n),]
        Points = as.numeric(nrow(data_in2))
        
        fit <- lm(data_in2$Ratio~data_in2$Standard-1)
        fit_summary <- summary(fit)
        slope_fit <- fit_summary$coefficients[1]
        slope_intercept <- 0
        R2_fit <- fit_summary$r.squared
        SE_fit <- fit_summary$sigma
        Run_Set = paste(i,"to",n)
      

df <- rbind(df, data.frame(Run_set = Run_Set, Points, Slope = round(slope_fit,4), Intercept = slope_intercept, R2 = round(R2_fit,4), SE = round(SE_fit,4)))
}
df

m=n-3

for(i in 1:m) {
        j=i+2
        data_in2 <- data_in[c(1:j),]
        Points = as.numeric(nrow(data_in2))
        
        fit <- lm(data_in2$Ratio~data_in2$Standard-1)
        fit_summary <- summary(fit)
        slope_fit <- fit_summary$coefficients[1]
        slope_intercept <- 0
        R2_fit <- fit_summary$r.squared
        SE_fit <- fit_summary$sigma
        Run_Set = paste("1 to",j)
        
        df <- rbind(df, data.frame(Run_set = Run_Set, Points, Slope = round(slope_fit,4), Intercept = slope_intercept, R2 = round(R2_fit,4), SE = round(SE_fit,4)))
}

df <- df %>% arrange(SE)
df <- df[,c(1,2,3,5,6)]
df$LOQ <- round(df$SE*10,2)
df



# write_xlsx(df, "output/Ethanol2_table_zero.xlsx")
