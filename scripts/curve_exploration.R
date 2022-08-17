#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(writexl)
library(tidyverse)
library(LK.Toolbox)
library(here)


#### Functions -----------------------------


#### Data Input -----------------------------
here::here()

alc_data <- read_excel("data/Alcohol.xlsx")
alc_data$Run <- as.factor(alc_data$Run)

#### Data Cleaning -----------------------------



#### least squares --------------------------------

# fit <- lm(Response~Conc, data = alc_data, weights = (1/Conc))
 fit <- lm(Response~Conc, data = alc_data)
summary_fit <- summary(fit)

#### Visualising Data -----------------------------

alc_plot <- ggplot(alc_data, aes(x = Conc, y = Response, fill = Run)) +
        geom_point(shape = 21, size = 3)+
        # geom_smooth(method = "lm")+
        geom_abline(slope = summary_fit$coefficients[2], intercept = summary_fit$coefficients[1])+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 1))+
                labs(        title = "Alcohol Standards",
                                subtitle = "",
                                caption = "",
                                                        x = "µg/mL",
                                                        y = "Ratio")
alc_plot

ggsave("alcohol_runs.png", dpi = 100, width = 8, height = 5)


Std_error <- summary_fit$sigma
Std_error
