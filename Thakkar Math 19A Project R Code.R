#### LOAD PACKAGES ####
library(dplyr)
library(tidyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(broom)
library(epitools)

A <- structure(list(Time = c(seq(1,21,1)), 
                    Incidence = c(0.001036604,
                                  0.001001481,
                                  0.000637633,
                                  0.000604311,
                                  0.000584497,
                                  0.001123964,
                                  0.000748409,
                                  0.00100058,
                                  0.001245546,
                                  0.000928531,
                                  0.001910198,
                                  0.001398651,
                                  0.002096625,
                                  0.002356902,
                                  0.002583856,
                                  0.003834806,
                                  0.003865427,
                                  0.003292638,
                                  0.004748027,
                                  0.005134389,
                                  0.005687365)), 
                .Names = c("Time", "Incidence"), class = "data.frame")

attach(A)
names(A)

exponential.model <- lm(log(Incidence)~ Time)
summary(exponential.model)

timevalues <- seq(1,21,1)

Counts.exponential2 <- exp(predict(exponential.model,list(Time=timevalues)))

plot(Time, Incidence,pch=21)
lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Week", ylab = "Community Incidence")
