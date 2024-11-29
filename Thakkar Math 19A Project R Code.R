#### LOAD PACKAGES ####
library(dplyr)
library(tidyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(broom)
library(epitools)

#### EXPONENTIAL MODEL FOR COMMUNITY INCIDENCE ####
A <- structure(list(Time = c(seq(0,16,1)), 
                    CommunityIncidence = c(0.001036604,
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
                                           0.003865427),
                       SchoolIncidence = c(0.000473934,
                                           0,
                                           0.000473934,
                                           0,
                                           0.000473934,
                                           0.000473934,
                                           0.000473934,
                                           0.001421801,
                                           0.002369668,
                                           0.001421801,
                                           0.000473934,
                                           0.000947867,
                                           0.003791469,
                                           0.000947867,
                                           0.000947867,
                                           0.000473934,
                                           0.000947867)), 
                .Names = c("Time", "CommunityIncidence","SchoolIncidence"), class = "data.frame")

attach(A)
names(A)

exponential.model <- lm(log(CommunityIncidence)~ Time)
summary(exponential.model)

timevalues <- seq(0,16,1)

Counts.exponential1 <- exp(predict(exponential.model,list(Time=timevalues)))
Counts.exponential2 <- 0.72*Counts.exponential1

plot(Time, CommunityIncidence,pch=19)
lines(timevalues, Counts.exponential1,lwd=3, col = "#F8766D", xlab = "Week", ylab = "Community Incidence")

plot(Time, SchoolIncidence,pch=19)
lines(timevalues, Counts.exponential2,lwd=3, col = "#00BFC4", xlab = "Week", ylab = "School Incidence")

#### GRAPHING BASELINE MODEL VS OBSERVED ####

B <- data.frame(time = c(seq(0,16,1)),
                predicted = c(0,
                            3.97166991419933,
                            5.54231230220643,
                            6.62155733927081,
                            7.64326308201649,
                            8.72717492895942,
                            9.91565072219823,
                            11.2306625253039,
                            12.6892977312013,
                            14.308342233393,
                            16.1057712377832,
                            18.1013404804322,
                            20.3169211372919,
                            22.7767755181771,
                            25.5078328978135,
                            28.5399877215478,
                            31.9064251312341),
                observed = c(1,
                             1,
                             2,
                             2,
                             3,
                             4,
                             5,
                             8,
                             13,
                             16,
                             17,
                             19,
                             27,
                             29,
                             31,
                             32,
                             34))

head(B)

ggplot(data=B,aes(x=Time))+
  geom_line(aes(x=Time,y=observed), color = "#00BFC4",size=1.5)+
  geom_point(aes(x=Time,y=observed), color = "#00BFC4",size=2.5)+
  geom_line(aes(x=Time,y=predicted),color="#F8766D", size=1.5)+
  geom_point(aes(x=Time,y=predicted),color="#F8766D",size=2.5)+
  theme_classic()+
  xlab("Week")+
  ylab("Number of COVID-19 Cases")

#### NO MITIGATION MEASURES ####

C <- data.frame(time = c(seq(0,16,1)),
                predicted = c(0,
                              4.59707503482223,
                              7.81942994317941,
                              10.7591314616023,
                              13.5999610382176,
                              16.4492599309373,
                              19.3915151136817,
                              22.4972410645091,
                              25.8278898870735,
                              29.4394335356113,
                              33.3850704939516,
                              37.7173089599354,
                              42.4896052734774,
                              47.7576915464544,
                              53.5806852143186,
                              60.0220546435963,
                              67.1504914112009),
                observed = c(1,
                             1,
                             2,
                             2,
                             3,
                             4,
                             5,
                             8,
                             13,
                             16,
                             17,
                             19,
                             27,
                             29,
                             31,
                             32,
                             34))

head(C)

ggplot(data=C,aes(x=Time))+
  geom_line(aes(x=Time,y=observed), color="#00BFC4",size=1.5)+
  geom_point(aes(x=Time,y=observed), color="#00BFC4",size=2.5)+
  geom_line(aes(x=Time,y=predicted),color="#F8766D", size=1.5)+
  geom_point(aes(x=Time,y=predicted),color="#F8766D",size=2.5)+
  theme_classic()+
  xlab("Week")+
  ylab("Number of COVID-19 Cases")


#### MEAN INCUBATION PERIOD ####
IncPer <- data.frame(days = c(2,2,2,
                              3,3,3,3,3,3,3,
                              4,4,4,4,4,4,4,
                              5,5,5,5,5,
                              6,6))
head(IncPer)

ggplot(data=IncPer,aes(x=days))+
  geom_histogram(binwidth=1,color="white",fill="#00BFC4")+
  theme_classic()


#### INTERRUPTED TIME SERIES FOR MASKING ####
itsData <- data.frame(week=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6),
                      School_Incidence=c(2654.028436,
                                         1706.161137,
                                         1800.947867,
                                         710.9004739,
                                         1184.834123,
                                         331.7535545,
                                         0,
                                         189.5734597,
                                         0,
                                         189.5734597,
                                         379.1469194,
                                         2227.488152,
                                         379.1469194),
                      Community_Incidence=c(1548.872614,
                                            2070.417055,
                                            754.1725357,
                                            395.7289374,
                                            265.2302505,
                                            174.9889225,
                                            89.34071595,
                                            60.97143619,
                                            64.12357838,
                                            49.08335705,
                                            40.97784855,
                                            27.10842288,
                                            50.43427513),
                      y3=c(2030,
                           1216,
                           1315,
                           431,
                           803,
                           160,
                           0,
                           73,
                           0,
                           73,
                           192,
                           1679,
                           192),
                      y4=c(3407,
                           2329,
                           2463,
                           1170,
                           1743,
                           683,
                           181.7,
                           486,
                           181.7,
                           486,
                           746,
                           2949,
                           746))

its_plot <- ggplot(itsData, aes(week)) +  
  geom_line(aes(y = School_Incidence), color = "#00BFC4",size=1.2) +
  geom_line(aes(y = Community_Incidence), color = "black",linetype="dashed",size=1.2)+
  geom_point(aes(y = School_Incidence), color = "#00BFC4",size=1.9) +
  geom_point(aes(y = Community_Incidence), color = "black",size=1.9)+
  geom_ribbon(aes(ymin=y3,ymax=y4),fill="#00BFC4",alpha=0.2)+
  xlab("Week") + ylab("COVID-19 Cases per 100,000")+
  theme_classic()


its_plot
