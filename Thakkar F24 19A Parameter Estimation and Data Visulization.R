#### LOAD PACKAGES ####
library(dplyr)
library(tidyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(broom)
library(epitools)

#### EXPONENTIAL MODEL FOR COMMUNITY INCIDENCE ####
Fit <- structure(list(Time = c(seq(0,16,1)), 
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

attach(Fit)
names(Fit)

exponential.model <- lm(log(CommunityIncidence)~ Time)
summary(exponential.model)

timevalues <- seq(0,16,1)

Counts.exponential1 <- exp(predict(exponential.model,list(Time=timevalues)))
Counts.exponential2 <- 0.68*Counts.exponential1

plot(Time, CommunityIncidence,pch=19)
lines(timevalues, Counts.exponential1,lwd=3, col = "#F8766D", xlab = "Week", ylab = "Community Incidence")

plot(Time, SchoolIncidence,pch=19)
lines(timevalues, Counts.exponential2,lwd=3, col = "#00BFC4", xlab = "Week", ylab = "School Incidence")

#### GRAPHING BASELINE MODEL VS OBSERVED ####

Baseline <- data.frame(time = c(seq(0,16,1)),
                predicted = c(0,
                              1.7130066008870763,
                              2.775883369811892,
                              3.836953455302492,
                              4.995626534467406,
                              6.277877169266246,
                              7.700001547690655,
                              9.277904790173721,
                              11.028759887362543,
                              12.971480793502517,
                              15.126987415286676,
                              17.518438935326134,
                              20.171473385627746,
                              23.114466210579184,
                              26.37881248466417,
                              29.99923628712784,
                              34.01412544929426),
                observed = c(1,1,2,2,3,4,5,8,13,
                             16,17,19,27,29,31,
                             32,34))

Baseline <- gather(Baseline,key="predobs",value="infections",2:3)

head(Baseline)

ggplot(data=Baseline,aes(x=time,y=infections, color = predobs))+
  geom_line(size=1)+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  xlab("Week")+
  ylab("Number of SARS-CoV-2 Infections")+
  labs(color="Legend")+
  theme(legend.position=c(0.06,0.9))



#### NO MASKING ####

NoMask <- data.frame(time = c(seq(0,16,1)),
                predicted = c(0,
                              1.8456714500446870,
                              3.2619077628186504,
                              4.7465314821116250,
                              6.3651700634809590,
                              8.1470441282375730,
                              10.116776726703483,
                              12.298565423638674,
                              14.717620375301435,
                              17.401010345943625,
                              20.378261217731186,
                              23.681836527477053,
                              27.347571439392365,
                              31.415095944159766,
                              35.928273429999320,
                              40.935662797480210,
                              46.491023268790280),
                observed = c(1,1,2,2,3,4,5,8,13,
                             16,17,19,27,29,31,
                             32,34))

NoMask <- gather(NoMask,key="predobs",value="infections",2:3)

ggplot(data=NoMask,aes(x=time,y=infections, color = predobs))+
  geom_line(size=1)+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  xlab("Week")+
  ylab("Number of SARS-CoV-2 Infections")+
  labs(color="Legend")+
  theme(legend.position=c(0.05,0.9))



#### NO VENTILATION ####

NoVent <- data.frame(time = c(seq(0,16,1)),
                     predicted = c(0,
                                   1.7169834164712747,
                                   2.7831807401528550,
                                   3.8398691892068957,
                                   4.9888157132518800,
                                   6.2584767524694880,
                                   7.6663626933187370,
                                   9.2288884014346520,
                                   10.963465794727892,
                                   12.889177143119785,
                                   15.027126288749253,
                                   17.400713880894262,
                                   20.035908017552547,
                                   22.961531373869040,
                                   26.209575730678374,
                                   29.815552274738800,
                                   33.818876050213426),
                     observed = c(1,1,2,2,3,4,5,8,13,
                                  16,17,19,27,29,31,
                                  32,34))
NoVent <- gather(NoVent,key="predobs",value="infections",2:3)

ggplot(data=NoVent,aes(x=time,y=infections, color = predobs))+
  geom_line(size=1)+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  xlab("Week")+
  ylab("Number of SARS-CoV-2 Infections")+
  labs(color="Legend")+
  theme(legend.position=c(0.05,0.9))



#### NO MASKING, NO VENTILATION ####

NoMaskNoVent <- data.frame(time = c(seq(0,16,1)),
                     predicted = c(0,
                                   1.9582047934636067,
                                   3.7302998978076323,
                                   5.7373377354963555,
                                   8.0114354784573760,
                                   10.569168830495663,
                                   13.431358278886254,
                                   16.623720035554268,
                                   20.176683700547496,
                                   24.125329511971180,
                                   28.509461315782744,
                                   33.373792709845965,
                                   38.768223745581720,
                                   44.748194120965444,
                                   51.375103041725190,
                                   58.716796209141236,
                                   66.84809590518435),
                     observed = c(1,1,2,2,3,4,5,8,13,
                                  16,17,19,27,29,31,
                                  32,34))

NoMaskNoVent <- gather(NoMaskNoVent,key="predobs",value="infections",2:3)

ggplot(data=NoMaskNoVent,aes(x=time,y=infections, color = predobs))+
  geom_line(size=1)+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual(values = c("#00BFC4","#F8766D"))+
  xlab("Week")+
  ylab("Number of SARS-CoV-2 Infections")+
  labs(color="Legend")+
  theme(legend.position=c(0.05,0.9))



#### MEAN INCUBATION PERIOD ####

# create data frame
IncPer <- data.frame(days = c(2,2,2,
                              3,3,3,3,3,3,3,
                              4,4,4,4,4,4,4,
                              5,5,5,5,5,
                              6,6))
head(IncPer)

# calculate mean incubation period
mean(IncPer$days)

# plot histogram
ggplot(data=IncPer,aes(x=days))+
  geom_histogram(binwidth=1,color="white",fill="#00BFC4")+
  theme_classic()



#### INTERRUPTED TIME SERIES FOR MASKING ####

# create data frame
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

# create ITS plot
ggplot(itsData, aes(week)) +  
  geom_line(aes(y = School_Incidence), color = "#00BFC4",size=1.2) +
  geom_line(aes(y = Community_Incidence), color = "black",linetype="dashed",size=1.2)+
  geom_point(aes(y = School_Incidence), color = "#00BFC4",size=1.9) +
  geom_point(aes(y = Community_Incidence), color = "black",size=1.9)+
  geom_ribbon(aes(ymin=y3,ymax=y4),fill="#00BFC4",alpha=0.2)+
  xlab("Week") + ylab("COVID-19 Cases per 100,000")+
  theme_classic()
