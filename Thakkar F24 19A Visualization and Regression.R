# Math 19a Final Project
# Harvard College
# Completed December 09, 2024
# Pavan V. Thakkar

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

IRR<-matrix(c(10,71,2100,2039),nrow = 2, ncol = 2)
rateratio.wald(IRR)



#### COMMUNITY INCIDENCE AND MASKING POLICY ANALYSIS AND ADDITIONAL SUPPLEMENTAL ANALYSES ####

# Bar Graph #

BarGraphData <- data.frame(CommunityIncidence = c("0002","0010","0050","0100","0500","1000","2000"),
                  NoMaskMandate = c(2,4,12,23,96,166,258),
                  MaskMandate = c(0,0,1,2,9,16,27))

BarGraphData <- gather(BarGraphData,key="MaskPolicy",value="SecondaryInfections",2:3)

ggplot(data = BarGraphData)+
  geom_bar(aes(x=CommunityIncidence,y=SecondaryInfections,fill=MaskPolicy,color=MaskPolicy),stat = "identity",
           position = position_dodge())+
  xlab("Community Incidence per 100,000 Persons") + ylab("Number of Secondary SARS-CoV-2 Infections")+
  theme_classic()
  
# Linear Regressions #

LinRegCommIncNoMask <- data.frame(CommunityIncidence = c(2,10,50,100,500,1000,2000),
                                  SecondaryInfections = c(2,4,12,23,96,166,258))

LinModCommIncNoMask <- lm(SecondaryInfections~CommunityIncidence, data=LinRegCommIncNoMask)
summary(LinModCommIncNoMask)

ggplot(data=LinRegCommIncNoMask, aes(x=CommunityIncidence, y=SecondaryInfections)) +
  geom_point() +
  stat_smooth(method="lm",se=FALSE,color="#F8766D")+
  labs(x = "Community Incidence per 100,000 Persons",
       y = "Predicted Secondary Infections")+
  theme_classic()

  ##
  
LinRegCommIncMask <- data.frame(CommunityIncidence = c(2,10,50,100,500,1000,2000),
                                    SecondaryInfections = c(0,0,1,2,9,16,27))
  
LinModCommIncMask <- lm(SecondaryInfections~CommunityIncidence, data=LinRegCommIncMask)
summary(LinModCommIncMask)
  
ggplot(data=LinRegCommIncMask, aes(x=CommunityIncidence, y=SecondaryInfections)) +
  geom_point() +
  stat_smooth(method="lm",se=FALSE,color="#00BFC4")+
labs(x = "Community Incidence per 100,000 Persons",
     y = "Secondary Infections")



dfx <- data.frame(time = c(seq(0,16,1)),
                  "0002 per 100,000" = c(0,
                          1.7038292858595614,
                          2.5427870021442898,
                          3.060709016293248,
                          3.399967449488923,
                          3.633451966199988,
                          3.8038667196582963,
                          3.93664630236676,
                          4.0469671978217345,
                          4.143886912233917,
                          4.232811081995397,
                          4.316965876983234,
                          4.398276687462223,
                          4.47789257173169,
                          4.556499219230511,
                          4.634505742506043,
                          4.7121561324876),
                  "0010 per 100,000" = c(0,
                           1.7396361734390384,
                           2.7109830881928567,
                           3.431236186595899,
                           4.01534807306014,
                           4.519090098526016,
                           4.974907200632111,
                           5.402112474756104,
                           5.812231563139481,
                           6.212144234979199,
                           6.605957570244411,
                           6.996123428737934,
                           7.384105793856603,
                           7.770778871473538,
                           8.15666483767808,
                           8.54207579094622,
                           8.927198498129854),
                  "0050 per 100,000" = c(0,
                          1.9186434476714596,
                          3.5516325592713294,
                          5.282514928951149,
                          7.088700857180296,
                          8.940032393719477,
                          10.817454064490956,
                          12.709583203133704,
                          14.609687897743017,
                          16.513814249851013,
                          18.419663160451066,
                          20.325918192804604,
                          22.231845345996177,
                          24.137053735781635,
                          26.041353344722296,
                          27.944670397683176,
                          29.846997089944477),
                  "0100 per 100,000" = c(0,
                           2.1423389260464867,
                           4.601669179866739,
                           7.593436828927517,
                           10.922088734295937,
                           14.449275507030254,
                           18.09110293257924,
                           21.797630949763203,
                           25.53931835487772,
                           29.29881304090451,
                           33.06602656376781,
                           36.835187859165316,
                           40.60308211608753,
                           44.36800016730837,
                           48.129112652405716,
                           51.88609825277112,
                           55.63892357377637),
                  "0500 per 100,000" = c(0,
                           3.9293599196599405,
                           12.971059850668693,
                           25.954586878253654,
                           41.260615624862865,
                           57.85638982079235,
                           75.12383260666952,
                           92.70043741541248,
                           110.37810044948878,
                           128.04108442683315,
                           145.62828787809391,
                           163.11047652384457,
                           180.47664544172596,
                           197.7259279108439,
                           214.86283905295724,
                           231.89452430012236,
                           248.82918993284892),
                  "1000 per 100,000" = c(0,
                            6.156795063119442,
                            23.356050916938162,
                            48.59445784783335,
                            78.37916680678657,
                            110.49775068219384,
                            143.63915411094618,
                            177.0541529300839,
                            210.33442826059786,
                            243.27412545564906,
                            275.784581766713,
                            307.8426917274833,
                            339.4602440824636,
                            370.6661276973994,
                            401.4962560065432,
                            431.9880781188065,
                            462.177744472054),
                  "2000 per 100,000" = c(0,
                            10.590631602593731,
                            43.87355351749269,
                            92.8616149788242,
                            150.04181782322544,
                            210.6991093458011,
                            272.12584150261443,
                            332.8828913086961,
                            392.2938255310437,
                            450.1181015464757,
                            506.34874020773304,
                            561.0924776352321,
                            614.5021711272839,
                            666.7407990068114,
                            717.9638038935475,
                            768.3115887213571,
                            817.9073449980591))

dfx <- gather(dfx,key="CommunityIncidence",value="TotalInfections",2:8)

dfx


ggplot(data=dfx,aes(x=time,y=TotalInfections, color = CommunityIncidence))+
  geom_line(size=1)+
  geom_point(size=2)+
  theme_classic()+
  xlab("Week")+
  ylab("Number of SARS-CoV-2 Infections")+
  labs(color="Legend")

###

InfectByVaxMaskData <- data.frame(VaccinationCoverage = c(0,25,50,75,100),
                                  NoMaskMandate = c(52,44,36,30,24),
                                  MaskMandate = c(33,30,27,23,20))

LmTotalByVaxNoMask <- lm(NoMaskMandate~VaccinationCoverage, data=InfectByVaxMaskData)
summary(LmTotalByVaxNoMask)

LmTotalByVaxMask <- lm(MaskMandate~VaccinationCoverage, data=InfectByVaxMaskData)
summary(LmTotalByVaxMask)

InfectByVaxMaskData <- gather(InfectByVaxMaskData,key="MaskPolicy",value="TotalInfections",2:3)

ggplot(data=InfectByVaxMaskData,aes(x=VaccinationCoverage,y=TotalInfections,color=MaskPolicy))+
  geom_point(size=2)+
  geom_smooth(method=lm,se=FALSE)+
  theme_classic()+
  xlab("Vaccination Coverage (%)")+
  ylab("Total Number of SARS-CoV-2 Infections")+
  ylim(0,60)


SecondaryByVaxMaskData <- data.frame(VaccinationCoverage = c(0,25,50,75,100),
                                  NoMaskMandate = c(20,15,10,8,5),
                                  MaskMandate = c(2,1,1,1,1))


LmSecondaryByVaxNoMask <- lm(NoMaskMandate~VaccinationCoverage, data=SecondaryByVaxMaskData)
summary(LmSecondaryByVaxNoMask)

LmSecondaryByVaxMask <- lm(MaskMandate~VaccinationCoverage, data=SecondaryByVaxMaskData)
summary(LmSecondaryByVaxMask)

