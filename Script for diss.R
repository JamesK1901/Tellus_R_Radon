#SCRIPT FOR DISSERTATION-STATISTICAL ANALYSIS


rm(list=ls()) # clear previous work

#read in data
radiometric_data <- read.csv("Land_data.csv")

#load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(ggpubr)
library(car)

options(scipen=999) #to get rid of scientific notation

#quick summaries of columns
summary(radiometric_data)

#uranium plot
radiometric_data %>%
  ggplot(mapping=aes(x=D_URA)) +
  geom_histogram(binwidth = 0.5, color = "#000000", fill = "#0099F8") +
  labs(title = "Uranium distribution",
       x="Uranium count (ppm)", y="Frequency") +
  theme_classic() 

ggsave(filename="my_plot.png", dpi=600)   #save for download

#potassium plot
radiometric_data %>%
  ggplot(mapping=aes(x=D_KAL)) +
  geom_histogram(binwidth = 0.2, color = "#000000", fill = "#0099F8") +
  labs(title = "Potassium distribution",
       x="Potassium count (ppm)", y="Frequency") +
  theme_classic()

ggsave(filename="my_plot.png", dpi=600)

#thorium plot
radiometric_data %>%
  ggplot(mapping=aes(x=D_THO)) +
  geom_histogram(binwidth = 0.5, color = "#000000", fill = "#0099F8") +
  labs(title = "Thorium distribution",
       x="Thorium count (ppm)", y="Frequency") +
  theme_classic()


#all radiometric points plot
radiometric_data %>%
  ggplot(mapping=aes(x=D_TOT_CPS)) +
  geom_histogram(binwidth = 0.5, color = "#000000", fill = "#0099F8") +
  labs(title = "Total radiation distribution",
       x="Total radiation count (ppm)", y="Frequency") +
  theme_classic()



#RadonClass and DEM graphs use updated data, as they had to be added to original file
updated_data %>%
  ggplot(mapping=aes(x=RadonClass)) +
  geom_histogram(binwidth = 0.5, color = "#000000", fill = "#0099F8") +
  labs(title = "Radon Level Distribution",
       x="Radon Level", y="Frequency") +
  theme_classic()


updated_data %>%
  ggplot(mapping=aes(x=DEM)) +
  geom_histogram(binwidth = 15, color = "#000000", fill = "#0099F8") +
  labs(title = "Digital Elevation Model",
       x="Height in metres (M)", y="Frequency") +
  theme_classic()











###############################################################################################################################################

#exploring relationships between the data, initial exploration of data

radiometric_data %>%
  ggplot(mapping=aes(x=DTM, y=D_KAL)) +
  geom_point() +
  geom_smooth(method=lm)

radiometric_data %>%
  ggplot(mapping=aes(x=DTM, y=D_THO)) +
  geom_point() +
  geom_smooth(method=lm)

radiometric_data %>%
  ggplot(mapping=aes(x=DTM, y=D_URA)) +
  geom_point() +
  geom_smooth(method=lm)

#EXPLORING TRENDS BETWEEN DTM AND ELEMENTS^^ - was not used



cor(radiometric_data$DTM, radiometric_data$D_KAL)
#0.09973339

cor(radiometric_data$DTM, radiometric_data$D_THO)
#0.4381893

cor(radiometric_data$DTM, radiometric_data$D_URA)
#0.3511536

#CORRELATION COEFFICIENT BETWEEN DTM AND ELEMENTS - NOT USED^



radiometric_data %>%
  ggplot(mapping=aes(x=URA, y=D_KAL)) +
  geom_point() +
  geom_smooth(method=lm)

radiometric_data %>%
  ggplot(mapping=aes(x=D_URA, y=D_THO)) +
  geom_point() +
  geom_smooth(method=lm)

radiometric_data %>%
  ggplot(mapping=aes(x=D_THO, y=D_KAL)) +
  geom_point() +
  geom_smooth(method=lm)

#PLOT RELATIONSHIPS BETWEEN ELEMENTS - NOT USED^


cor.test(radiometric_data$D_URA, radiometric_data$D_KAL)
#0.467216

cor(radiometric_data$D_URA, radiometric_data$D_THO)
#0.7120289

cor(radiometric_data$D_THO, radiometric_data$D_KAL)
#0.6046123

#CORRELATION COEFFICIENTS BETWEEN ELEMENTS - NOT USED^

###################################################################################################################################################################################

#Z score standardisation, UPDATED/COMBINED DATASET LOADED IN 

updated_data <- read.csv("UPDATED_RAD_BEDROCK_SUPERFIC_DEM_RADON.csv") 

updated_data_standardise <- updated_data %>% mutate_at(c('D_KAL', 'D_THO', 'D_URA','DEM', 'RadonClass'), ~(scale(.) %>% as.vector))  #this standardises the data
print(updated_data)





#linear regression

#regression model between radon and potassium

Rad_Kal.lm <- lm(D_KAL ~ RadonClass, data = updated_data_standardise)
summary(Rad_Kal.lm)
#p-value way under 0.5

#regression model between radon and thorium

Rad_Tho.lm <- lm(D_THO ~ RadonClass, data = updated_data_standardise)
summary(Rad_Tho.lm)
#p-value way under 0.5

#regression model between radon and uranium

Rad_Ura.lm <- lm(D_URA ~ RadonClass, data = updated_data_standardise)
summary(Rad_Ura.lm)
#p-value way under 0.5

#regression model between radon and DEM

Rad_DEM.lm <- lm(DEM ~ RadonClass, data = updated_data_standardise)
summary(Rad_DEM.lm)
#p-value way under 0.5

#multiple regression model between radon and elements

radiometry.lm<-lm(RadonClass ~ D_URA + D_KAL + D_THO, data = updated_data_standardise)

summary(radiometry.lm)
#p-value way under 0.5



#normality tests (additional tool for normality test)

library("dgof")   #for Kolmogorov-Smirnov test

DEM_DATA<- read.csv("DEM.csv")
ks.test(DEM_DATA, "pnorm")
#p-value is less than .05, we reject the null hypothesis

D_KAL_DATA <- read.csv("D_KAL.csv")
ks.test(D_KAL_DATA, "pnorm")
#p-value is less than .05, we reject the null hypothesis

D_THO_DATA <- read.csv("D_THO.csv")
ks.test(D_THO_DATA, "pnorm")
#p-value is less than .05, we reject the null hypothesis

D_URA_DATA <- read.csv("D_URA.csv")
ks.test(D_URA_DATA, "pnorm")
#p-value is less than .05, we reject the null hypothesis


#pearson and spearman correlation coefficients

Rad_DEM_TEST <- cor.test(updated_data_standardise$RadonClass, updated_data_standardise$DEM, method = "spearman")
Rad_DEM_TEST
#pearson - 0.3316113
#spearman - 0.1469214

Rad_THO_TEST <- cor.test(updated_data_standardise$RadonClass, updated_data_standardise$D_THO, method = "spearman")
Rad_THO_TEST
#pearson - 0.4881515
#spearman - 0.3716259

Rad_KAL_TEST <- cor.test(updated_data_standardise$RadonClass, updated_data_standardise$D_KAL, method = "spearman")
Rad_KAL_TEST
#pearson - 0.2628496
#spearman - 0.203817

Rad_URA_TEST <- cor.test(updated_data_standardise$RadonClass, updated_data_standardise$D_URA, method = "spearman")
Rad_URA_TEST
#pearson - 0.4160953
#spearman - 0.2859677

#P-value way below 0.5 for all, all positive relationships


#graph showing standardised radon and potassium with correlation coefficient


ggscatter(updated_data_standardise, x = "RadonClass", y = "D_KAL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radon Level (Standardised)", ylab = "Potassium count (Standardised)")

#graph showing standardised radon and thorium with correlation coefficient


ggscatter(updated_data_standardise, x = "RadonClass", y = "D_THO", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radon Level (Standardised)", ylab = "Thorium count (Standardised)")


#graph showing standardised radon and uranium with correlation coefficient


ggscatter(updated_data_standardise, x = "RadonClass", y = "D_URA", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radon Level (Standardised)", ylab = "Uranium count (Standardised)")

#graph showing standardised radon and DEM with correlation coefficient

ggscatter(updated_data_standardise, x = "RadonClass", y = "DEM", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radon Level (Standardised)", ylab = "DEM (Standardised)")


#updated geology codes, this is so geology can be used in analysis

updated_geology <- read.csv("UPDATED_GEOLOGY.csv")

#experimenting with linear regression with geology and radon
RAD_GEOLOGY_TEST <- cor.test(updated_geology$RadonClass, updated_geology$Bedrock_Code, method = "spearman")
RAD_GEOLOGY_TEST

#TABLE PRODUCTION FOR COUNTS OF RADIOMETRY VALUES IN GEOLOGY AREAS, results noted on word

bedrock_one_granite <- filter(updated_geology,
       Bedrock_Code==1)
table(bedrock_one_granite['RadonClass'])




bedrock_two_AcidicVolcanics <- filter(updated_geology,
                      Bedrock_Code==2)
table(bedrock_two_AcidicVolcanics['RadonClass'])



bedrock_three_grandodorite <- filter(updated_geology,
                                      Bedrock_Code==3)
table(bedrock_three_grandodorite['RadonClass'])



bedrock_four_basalt <- filter(updated_geology,
                                     Bedrock_Code==4)
table(bedrock_four_basalt['RadonClass'])



bedrock_five_microgabbro <- filter(updated_geology,
                              Bedrock_Code==5)
table(bedrock_five_microgabbro['RadonClass'])



bedrock_six_sandstone <- filter(updated_geology,
                                   Bedrock_Code==6)
table(bedrock_six_sandstone['RadonClass'])



bedrock_seven_argillaceous <- filter(updated_geology,
                                Bedrock_Code==7)
table(bedrock_seven_argillaceous['RadonClass'])



bedrock_eight_sandstone <- filter(updated_geology,
                                     Bedrock_Code==8)
table(bedrock_eight_sandstone['RadonClass'])



#superficial table production, counts of radioactive elements in superficial data, results noted on word


superficial_ten_peat <- filter(updated_geology,
                              Superficial_Code==10)
table(superficial_ten_peat['RadonClass'])



superficial_twenty_diamicton <- filter(updated_geology,
                               Superficial_Code==20)
table(superficial_twenty_diamicton['RadonClass'])



superficial_thirty_sand <- filter(updated_geology,
                                       Superficial_Code==30)
table(superficial_thirty_sand['RadonClass'])



superficial_forty_clay <- filter(updated_geology,
                                  Superficial_Code==40)
table(superficial_forty_clay['RadonClass'])



superficial_fifty_gravel <- filter(updated_geology,
                                 Superficial_Code==50)
table(superficial_fifty_gravel['RadonClass'])



superficial_zero <- filter(updated_geology,
                                   Superficial_Code==0)
table(superficial_zero['RadonClass'])


#END OF CODE
