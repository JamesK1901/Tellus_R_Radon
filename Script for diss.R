rm(list=ls()) # clear previous work

radiometric_data <- read.csv("Land_data.csv")
library(tidyverse)
library("dplyr")
library(ggplot2)

options(scipen=999) #for scientific notation

str(radiometric_data)
summary(radiometric_data)

radiometric_data %>%
  ggplot(mapping=aes(x=D_URA)) +
  geom_histogram(binwidth = 0.5, color = "#000000", fill = "#0099F8") +
  labs(title = "Uranium distribution",
       x="Uranium count (ppm)", y="Frequency") +
  theme_classic() 

ggsave(filename="my_plot.png", dpi=600)


radiometric_data %>%
  ggplot(mapping=aes(x=D_KAL)) +
  geom_histogram(binwidth = 0.2, color = "#000000", fill = "#0099F8") +
  labs(title = "Potassium distribution",
       x="Potassium count (ppm)", y="Frequency") +
  theme_classic()

ggsave(filename="my_plot.png", dpi=600)

radiometric_data %>%
  ggplot(mapping=aes(x=D_TOT_CPS)) +
  geom_histogram(binwidth = 0.5, color = "#000000", fill = "#0099F8") +
  labs(title = "Total radiation distribution",
       x="Total radiation count (ppm)", y="Frequency") +
  theme_classic()

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




radiometric_data %>%
  ggplot(mapping=aes(x=D_THO)) +
  geom_histogram(binwidth = 0.5, color = "#000000", fill = "#0099F8") +
  labs(title = "Thorium distribution",
       x="Thorium count (ppm)", y="Frequency") +
  theme_classic()

min(updated_data, "D_KAL")

summary(radiometric_data)




#exploring relationships between the data

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

#EXPLORING TRENDS BETWEEN DTM AND ELEMENTS^^



cor(radiometric_data$DTM, radiometric_data$D_KAL)
#0.09973339

cor(radiometric_data$DTM, radiometric_data$D_THO)
#0.4381893

cor(radiometric_data$DTM, radiometric_data$D_URA)
#0.3511536

#CORRELATION COEFFICIENT BETWEEN DTM AND ELEMENTS



radiometric_data %>%
  ggplot(mapping=aes(x=URA, y=D_KAL)) +
  geom_point() +
  geom_smooth(method=lm)

radiometric_data %>%
  ggplot(mapping=aes(x=URA, y=D_THO)) +
  geom_point() +
  geom_smooth(method=lm)

radiometric_data %>%
  ggplot(mapping=aes(x=THO, y=D_KAL)) +
  geom_point() +
  geom_smooth(method=lm)



#RELATIONSHIP BETWEEN ELEMENTS


cor.test(radiometric_data$D_URA, radiometric_data$D_KAL)
#0.467216

cor(radiometric_data$D_URA, radiometric_data$D_THO)
#0.7120289

cor(radiometric_data$D_THO, radiometric_data$D_KAL)
#0.6046123

#CORRELATION COEFFICIENTS BETWEEN ELEMENTS^^

#Z score standardisation

updated_data <- read.csv("UPDATED_RAD_BEDROCK_SUPERFIC_DEM_RADON.csv") 

library(dplyr)

updated_data_standardise <- updated_data %>% mutate_at(c('D_KAL', 'D_THO', 'D_URA','DEM', 'RadonClass'), ~(scale(.) %>% as.vector))  #this standardises the data
print(updated_data)





#linear regression

install.packages("ggpubr")
install.packages("car")

library(ggplot2)
library(broom)
library(ggpubr)
library(car)



Rad_Kal.lm <- lm(D_KAL ~ RadonClass, data = updated_data_standardise)
summary(Rad_Kal.lm)
#p-value way under 0.5


Rad_Tho.lm <- lm(D_THO ~ RadonClass, data = updated_data_standardise)
summary(Rad_Tho.lm)
#p-value way under 0.5

Rad_Ura.lm <- lm(D_URA ~ RadonClass, data = updated_data_standardise)
summary(Rad_Ura.lm)
#p-value way under 0.5

Rad_DEM.lm <- lm(DEM ~ RadonClass, data = updated_data_standardise)
summary(Rad_DEM.lm)

radiometry.lm<-lm(RadonClass ~ D_URA + D_KAL + D_THO, data = updated_data_standardise)

summary(radiometry.lm)

#normality tests (additional tool for normality test)

library("dgof")   #FOR KS.TEST


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


#spearman correlation coefficient

Rad_DEM_TEST <- cor.test(updated_data_standardise$RadonClass, updated_data_standardise$DEM, method = "spearman")
Rad_DEM_TEST
#0.3316113
#spearman - 0.1469214

Rad_THO_TEST <- cor.test(updated_data_standardise$RadonClass, updated_data_standardise$D_THO, method = "spearman")
Rad_THO_TEST
#0.4881515
#spearman - 0.3716259

Rad_KAL_TEST <- cor.test(updated_data_standardise$RadonClass, updated_data_standardise$D_KAL, method = "spearman")
Rad_KAL_TEST
#0.2628496
#spearman - 0.203817

Rad_URA_TEST <- cor.test(updated_data_standardise$RadonClass, updated_data_standardise$D_URA, method = "spearman")
Rad_URA_TEST
#0.4160953
#spearman - 0.2859677

#P-value way below 0.5 for all, all positive relationships


library(ggplot2)




library("ggpubr")

ggscatter(updated_data_standardise, x = "RadonClass", y = "D_KAL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radon Level (Standardised)", ylab = "Potassium count (Standardised)")

ggscatter(updated_data_standardise, x = "RadonClass", y = "D_THO", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radon Level (Standardised)", ylab = "Thorium count (Standardised)")

ggscatter(updated_data_standardise, x = "RadonClass", y = "D_URA", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radon Level (Standardised)", ylab = "Uranium count (Standardised)")

ggscatter(updated_data_standardise, x = "RadonClass", y = "DEM", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radon Level (Standardised)", ylab = "DEM (Standardised)")



#install.packages("ggstatsplot")
#library(ggside)
#library(ggstatsplot)
#ggscatterstats(
#  data = updated_data_standardise,
 # x = RadonClass,
#  y = D_KAL,
  
 # bf.message = FALSE
#)




#par(mfrow=c(2,2))
#plot(radiometry.lm)
#par(mfrow=c(1,1))


















#KAL_GRAPH <- ggplot(updated_data_standardise, aes(x=RadonClass, y=D_KAL)) +
 # geom_point()

#KAL_GRAPH <- KAL_GRAPH + geom_smooth(method="lm", col="black")

#KAL_GRAPH

#plot(D_URA ~ RadonClass, data = updated_data_standardise)
#updated_data_standardise %>%
#  ggplot(mapping=aes(x=RadonClass, y=D_KAL)) +
 # geom_point() +
  #geom_smooth(method=lm)
#scatterplot(D_KAL ~ RadonClass, data = updated_data_standardise)



#updated geology codes

updated_geology <- read.csv("UPDATED_GEOLOGY.csv")

RAD_GEOLOGY_TEST <- cor.test(updated_geology$RadonClass, updated_geology$Bedrock_Code, method = "spearman")
RAD_GEOLOGY_TEST

ggscatter(updated_geology, x = "RadonClass", y = "Bedrock_Code", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Radon Level", ylab = "Bedrock (codes")


bedrock_one_granite <- filter(updated_geology,
       Bedrock_Code==1)

table(bedrock_one_granite['RadonClass'])
#granite occurs 5934 times for radonclass 5, and 5172 for radonclass 6


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



#superficial codes


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
