#INF 1344
#Final Project R file
#Group 4

#Importing Libraries
#install.packages("corrplot")
#install.packages("plm")
library(readr)
library(plm)
library(corrplot)
library(tidyverse)

#Loading the dataset
snowpack_1970_1980_clean <- read_csv("~/Downloads/snowpack_1970_1980_clean.csv")
snowpack_2009_2019_clean <- read_csv("~/Downloads/snowpack_2009_2019_clean.xls.csv")
df <- read_csv("~/Downloads/snowpack_1970_2019_clean.csv")

df_1970_80 <- snowpack_1970_1980_clean
df_2009_19 <- snowpack_2009_2019_clean

View(df_1970_80)

#Data Cleaning
df_1970_80$`24_hour_total_precip_mm` <- as.numeric(df_1970_80$`24_hour_total_precip_mm`)
df_2009_19$`24_hour_total_precip_mm` <- as.numeric(df_2009_19$`24_hour_total_precip_mm`)

#Exploratory Data Analysis
View(df)

summary(df)
nrow(df)
ncol(df)

#Finding correlation between our dependent variable and variable of interest
cor(df$air_temp_max_c, df$snowpack_depth_cm, use="complete")

#Prepping data for correlation matrix
df_corr <- df[, c(4,5,6,7,8,9,10)]
View(df_corr)

correlation <- cor(df_corr, method = "pearson", use = "complete")
corrplot(correlation, method="number")


#Data Visualizations
scatter.smooth(df$air_temp_max_c, df$snowpack_depth_cm)

ggplot(df, aes(x=air_temp_max_c, y=df$snowpack_depth_cm)) + 
  geom_point()+
  geom_smooth()+
  ggtitle("Snowpack Depth v Air Temperature (Max)")+
  labs(y = "Snowpack Depth in cm", x = "Air Temperature in C")

ggplot(df, aes(year, snowpack_depth_cm)) + 
  geom_point()

#T Tests
#Creating Vectors of the columns

#Data from 1970 to 1980
df1 <- df_1970_80

airtempmin1 <-df_1970_80$air_temp_min_c
airtempmax1 <-df_1970_80$air_temp_max_c
newsnow1 <- df_1970_80$new_snow_cm
snowpack1 <-df_1970_80$snowpack_depth_cm
seasontotal1 <-df_1970_80$season_total_snow_cm

#Data from 2009 to 2019
df2 <- df_2009_19

airtempmin2<-df_2009_19$air_temp_min_c
airtempmax2<- df_2009_19$air_temp_max_c
newsnow2<-df_2009_19$new_snow_cm
snowpack2<-df_2009_19$snowpack_depth_cm
seasontotal2<-df_2009_19$season_total_snow_cm

#Omit NA Responses from Air Temp Min 1
airtempmin1clean <- na.omit(airtempmin1)

#T-Test for Air Temp Min 1970 to 1980 and 2009 to 2019
t.test(airtempmin1clean,airtempmin2, var.equal=TRUE)

#Omit NA Responses from Air Temp Max 1970 to 1980
airtempmax1clean <- na.omit(airtempmax1)

#T-Test for Air Temp Max 1970 to 1980 and 2009 to 2019 
t.test(airtempmax1clean,airtempmax2, var.equal=TRUE)

#Omit NA Responses for Snowpack 1970 to 1980
snowpack1clean <- na.omit(snowpack1)

#T-Test for Snowpack 1970 to 1980 and 2009 to 2019
t.test(snowpack1clean,snowpack2, var.equal = TRUE)

#Omit NA Responses from New Snow 1970 to 1980
newsnow1clean <- na.omit(newsnow1)

#T-Test for New Snow 1970 to 1980 and 2009 to 2019
t.test(newsnow1clean,newsnow2, var.equal = TRUE)

#Omit NA Responses from Season Total Snow 1970 to 1980
seasontotal1clean <- na.omit(seasontotal1)

#T-Test Season Total Snow 1970-1980 and 2009-2019
t.test(seasontotal1clean,seasontotal2, var.equal = TRUE)

#Panel Linear Modelling
plm_1970_80 <- plm(df_1970_80$snowpack_depth_cm~df_1970_80$air_temp_max_c + df_1970_80$'24_hour_total_precip_mm', data = df_1970_80)
summary(model3)

plm_2009_19 <- plm(df_2009_19$snowpack_depth_cm~df_2009_19$air_temp_max_c + df_2009_19$'24_hour_total_precip_mm', data = df_2009_19)
summary(model)

