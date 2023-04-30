# This document contains the code to perform the regression analysis and create the figures for the sample home outlined in
# A Statistical Machine Learning Approach to Predict Residential Hvac Usage With Lagged Environmental Predictors
# This paper is submitted for possible publication in ASME IMECE 2023


#Code for figures for July 16
#For June 11, dates and titles throughout code must be changed
#Set path for input data file
setwd("")

#Install the R package to read an Excel file 
#install.packages("readxl")
#install.packages("janitor")

#Load the package 
library(readxl)
library(janitor)

#Read in data 
raw_weather <- read_excel("Temp_Humidity Data.xlsx",sheet = 1, col_names = TRUE) # data files are not provided as part of the code

#Separate the temperature and humidity values  
temp_16_22 <- as.matrix(raw_weather[,3]) #temperature 
humid_16_22 <- as.matrix(raw_weather[,4]) # humidity

#Interpolate intermediate temperature values in 15-minute intervals
xt <- c(1: length(temp_16_22)) 
xtout <- seq(from = 1, to = length(temp_16_22), by = 0.25) 
interp_temp <- approx(xt, temp_16_22, xtout, method = "linear") 
temp_15 <- as.data.frame(interp_temp[2]) 
temp_15 <- temp_15[-nrow(temp_15),] # deleting the last row

#Interpolate intermediate humidity values in 15-minute intervals
xh <- c(1: length(humid_16_22))
xhout <- seq(from = 1, to = length(humid_16_22), by = 0.25)
interp_humid <- approx(xh, humid_16_22, xhout, method = "linear")
humid_15 <- as.data.frame(interp_humid[2])
humid_15 <- humid_15[-nrow(humid_15),] # deleting the last row

#Note: all data is for year 2018 in Austin 
#Read in data 
raw_data <- read.csv("15minute_data_austin.csv",header = TRUE, sep = ",", row.names=NULL, stringsAsFactors = FALSE) # data files are not provided as part of the code

#Define column names  
#-----------House IDs---------------------- 
houseid <- as.matrix(raw_data[,1]) 
#Houseids for the 25 different homes 
house_id_vector<- as.vector(unique(houseid)) 

#--------------HVAC usage----------------- 
hvac_1<- as.matrix(raw_data[,3]) 
hvac_2<- as.matrix(raw_data[,4]) 
hvac_3<- as.matrix(raw_data[,5]) 
#----------------------------------- 

#-------------Energy bought from the grid-------------- 
grid<- as.matrix(raw_data[,32]) 
#----------------------------------- 

#-------------Solar generation-------------- 
solar<- as.matrix(raw_data[,68]) 
#----------------------------------- 

#------------Date and Time----------------- 
dateandtime<- as.matrix(raw_data[,2]) 
Date_data <- format(as.POSIXct(strptime(dateandtime,"%m/%d/%Y %H:%M",tz="")) ,format = 
                      "%Y-%m-%d")

#-------------Calculate proportion of overall electricity consumption used by HVAC system--------

#Pick a house id  
i=house_id_vector[12] 

#Period of analysis (in days) 
no_of_days=7 #from July 16-July 22 

x=1 

#Initiating matrices 
percent_HVAC_1=matrix(0, nrow=96*no_of_days, ncol=1) 
hvac=matrix(0, nrow=96*no_of_days, ncol=1) 
consump=matrix(0, nrow=96*no_of_days, ncol=1)
Datetime_data_house=matrix("A", nrow=96*no_of_days, ncol=1)

for (j in 1:nrow(raw_data)){ #go through each row 
  if (houseid[j]==i & Date_data[j]>=as.Date("0018-07-16") & Date_data[j]<=as.Date("0018-07-22")) { 
    hvac[x,1] <- hvac_1[j,1] #hvac energy usage 
    consump[x,1] <-(grid[j,1]) #overall energy consumption 
    percent_HVAC_1[x,1] <- hvac_1[j,1]*100/(grid[j,1]) #percentage of HVAC usage
    Datetime_data_house[x,1] <- dateandtime[j, 1]
    x=x+1 
  } 
}

percent_HVAC_1[percent_HVAC_1<0]=0

# Create Figure 1
#Ambient temperature plot
library(ggplot2)
library(scales) 
library(reshape2) 
#----Basic plot------ 
date_vector <- seq(ISOdatetime(2018, 07, 16, 0, 0, 0), ISOdatetime(2018, 07, 22, 
                                                                   23, 59, 0), by="15 min", tz="CST") #x-axis 

df<- data.frame(date_vector, temp_15) #the x and y values for the graph in a data frame 

my_plot1<-ggplot(df, aes(x=date_vector)) + geom_line(aes(y = temp_15), color 
                                                     = "#D55E00", size=2)#Basic plot 

my_plot1 #Display the basic plot 
#--------------------------------- 

#-----------Adding axis labels------------ 
my_plot1_interm_1<- my_plot1 +labs(title="Ambient Temperature Data for Austin, TX: July 16th-22nd, 2018",x="Time of Day", y = expression(paste("Temperature (", 
                                                                                                                                               degree, "F)"))) #Adding the axis labels 

my_plot1_interm_1 #Display the plot with the axis labels 
#--------------------------------- 

#------Justifying the plot title to the center, specifying the size/color of the X and 
#Y axis titles, specifying the size of the plot title (+making it bold), adding black X 
#and Y axis lines, specifying size/color of the axis texts------------ 
my_plot1_interm_2<- my_plot1_interm_1 +theme(plot.title = element_text(hjust = 
                                                                         0.5))+theme(axis.text=element_text(size=36), axis.title=element_text(size=36))+ 
  theme(plot.title = element_text(size=36, face="bold", 
                                  color="black"))+theme(axis.line = element_line(size = 1, colour = "black", 
                                                                                 linetype=1)) +theme(axis.text=element_text(size=36)) +theme(axis.title.x = 
                                                                                                                                               element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ 
  theme(axis.text.x = element_text(color="black"), axis.text.y = 
          element_text(colour="black")) 

my_plot1_interm_2 #Display the plot  
#--------------------------------- 

#------changing the grey panel background and specifying color/linetype of the 
#major and minor grid lines 
my_plot1_interm_3<- my_plot1_interm_2 +theme(plot.background = 
                                               element_rect(fill = "white"), panel.background = element_rect(fill = "white", 
                                                                                                             colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', 
                                                                                                                                                                colour = "white"), panel.grid.minor = element_line(size = 0.5, linetype ='solid', 
                                                                                                                                                                                                                   colour = "white")) 

my_plot1_interm_3 #Display the plot  
#--------------------------------- 

#------specifying the limits of the X and Y axis, rotating the x axis texts (to make 
#the time stamps fit properly), specifying the axis ticks length (to make them more 
#visible) 

my_plot1_interm_4<- my_plot1_interm_3 +
  scale_x_datetime(breaks=date_breaks("12 hour"),labels = 
                     date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(limits=c(60, 
                                                                                              107))+ theme(axis.text.x=(element_text(angle=60, vjust=0.5)))+ 
  theme(axis.ticks.length=unit(.25, "cm")) 

my_plot1_interm_4 #Display the plot

# Add a second axis with temperature values in degrees celsius 
my_plot1_interm_5<- my_plot1_interm_4 +
  scale_x_datetime(breaks=date_breaks("12 hour"),labels = 
                     date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(limits=c(60, 
                                                                                              110), sec.axis = sec_axis(~ (.-32)* (5/9), name = expression(paste("Temperature (", degree, "C)"))))+ theme(axis.text.x=(element_text(angle=60, vjust=0.5)))+theme(axis.ticks.length=unit(.25, "cm")) 

my_plot1_interm_5 #Display the plot


#--------------------------------- 

#------Finally, we will add text annotations for each of the 7 days + add vertical 
#reference lines separating each day of the week------ 
myplottemp_final <- my_plot1_interm_5 + 
  annotate(geom="text", x=as.POSIXct("2018-07-16 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=107, label="July 16", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-17 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=107, label="July 17", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-18 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=107, label="July 18", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-19 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=107, label="July 19", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-20 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=107, label="July 20", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-21 11:45:00",format="%Y-%m-%d
%H:%M:%S"), y=107, label="July 21", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-22 11:45:00",format="%Y-%m-%d
%H:%M:%S"), y=107, label="July 22", fontface="bold", size=12)+
  geom_vline(xintercept=as.numeric (df$date_vector[c(96, 192, 288, 384, 480, 576, 672)]), 
             linetype=4, colour="black") 

myplottemp_final #Display the final plot 

#Create Figure 2
#Ambient humidity plot

#----Basic plot------ 
date_vector <- seq(ISOdatetime(2018, 07, 16, 0, 0, 0), ISOdatetime(2018, 07, 22, 
                                                                   23, 59, 0), by="15 min", tz="CST") #x-axis 

df<- data.frame(date_vector, humid_15) #the x and y values for the graph in a data frame 

my_plot1<-ggplot(df, aes(x=date_vector)) + geom_line(aes(y = humid_15), color 
                                                     = "#0000ff", size=2)#Basic plot 

my_plot1 #Display the basic plot 
#--------------------------------- 

#-----------Adding axis labels------------ 
my_plot1_interm_1<- my_plot1 + labs(title="Relative Humidity Data for Austin, TX: July 16th-22nd, 2018",x="Time of Day", y = expression(paste("Relative Humidity (%)"))) #Adding the axis labels 

my_plot1_interm_1 #Display the plot with the axis labels 
#--------------------------------- 

#------Justifying the plot title to the center, specifying the size/color of the X and 
#Y axis titles, specifying the size of the plot title (+making it bold), adding black X 
#and Y axis lines, specifying size/color of the axis texts------------ 
my_plot1_interm_2<- my_plot1_interm_1 +theme(plot.title = element_text(hjust = 
                                                                         0.5))+theme(axis.text=element_text(size=36), axis.title=element_text(size=36))+ 
  theme(plot.title = element_text(size=36, face="bold", 
                                  color="black"))+theme(axis.line = element_line(size = 1, colour = "black", 
                                                                                 linetype=1)) +theme(axis.text=element_text(size=36)) +theme(axis.title.x = 
                                                                                                                                               element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ 
  theme(axis.text.x = element_text(color="black"), axis.text.y = 
          element_text(colour="black")) 

my_plot1_interm_2 #Display the plot  
#--------------------------------- 

#------changing the grey panel background and specifying color/linetype of the 
#major and minor grid lines 
my_plot1_interm_3<- my_plot1_interm_2 +theme(plot.background = 
                                               element_rect(fill = "white"), panel.background = element_rect(fill = "white", 
                                                                                                             colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', 
                                                                                                                                                                colour = "white"), panel.grid.minor = element_line(size = 0.5, linetype ='solid', 
                                                                                                                                                                                                                   colour = "white")) 

my_plot1_interm_3 #Display the plot  
#--------------------------------- 

#------specifying the limits of the X and Y axis, rotating the x axis texts (to make 
#the time stamps fit properly), specifying the axis ticks length (to make them more 
#visible) 

my_plot1_interm_4<- my_plot1_interm_3 +
  scale_x_datetime(breaks=date_breaks("12 hour"),labels = 
                     date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(limits=c(0, 
                                                                                              106))+ theme(axis.text.x=(element_text(angle=60, vjust=0.5)))+ 
  theme(axis.ticks.length=unit(.25, "cm")) 

my_plot1_interm_4 #Display the plot

#--------------------------------- 

#------Finally, we will add text annotations for each of the 7 days + add vertical 
#reference lines separating each day of the week------ 
myplothumid_final <- my_plot1_interm_4 + 
  annotate(geom="text", x=as.POSIXct("2018-07-16 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=106, label="July 16", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-17 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=106, label="July 17", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-18 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=106, label="July 18", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-19 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=106, label="July 19", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-20 11:45:00",format="%Y-%m-%d 
%H:%M:%S"), y=106, label="July 20", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-21 11:45:00",format="%Y-%m-%d
%H:%M:%S"), y=106, label="July 21", fontface="bold", size=12)+ 
  annotate(geom="text", x=as.POSIXct("2018-07-22 11:45:00",format="%Y-%m-%d
%H:%M:%S"), y=106, label="July 22", fontface="bold", size=12)+
  geom_vline(xintercept=as.numeric (df$date_vector[c(96, 192, 288, 384, 480, 576, 672)]), 
             linetype=4, colour="black") 

myplothumid_final #Display the final plot 




# Create Figure 3

#---------install and load the CVXR package for the optimization------------ 
#install.packages("CVXR") 
library(CVXR) 

v=c(1:96)#vector of 96 time steps for the first day 
y <- percent_HVAC_1[v] #percent HVAC usage data for the first day 

lambda_1 <- 0.2 #tuning parameter 

beta  <-  Variable(length(y))  #defining  the  set  of  96  scalar  variables  that  will  be minimized  

objective_1 <- Minimize(0.5 * p_norm(y - beta) + 
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1)) 

p1 <- Problem(objective_1) 
betaHat_50 <- solve(p1)$getValue(beta) 

#Graph for the first day 
#Load required packages; you will need to install them if not already installed 
library(ggplot2) 
library(scales) 
#install.packages('reshape2') 
library(reshape2) 
#creating  the  dataframe  with  four  columns:  timedate,  percent  HVAC  usage, 
#smoothened HVAC usage, and ambient temperature.  
cvxr_data <- data.frame(x = seq(ISOdatetime(2018, 07, 16, 0, 0, 0), 
                                ISOdatetime(2018, 07, 16, 23, 59, 0), by="15 min", tz="CST"), 
                        y = percent_HVAC_1[v], 
                        l1_50 = betaHat_50,  
                        temp_data = temp_15[v], humid_15[v]) 
cvxr_data_update <-reshape2::melt(cvxr_data, id.var = "x") 

#Basic plot 
my_plot2<-ggplot(data = cvxr_data_update, aes(x=x,y=value, color=variable)) + 
  geom_line(aes(color = variable), lwd=1.5)+ 
  scale_color_manual(values = c("darkred", "darkgreen","#D55E00", "blue" ), name=NULL, 
                     labels=c("% HVAC usage", "Smoothened % HVAC usage", 
                              expression(paste('Ambient Temperature (',~degree,'F)',sep=""), expression(paste('Relative Humidity (%)',sep=""))))) 
#Display basic plot 
#my_plot1 


#Adding X and Y axis labels, plot title, plot/panel background, major/minor grid 
#lines, formatting axis lines, X- and Y-axis labels, plot title, legend, X and Y axis limits, 
#etc. 
myplot2_final  <-  my_plot2+  labs(x  =  "Time  of  Day",  y  =  "",  title="July 16") +
  theme(plot.background = element_rect(fill = "white"),panel.background = 
          element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size 
                                                                                          =  0.5,  linetype  ='solid',  colour  =  "white"),    panel.grid.minor  =  element_line(size  = 
                                                                                                                                                                                    0.25, linetype ='blank',    colour = "white"), legend.background = 
          element_rect(fill="white",size=0.5, linetype="solid", colour ="black")) +
  theme(axis.line = element_line(size = 1, colour = "black", 
                                 linetype=1))+theme(axis.text=element_text(size=36), 
                                                    axis.title=element_text(size=40))+ theme(plot.title = element_text(size=42, 
                                                                                                                       face="bold", color="black")) +theme(plot.title = element_text(hjust = 
                                                                                                                                                                                       0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = 
                                                                                                                                                                                                                date_format("%H:%M",tz="America/Chicago"))+ scale_y_continuous(expand= 
                                                                                                                                                                                                                                                                                 c(0,0), limits=c(-4, 100))+theme(axis.title.x = element_text(colour = 
                                                                                                                                                                                                                                                                                                                                                "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = 
                                                                                                                                                                                                                                                                                                                                                                                                                element_text(color="black"), axis.text.y = element_text(colour="black")) +
  theme(legend.text=element_text(color="black",size=22))+  theme(legend.title  = 
                                                                   element_text(size=22, color = "black"), legend.text = 
                                                                   element_text(size=22),legend.key=element_rect(fill='white'), 
                                                                 legend.position='none') 

#Display the final plot  

myplot2_final 


#Perform Regression
#--------- load the CVXR package for the optimization------------ 
library(CVXR) 

#--------- install and load the forecast package------------ 
#install.packages ("forecast") 
library (forecast) 
library(magrittr)
library(dplyr)

# Current Temperature (CT)
v=c(1:96)#vector of 96 time steps for the first day 
y <- percent_HVAC_1[v] #percent HVAC usage data for the first day 

lambda_1  <-  0.2  #tuning  parameter;  update  with  new  tuning  parameter  (from 
#last week’s analysis) for your sample home and the first day of your chosen period 
#of analysis 

beta  <-  Variable(length(y))  #defining  the  set  of  96  scalar  variables  that  will  be minimized 


objective_1 <- Minimize(0.5 * p_norm(y - beta) + 
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1)) 
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta) 

Arima(betaHat_50,order=c(1,0,0),xreg=cbind(temp_15[v]),include.constant=TRUE)


# Current Temperature Current Humidity (CTCH)
v=c(1:96)#vector of 96 time steps for the first day 
y <- percent_HVAC_1[v] #percent HVAC usage data for the first day 

lambda_1  <-  0.1  #tuning  parameter;  update  with  new  tuning  parameter  (from 
#last week’s analysis) for your sample home and the first day of your chosen period 
#of analysis 

beta  <-  Variable(length(y))  #defining  the  set  of  96  scalar  variables  that  will  be minimized 


objective_1 <- Minimize(0.5 * p_norm(y - beta) + 
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1)) 
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta) 

Arima(betaHat_50,order=c(1,0,0),xreg=cbind(temp_15[v], humid_15[v]),include.constant=TRUE)

# Lagged Temperature Current Humidity (LTCH)
Advert <- cbind(temp_15, lag(temp_15,1)) %>% head(NROW(temp_15))

#first day 

v=c(1:96)#vector of 96 time steps for the first day 
y <- percent_HVAC_1[v] #percent HVAC usage data for the first day 

lambda_1  <-  0.1  #tuning  parameter;  update  with  new  tuning  parameter  (from 
#last week’s analysis) for your sample home and the first day of your chosen period 
#of analysis 

beta  <-  Variable(length(y))  #defining  the  set  of  96  scalar  variables  that  will  be minimized 

objective_1 <- Minimize(0.5 * p_norm(y - beta) + 
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1)) 
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta) 

Arima(betaHat_50,order=c(1,0,0),xreg=cbind(Advert[v,1:2], humid_15[v]),include.constant=TRUE,method="ML")

# Current Temperature Lagged Humidity (CTLH)
Advert <- cbind(humid_15, lag(humid_15,1)) %>% head(NROW(temp_15))

#first day 

v=c(1:96)#vector of 96 time steps for the first day 
y <- percent_HVAC_1[v] #percent HVAC usage data for the first day 

lambda_1  <-  0.3  #tuning  parameter;  update  with  new  tuning  parameter  (from 
#last week’s analysis) for your sample home and the first day of your chosen period 
#of analysis 

beta  <-  Variable(length(y))  #defining  the  set  of  96  scalar  variables  that  will  be minimized 

objective_1 <- Minimize(0.5 * p_norm(y - beta) + 
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1)) 
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta) 

Arima(betaHat_50,order=c(1,0,0),xreg=cbind(Advert[v,1:2], temp_15[v]),include.constant=TRUE,method="ML")

# Lagged Temperature Lagged Humidity (LTLH)
Advert <- cbind(temp_15, lag(temp_15,1), humid_15, lag(humid_15,1)) %>% head(NROW(temp_15))

#first day 

v=c(1:96)#vector of 96 time steps for the first day 
y <- percent_HVAC_1[v] #percent HVAC usage data for the first day 

lambda_1  <-  0.3  #tuning  parameter;  update  with  new  tuning  parameter  (from 
#last week’s analysis) for your sample home and the first day of your chosen period 
#of analysis 

beta  <-  Variable(length(y))  #defining  the  set  of  96  scalar  variables  that  will  be minimized 

objective_1 <- Minimize(0.5 * p_norm(y - beta) + 
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1)) 
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta) 

Arima(betaHat_50,order=c(1,0,0),xreg=cbind(Advert[v,1:4]),include.constant=TRUE,method="ML")


# Create Figures 4 and 5
# This code reads data from an Excel file created with a table of AIC values for each model over the week
# Create the data frame.
setwd("/Users/banan/OneDrive/Documents/Maddy 22-23/MEEN 491H/July 2018 Analysis")
#Install the R package to read an Excel file 
#MEEN 491H Fall 2022 

install.packages("readxl") 
#Load the package 
library(readxl)
library(datasets)
library(ggplot2)
#Read in data 
JuneAIC <- read_excel("AIC.xlsx",sheet = 1, col_names = TRUE) # data files are not provided as part of the code





# Use single color
ggplot(JuneAIC, aes(x=ModelType, y=AICValues)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()
# Change box plot colors by groups
p<-ggplot(JuneAIC, aes(x=ModelType, y=AICValues, fill=ModelType)) + 
  geom_boxplot() + labs(title="Sample Home (July 16-22, 2018)",x="Model Type", y = "AIC Value")
p<- p + guides(fill=guide_legend(title="Model Type")) + theme(plot.title = element_text(hjust = 0.5)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                              panel.background = element_blank(), axis.line = element_line(colour = "black"))
p <- p + scale_x_discrete(limits=c("LTLH", "CTLH", "LTCH", "CTCH", "CT"))
p <- p + scale_fill_discrete(breaks=c("CT",	"CTCH",	"LTCH",	"CTLH",	"LTLH"))
p <- p + theme(axis.text=element_text(size=36), axis.title=element_text(size=36)) + 
  theme(plot.title = element_text(size=46, face="bold", 
                                  color="black"))+theme(axis.line = element_line(size = 1, colour = "black", 
                                                                                 linetype=1)) +theme(axis.text=element_text(size=34)) +theme(axis.title.x = 
                                                                                                                                               element_text(colour = "black", size = 42),axis.title.y = element_text(color = "black", size = 42))+ 
  theme(axis.text.x = element_text(color="black"), axis.text.y = 
          element_text(colour="black")) + coord_flip() + theme(legend.text=element_text(size=24), legend.title=element_text(size=24), legend.key.size=unit(2, "cm"), legend.position="none")
p <- p + scale_fill_brewer(palette="Set1")
p



# Create Figure 6

# Current Temperature (CT)
first_day_reg_coef <- 1.842
first_day_intercept <- -102.3603
# creating in-sample fitted curves
percent_HVAC_1_firstdayLinT=first_day_reg_coef*temp_15[1:96]+first_day_intercept

# Current Temperature Current Humidity (CTCH)
first_day_treg_coef <- 1.2913
first_day_hreg_coef <- -0.2091
first_day_intercept <- -42.5347  
# creating in-sample fitted curves
percent_HVAC_1_firstdayLinTH=first_day_treg_coef*temp_15[1:96]+first_day_hreg_coef*humid_15[1:96]+first_day_intercept 

# Lagged Temperature Current Humidity (LTCH)
library(magrittr)
library(dplyr)
Advert <- cbind(temp_15, lag(temp_15,1)) %>% head(NROW(temp_15))
# initializing coefficient values 
first_day_treg_coef <- -0.1392
first_day_tlagreg_coef <- 1.7013
first_day_hreg_coef <- -0.1244
first_day_intercept <- -71.5396   
# creating in-sample fitted curves
percent_HVAC_1_firstdayLagT=first_day_treg_coef*Advert[1:96,1]+first_day_tlagreg_coef*Advert[1:96,2]+humid_15[1:96]*first_day_hreg_coef+first_day_intercept

# Current Temperature Lagged Humidity (CTLH)
Advert <- cbind(humid_15, lag(humid_15,1)) %>% head(NROW(temp_15))
first_day_hreg_coef <- 0.1968 
first_day_hlagreg_coef <- -0.4506
first_day_treg_coef <- 1.2062
first_day_intercept <- -33.4662  
# creating in-sample fitted curves
percent_HVAC_1_firstdayLagH=first_day_hreg_coef*Advert[1:96,1]+first_day_hlagreg_coef*Advert[1:96,2]+temp_15[1:96]*first_day_treg_coef+first_day_intercept

# Lagged Temperature Lagged Humidity (LTLH)
Advert <- cbind(temp_15, lag(temp_15,1), humid_15, lag(humid_15,1)) %>% head(NROW(temp_15))
first_day_treg_coef <- 0.2212
first_day_tlagreg_coef <- 1.2095
first_day_hreg_coef <- 0.0608 
first_day_hlagreg_coef <- -0.2373
first_day_intercept <- -57.2851 
# creating in-sample fitted curves
percent_HVAC_1_firstdayLagTH=first_day_treg_coef*Advert[1:96,1]+first_day_tlagreg_coef*Advert[1:96,2]+first_day_hreg_coef*Advert[1:96,3]+first_day_hlagreg_coef*Advert[1:96,4]+first_day_intercept

#Average all three lagged models and compare to current temperature
percent_HVAC_1_firstdayAvg = (percent_HVAC_1_firstdayLagTH+percent_HVAC_1_firstdayLagH+percent_HVAC_1_firstdayLagT)/3

# Create Figure

#Graph for the first day 
#Load required packages; you will need to install them if not already installed 
library(ggplot2) 
library(scales) 
library(reshape2) 
#creating  the  dataframe  with  three  columns:  timedate,  smoothened  HVAC 
#usage, and the in-sample fitted percent HVAC usage.  
v=c(1:96)#vector of 96 time steps for the first day 
y <- percent_HVAC_1[v] #percent HVAC usage data for the first day 

lambda_1  <-  0.2  #tuning  parameter;  update  with  new  tuning  parameter  (from 
#last week’s analysis) for your sample home and the first day of your chosen period 
#of analysis 

beta  <-  Variable(length(y))  #defining  the  set  of  96  scalar  variables  that  will  be minimized 


objective_1 <- Minimize(0.5 * p_norm(y - beta) + 
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1)) 
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta) 
df <- data.frame(x = seq(ISOdatetime(2018, 07, 16, 0, 0, 0), ISOdatetime(2018, 07, 
                                                                         16, 23, 59, 0), by="15 min", tz="CST"), 
                 l1_50 = betaHat_50,  
                 fitted_data = percent_HVAC_1_firstdayLinT, percent_HVAC_1_firstdayAvg) 
df_update <-reshape2::melt(df, id.var = "x") 

#Basic plot 
my_plot1<-ggplot(data = df_update, aes(x=x,y=value, color=variable, linetype = variable)) + 
  geom_line(aes(color = variable, linetype = variable), lwd=1.5) + theme(legend.position="none", legend.text = element_text(size=18), legend.key.size = unit(1, 'cm'), legend.key=element_rect(fill='white')) +
  scale_color_manual(values = c ("red", "blue", "darkgreen"), name=NULL,  
                     labels=c("Smoothened % HVAC usage", "CT", "AVG")) +
  scale_linetype_manual(values = c("solid", "solid", "solid"), name=NULL,  
                        labels=c("Smoothened % HVAC usage", "CT", "AVG"))
#Display basic plot 
# my_plot1

#Display basic plot 
# my_plot1 


#Adding X and Y axis labels, plot title, plot/panel background, major/minor grid 
#lines, formatting axis lines, X- and Y-axis labels, plot title, legend, X and Y axis limits, 
#etc. 
myplot1_final  <-  my_plot1+  labs(x  =  "Time  of  Day",  y  =  "Percent HVAC Usage",  title="Sample Home, Model Fits, July 16")+ 
  theme(plot.background = element_rect(fill = "white"),panel.background = 
          element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size 
                                                                                          =  0.5,  linetype  ='solid',  colour  =  "white"),    panel.grid.minor  =  element_line(size  = 
                                                                                                                                                                                    0.25, linetype ='blank',    colour = "white"), legend.background = 
          element_rect(fill="white",size=0.5, linetype="solid", colour ="black"))+ 
  theme(axis.line = element_line(size = 1, colour = "black", 
                                 linetype=1))+theme(axis.text=element_text(size=36), 
                                                    axis.title=element_text(size=40))+ theme(plot.title = element_text(size=42, 
                                                                                                                       face="bold", color="black")) +theme(plot.title = element_text(hjust = 
                                                                                                                                                                                       0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = 
                                                                                                                                                                                                                date_format("%H:%M",tz="America/Chicago"))+ scale_y_continuous(expand= 
                                                                                                                                                                                                                                                                                 c(0,0), limits=c(20, 80))+theme(axis.title.x = element_text(colour = 
                                                                                                                                                                                                                                                                                                                                               "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = 
                                                                                                                                                                                                                                                                                                                                                                                                               element_text(color="black"), axis.text.y = element_text(colour="black"))

#Display the final plot  
myplot1_final 