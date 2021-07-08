#Loading packages
library(tseries)
library("tsfknn")#knn 
library("imputeTS") #time series imputation
library("tsfgrnn")# grnn
library(forecast)
library(grid) # visualizations
library(ggplot2)
library(gridExtra)
library("nnfor")# time series with neural networks
library(timetk)
library(tidyverse)
library(lubridate)
library("tsoutliers")
library("nonlinearTseries") #To time for time reversility of a series
library("seastests") #seasonaity test
library(zoo)
#It implements a multilayer RNN, GRU, and LSTM directly in R
library("rnn")
library("keras")
library(tensorflow)
##perform Dunn's Test with Bonferroni correction for p-values
library(FSA)
library(nortsTest)
library("car")

options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 300) #Setting plot size

#setting a working directory
setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Tema Hospital Blood data and R codes")

# Importing the data from Tema Blood Bank
Blood_data<-read.csv(file="Blood_data_Tema.csv")
head(Blood_data)

# Declaring time series data
Demand_timeseries<- ts(Blood_data$QTY_DEMANDED, start=c(2013,1), end=c(2020, 9), frequency=12)

Demand_timeseries

paste("Percentage of missing data in the demand series=",
      round((length(which(is.na(Demand_timeseries)==TRUE))/length(Demand_timeseries))*100,2),"%")


statsNA(Demand_timeseries)

# Impute the missing values with na_kalman
# (tsAirgap is an example time series provided by the imputeTS package)
#"StructTS" - For using a structural model fitted by maximum likelihood (using StructTS)
#"auto.arima" - For using the state space representation of arima model (using auto.arima)
#nit: Parameter from Kalman Filtering (see KalmanLike). Usually no need to change from default.
#model = "StructTS" or auto.arima
Imputed_demandSeries_byKalman<- na_kalman(Demand_timeseries, model = "StructTS", smooth = TRUE, nit = -1)
Imputed_demandSeries_byKalman


dates<- as.numeric(time(Demand_timeseries))
## 'POSIXct, POSIXt' object
tms <- date_decimal(dates)
p_demand<-ggplot_na_distribution(Demand_timeseries,x_axis_labels=tms,color_missing = "white",color_missing_border ="yellow",
    theme = NULL)+
theme(panel.grid.major = element_blank(),plot.title=element_blank())+
ylab("Blood units demanded") 


# Visualizing the imputed data
#Code for visualization

###To change the time axis
#dates<- as.yearmon(time(Demand_timeseries))
dates<- as.numeric(time(Demand_timeseries))
## 'POSIXct, POSIXt' object
tms <- date_decimal(dates)


#Kalman Smoothing via a basic structural model (BSM) for a time series by maximum likelihood 
p_demand_imputed<-ggplot_na_imputations(Demand_timeseries,Imputed_demandSeries_byKalman,
    subtitle = "",x_axis_labels=tms,theme = NULL)+
theme(panel.grid.major = element_blank())+
xlab("Time") +ylab("Blood units demanded")+ ggtitle("Imputed series using Kalman Smoothing on BSM")


grid.arrange(p_demand, p_demand_imputed,nrow = 2,ncol = 1)

# Importing external scripts
#for plotting series with outliers
source("plot-tsoutliers-script.r")


#Method 1 of Stage 2
#Automatic Procedure for Detection of Outliers
#Imputed data via Kalman Smoothing on automatic ARIMA model (1,1,1)
outlier.demand<-tsoutliers::tso(Imputed_demandSeries_byKalman,types = c("AO","LS","TC"),maxit.iloop=10,tsmethod = "auto.arima")
outlier.demand

#Corrected data based on outlier detection via residuals of fitted ARIMA(1,1,1)
demand_corrected_data<- outlier.demand$yadj
demand_corrected_data

plot.tsoutliers1(outlier.demand,title_name="",
                xlabel="Time", ylabel="Blood units demanded",xaxt=NULL,lwd=3)


text<-c("No outlier", "Outlier")
legend(x=2014,y=1000,legend = text,col=c("blue","red"),
      cex=1, horiz = FALSE,pt.cex = 1,fill=c("blue","red"),ncol=1)

acf_plot<- ggAcf(demand_corrected_data,main="ACF plot")


splot<- ggseasonplot(demand_corrected_data, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Blood demand") +
  ggtitle("Seasonal plot")

grid.arrange(acf_plot,splot,nrow=2)

#The p-values indicate the p-values of the underlying test, i.e. the QS-test, the QS-R test and the KW-R-test.
#Seasonality test for data corrected by KS-BSM correction
summary(wo(demand_corrected_data,freq=12))

options(warn=-1)
adf.test(demand_corrected_data,k=0)
#Note: in fact, p.value = 0.01 means p.value <= 0.01 

#re-naming the series for the corrected data
demand<-demand_corrected_data

lobato.test(demand)

#For demand_STLKS series
yt=demand #the series
TimeRevStats<- sum(yt[1:(length(yt)-1)]*yt[2:length(yt)]^2-yt[1:(length(yt)-1)]^2*yt[2:length(yt)])/(length(yt)-1)

TimeRevStats

# Time Reversibility statistic
timeAsymmetry(demand)

st_demand <- surrogateTest(demand,significance = 0.05,one.sided = F,K=5,
                   FUN = timeAsymmetry, do.plot=F)

st_demand$data.statistic

#st$surrogates.statistics

par(mar=c(4,4,2,3),mfrow=c(2,1))

hist(st_demand$surrogates.statistics,col="blue",xlab="time-reversibility statistic values ",ylim=c(0,70),
     main="",
     xlim=c(min(st_demand$surrogates.statistics),-min(st_demand$surrogates.statistics)))
abline(v=st_demand$data.statistic,col="red",lwd=3,lty=1)
text(st_demand$data.statistic-1e5,65,paste("test statistic=",round(st_demand$data.statistic,2)),col="red")


text <- c("Surrogate series","Original series")
legend_order <- matrix(1:2,ncol=1,byrow = T)
legend(x = st_demand$data.statistic+40000,y=60,legend = text[legend_order],
       col=c("blue","red"),ncol=1, cex=1.1,title="",bty="n",
             fill=c("blue","red"),text.width = strwidth(text)[1]*1.80)

#Normality plot
qqPlot(demand,main="Normal Q-Q plot",ylab="Sample quantiles",xlab="Normal quantiles",ylim=c(0,600),id=F)

nonlinearityTest(demand,verbose=T)$Terasvirta

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021/R Scripts")

#script for ARIMA backcasting
source("reverse-ARIMA-forecast-script.r")

#Script of functions for rolling-origin evaluation for ARIMA model (forecasts & backcasts) 
source("rolling-eval-ARIMA-script.r")

# 18-month forecast based on STL with KS-BSM corrected data
options(warn=-1)
ARIMA_results_forecasts<- ro_eval_ARIMA_forecast(series=demand,forecast_length=18)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")
# 18-month rolling-origin evaluation with update and recalibration 

ARIMA_results_forecasts$ARIMA_errors
write.csv(ARIMA_results_forecasts$ARIMA_errors,"ARIMA_results_forecasts_ARIMA_errors.csv")

x=seq(18,2)
sum(x)


(18*(18+1)/2) -1

# 18-month rolling-origin evaluation with update and recalibration 
ARIMA_results_forecasts$ARIMA_model[[1]]

# 18-month backcasts based on STL with KS-BSM corrected data
ARIMA_results_backcasts<- ro_eval_ARIMA_backcast(series=demand,backcast_length=18)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

# 18-month rolling-origin evaluation with update and recalibration 
ARIMA_results_backcasts$ARIMA_errors
write.csv(ARIMA_results_backcasts$ARIMA_errors,"ARIMA_results_backcasts_ARIMA_errors.csv")

# 12-month rolling-origin evaluation with update and recalibration 
#based on STL with KS-BSM corrected data (best)
ARIMA_results_backcasts$ARIMA_model[[1]]

#ARIMA_results_backcasts$ARIMA_backcast[[1]]

p1=autoplot(ARIMA_results_forecasts$ARIMA_forecast[[1]])+
ggtitle("ARIMA model 18-month forecasts")+labs(y="Blood units")+
    scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018, 2019,2020),
        labels=c("2013","2014","2015","2016","2017","2018","2019","2020"))+ theme_bw()+ 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())
 

p3=autoplot(ARIMA_results_backcasts$ARIMA_backcast[[1]])+
ggtitle("ARIMA model 18-month baskcasts")+labs(y="Blood units")+
    scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018, 2019,2020),
        labels=c("2013","2014","2015","2016","2017","2018","2019","2020"))+ theme_bw()+ 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank()) 


grid.arrange(p1,p3,nrow=2,ncol=1)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021/R Scripts")

#Script of the function to choose k and the lag value which results in minimum RMSE
#or better forecasts & backcasts
source("KNN-k-lag-best-script.r")

#Script of the function for rolling-origin forecast evaluations for the KNN model
source("ro-eval-KNN-script.r")

# 18-month forecast
KNN_results_forecasts<- ro_eval_KNN_forecast(series=demand,forecast_length=18)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

KNN_results_forecasts$KNN_errors
write.csv(KNN_results_forecasts$KNN_errors,"KNN_results_forecasts_KNN_errors.csv")

# 18-month backcast
KNN_results_backcasts<- ro_eval_KNN_backcast(series=demand,backcast_length=18)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

KNN_results_backcasts$KNN_errors
write.csv(KNN_results_backcasts$KNN_errors,"KNN_results_backcasts_KNN_errors.csv")

KNN1<-autoplot(KNN_results_forecasts$KNN_Model[[1]], highlight = "neighbors", faceting =FALSE)+
ggtitle("KNN 18-month forecasts")+ theme_bw()+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(),legend.position=c(0.2, .8))+labs(y="Blood units")+
theme(plot.title = element_text(hjust=0), axis.text.x = element_text(size = 12)
        ,axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=12), 
    legend.text=element_text(size=12))+ scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018, 2019,2020),
        labels=c("2013","2014","2015","2016","2017","2018","2019","2020"))

KNN1+ theme(plot.title = element_text(size=15))

KNN3=autoplot(KNN_results_backcasts$KNN_Model[[1]], highlight = "neighbors", faceting =F)+
ggtitle("KNN 18-month backcasts")+theme_bw()+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(),legend.position=c(0.8, .8))+labs(y="Blood units")+ theme(plot.title = element_text(hjust=0), axis.text.x = element_text(size = 12)
        ,axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13))+
theme(plot.title = element_text(size = 12, face = "bold"),
    legend.title=element_text(size=12), 
    legend.text=element_text(size=12))+ scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018, 2019,2020),
        labels=c("2013","2014","2015","2016","2017","2018","2019","2020"))

KNN3+ theme(plot.title = element_text(size=15))

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021/R Scripts")

#script of functions for choosing the non-seasonal lag (p) and the average number of networks
#for the NNAR model forecasts & backcasting based on mimimum MAPE
source("NNAR-tuning-script.r")

#script of functions for rolling-origin forecast & backcast evaluations for the NNAR model
source("ro-eval-NNAR-script.r")

# 18-month forecast
NNAR_results_forecasts<- ro_eval_NNAR_forecast(series=demand,forecast_length=18,seed.number=1)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

NNAR_results_forecasts$NNAR_errors
write.csv(NNAR_results_forecasts$NNAR_errors,"NNAR_results_forecasts_NNAR_errors.csv")

# 18-month backcast
NNAR_results_backcasts<- ro_eval_NNAR_backcast(series=demand,backcast_length=18,seed.number=1)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

NNAR_results_backcasts$NNAR_errors
write.csv(NNAR_results_backcasts$NNAR_errors,"NNAR_results_backcasts_NNAR_errors.csv")

par(mar=c(4,4,2,2))
plot(NNAR_results_forecasts$NNAR_forecasts[[1]],main="")
mtext(text = "Blood units",
      side = 2, #side 2 = left
      line = 2,cex=1.2)

mtext(text = "Time",
      side = 1, #side 2 = left
      line = 3,cex=1.2)


mtext(text = "NNAR 18-month forecasts",
      side = 3, #side 2 = left
      line = 0.5,cex=1.2,font=2,adj =0)

par(mar=c(4,4,2,2))
plot(NNAR_results_backcasts$NNAR_backcasts[[1]],main="",xlim=c(2013,2021))
mtext(text = "Blood units",
      side = 2, #side 2 = left
      line = 2,cex=1.2)

mtext(text = "Time",
      side = 1, #side 2 = left
      line = 3,cex=1.2)

mtext(text = "NNAR 18-month backcasts",
      side = 3, #side 2 = left
      line = 0.5,cex=1.2,font=2,adj =0)

#setting a working directory
setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021/R Scripts")

#script of functions for choosing the lags used as autoregressive variables
#for the GRNN model forecasts & backcasting based on mimimum MAPE
#The smooting parameter is automatically chosen via optimisation
source("GRNN-lag-best-script.r")

#Script of the function for rolling-origin forecast evaluations for the GRNN model
source("ro-eval-GRNN-script.r")

#Script of function for GRNN autoplots (backcast and forecast)
source("autoplot-GRNN-script.r")

# 18-month forecast
GRNN_results_forecasts<- ro_eval_GRNN_forecast(series=demand,forecast_length=18)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

GRNN_results_forecasts$GRNN_errors
write.csv(GRNN_results_forecasts$GRNN_errors,"GRNN_results_forecasts_GRNN_errors.csv")

# 18-month backcast 
GRNN_results_backcasts<- ro_eval_GRNN_backcast(series=demand,backcast_length=18)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

GRNN_results_backcasts$GRNN_errors
write.csv(GRNN_results_backcasts$GRNN_errors,"GRNN_results_backcasts_GRNN_errors.csv")

G1<-autoplot.grnnForecast(GRNN_results_forecasts$GRNN_Model[[1]],highlight = c("points"))+
ggtitle("GRNN 18-month forecasts")+ theme_bw()+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+labs(y="Blood units")+ 
theme(plot.title = element_text(hjust=0), axis.text.x = element_text(size = 12)
        ,axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13))


G1+ scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018, 2019,2020),
       labels=c("2013","2014","2015","2016","2017","2018","2019","2020"))+theme(legend.position = c(0.8, 0.9),
                                        legend.title=element_text(size=13),legend.text=element_text(size=12))+
 theme(plot.title = element_text(size=15))+theme(plot.title = element_text(face = "bold"))

G3<-autoplot.grnnBackcast(GRNN_results_backcasts$GRNN_Model[[1]],highlight = c("points"))+
ggtitle("GRNN 18-month backcasts")+ theme_bw()+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+labs(y="Blood units")+ 
theme(plot.title = element_text(hjust=0), axis.text.x = element_text(size = 12)
        ,axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13))


G3+ scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018, 2019,2020),
       labels=c("2013","2014","2015","2016","2017","2018","2019","2020"))+theme(legend.position = c(0.8, 0.9),
                                        legend.title=element_text(size=13),legend.text=element_text(size=12))+
theme(plot.title = element_text(size=15))+theme(plot.title = element_text(face = "bold"))


 

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021/R Scripts")

#Script of function for MLP plots (backcast and forecast)

source("plot-MLP-script.r")

##script of functions for rolling-origin forecast & backcast evaluations for the MLP model
# sel.lag=TRUE automatically select lags
#difforder=NULL automatically determines the differencing order
#hd.auto.type="cv" automatically selects the number of hidden nodes using 5-fold cv (hd) if hd=NULL
source("ro-eval-MLP-script.r")

# 18-month forecast 
MLP_results_forecasts<- ro_eval_MLP_forecast(series=demand,forecast_length=18,seed.number=NULL)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

MLP_results_forecasts$MLP_errors
write.csv(MLP_results_forecasts$MLP_errors,"MLP_results_forecasts_MLP_errors.csv")

# 18-month backcast 
MLP_results_backcasts<- ro_eval_MLP_backcast(series=demand,backcast_length=18,seed.number=1)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

MLP_results_backcasts$MLP_errors
write.csv(MLP_results_backcasts$MLP_errors,"MLP_results_backcasts_MLP_errors.csv")

par(mar=c(4,4,2,2))
plot.MLP_forecast(MLP_results_forecasts$MLP_forecasts[[1]],
                  main_title="",
                xaxt = "n",main="",cex=1.2,xlab="")

mtext(text = "Blood units",
      side = 2, #side 2 = left
      line = 2,cex=1.2)


mtext(text = "MLP 18-month forecasts",
      side = 3, #side 2 = left
      line = 0.5,cex=1.2,font=2,adj =0)

 par(mar=c(4,4,2,2))
plot.MLP_reverse(MLP_results_backcasts$MLP_backcasts[[1]],
                 main_title="",
                xaxt = "n",main="",cex=1.2,xlab="")

mtext(text = "Blood units",
      side = 2, #side 2 = left
      line = 2,cex=1.2)


mtext(text = "MLP 18-month backcasts",
      side = 3, #side 2 = left
      line = 0.5,cex=1.2,font=2,adj =0)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021/R Scripts")

#Script of function for ELM plots (backcast and forecast)
source("plot-ELM-script.r")

##script of functions for rolling-origin forecast & backcast evaluations for the ELM model
source("ro-eval-ELM-script.r")

# 18-month forecast
ELM_results_forecasts<- ro_eval_ELM_forecast(series=demand,forecast_length=18,seed.number=NULL)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

ELM_results_forecasts$ELM_errors
write.csv(ELM_results_forecasts$ELM_errors,"ELM_results_forecasts_ELM_errors.csv")

# 18-month backcast 
ELM_results_backcasts<- ro_eval_ELM_backcast(series=demand,backcast_length=18,seed.number=1)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

ELM_results_backcasts$ELM_errors
write.csv(ELM_results_backcasts$ELM_errors,"ELM_results_backcasts_ELM_errors.csv")

par(mar=c(4,4,2,2))
plot.ELM_forecast(ELM_results_forecasts$ELM_forecasts[[1]],
                  main_title=" ",
                xaxt = "n",main="",cex=1.2,xlab="")

mtext(text = "Blood units",
      side = 2, #side 2 = left
      line = 2,cex=1.2)

mtext(text = "ELM 18-month forecasts",
      side = 3, #side 2 = left
      line = 0.5,cex=1.2,font=2,adj =0)

par(mar=c(4,4,2,2))
plot.ELM_reverse(ELM_results_backcasts$ELM_backcasts[[1]],
                 main_title="",
                xaxt = "n",main="",cex=1.2,xlab="")

mtext(text = "Blood units",
      side = 2, #side 2 = left
      line = 2,cex=1.2)

mtext(text = "MLP 18-month backcasts",
      side = 3, #side 2 = left
      line = 0.5,cex=1.2,font=2,adj =0)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021/R Scripts")

#Script of functions for obtaining target variable Y and predictor X
#Normalising data and invert scaling
source("Lag-normalise-invert-scaling-script.r")

##script of functions for rolling-origin forecast & backcast evaluations for the LSTM model
source("rolling-eval-LSTM-script.r")

# 18-month forecast 
LSTM_results_forecasts<-ro_eval_LSTM_forecast(series=demand,forecast_length=18,
                                            lag=1,units = 1,Epochs=50,batch_size = 1)



setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

LSTM_results_forecasts$LSTM_errors

write.csv(LSTM_results_forecasts$LSTM_errors,"LSTM_results_forecasts_LSTM_errors.csv")

# 18-month backcast
LSTM_results_backcasts<-ro_eval_LSTM_backcast(series=demand,backcast_length=18,
                                            lag=1,units = 1,Epochs=50,batch_size = 1)

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")

LSTM_results_backcasts$LSTM_errors

write.csv(LSTM_results_backcasts$LSTM_errors,"LSTM_results_backcasts_LSTM_errors.csv")

par(mar=c(4,4,2,2))
plot(LSTM_results_forecasts$LSTM_forecasts[[1]],
     main="",xlim=c(2013,2021))
mtext(text = "Blood units",
      side = 2, #side 2 = left
      line = 2,cex=1.2)

mtext(text = "Time",
      side = 1, #side 2 = left
      line = 3,cex=1.2)

mtext(text = "LSTM 18-month forecasts",
      side = 3, #side 2 = left
      line = 0.5,cex=1.2,font=2,adj =0)

par(mar=c(4,4,2,2))
plot(LSTM_results_backcasts$LSTM_backcasts[[1]],
     main="",xlim=c(2013,2021))
mtext(text = "Blood units",
      side = 2, #side 2 = left
      line = 2,cex=1.2)

mtext(text = "Time",
      side = 1, #side 2 = left
      line = 3,cex=1.2)


mtext(text = "LSTM 18-month backcasts",
      side = 3, #side 2 = left
      line = 0.5,cex=1.2,font=2,adj =0)

rev(rownames(ARIMA_results_backcasts$ARIMA_errors))

rownames(ARIMA_results_forecasts$ARIMA_errors)

# Comparing the forecast errors
par(mar=c(4,4,1,1),mfrow=c(2,1))
plot(ARIMA_results_forecasts$ARIMA_errors[,3],type="b",col="red",lwd=3,
     ylim=c(0,60),xaxt="n",xlab="Forecast origins",ylab="MAPE (%)",main="")
lines(KNN_results_forecasts$KNN_errors[,3],type="b",col="blue",lwd=3)
lines(NNAR_results_forecasts$NNAR_errors[,3],type="b",col="green",lwd=3)
lines(GRNN_results_forecasts$GRNN_errors[,3],type="b",col="yellow",lwd=3)
lines(MLP_results_forecasts$MLP_errors[,3],type="b",col="magenta",lwd=3)
lines(ELM_results_forecasts$ELM_errors[,3],type="b",col="grey",lwd=3)
lines(LSTM_results_forecasts$LSTM_errors[,3],type="b",col="black",lwd=3)

 axis(1, at=1:17, labels=rownames(ARIMA_results_forecasts$ARIMA_errors))

mtext(text = "Rolling-origin forecast evaluation",
      side = 3, #side 2 = left
      line = -1,cex=1,font=2,adj =0.01)

text <- c("ARIMA","KNN","NNAR","GRNN","MLP","ELM","LSTM")
colour<- c("red","blue","green","yellow","magenta","grey","black")    
    
legend(x=0.5,y=67,legend = text,col=colour,box.lwd = 2,fill=colour,bty="n",
      ,text.width = strwidth(text)[1]*0.7, cex=1,title="",horiz = T)




plot(rev(ARIMA_results_backcasts$ARIMA_errors[,3]),type="b",col="red",lwd=3,
     ylim=c(0,70),xaxt="n",xlab="Backcast origins",ylab="MAPE (%)",main="")
lines(rev(KNN_results_backcasts$KNN_errors[,3]),type="b",col="blue",lwd=3)
lines(rev(NNAR_results_backcasts$NNAR_errors[,3]),type="b",col="green",lwd=3)
lines(rev(GRNN_results_backcasts$GRNN_errors[,3]),type="b",col="yellow",lwd=3)
lines(rev(MLP_results_backcasts$MLP_errors[,3]),type="b",col="magenta",lwd=3)
lines(rev(ELM_results_backcasts$ELM_errors[,3]),type="b",col="grey",lwd=3)
lines(rev(LSTM_results_backcasts$LSTM_errors[,3]),type="b",col="black",lwd=3)

 axis(1, at=1:17, labels=rev(rownames(ARIMA_results_backcasts$ARIMA_errors)))
mtext(text = "Rolling-origin backcast evaluation",
      side = 3, #side 2 = left
      line = -1,cex=1,font=2,adj =0.01)


text <- c("ARIMA","KNN","NNAR","GRNN","MLP","ELM","LSTM")
colour<- c("red","blue","green","yellow","magenta","grey","black")  
#legend(par("usr")[1],par("usr")[3],text,col=c("green","black","red"),xjust=0, yjust=1.4,bty = "n",lwd=3,ncol=1)
#mtext("Population",side=3,col="black",line=1.5,cex=.8)  
    
legend(x=0.5,y=78,legend = text,col=colour,box.lwd = 2,fill=colour,bty="n",
      ,text.width = strwidth(text)[1]*0.7, cex=1,title="",horiz = T)



#Function to compute the median MAPE across the time series models
Median_MAPE<-function(MAPE_lead_times,Methods){
    median_MAPE<- apply(MAPE_lead_times,2,median)# median MAPE
    return(data.frame(median_MAPE= median_MAPE,Models=Methods))
}

setwd("C:/Users/user/Desktop/PhD Documents Cardiff_main/PhD Project Chapter 1-6/LATEX PhD Thesis 1/LaTex Files _PhD/Juliet Publication work_Main/Current-Resubmission_IJF_2021")


MAPEs_forecasts<-data.frame(ARIMA=ARIMA_results_forecasts$ARIMA_errors[,3],
    KNN=KNN_results_forecasts$KNN_errors[,3],NNAR=NNAR_results_forecasts$NNAR_errors[,3],
    GRNN=GRNN_results_forecasts$GRNN_errors[,3],MLP=MLP_results_forecasts$MLP_errors[,3],
                    ELM=ELM_results_forecasts$ELM_errors[,3],
                LSTM=LSTM_results_forecasts$LSTM_errors[,3])

MAPEs_forecasts$Lead_times<-rownames(ARIMA_results_forecasts$ARIMA_errors)
MAPEs_forecasts

Pooled_MAPE_forecast<-Median_MAPE(MAPE_lead_times=MAPEs_forecasts[,1:7],
                                   Methods=c("ARIMA","KNN","NNAR","GRNN","MLP","ELM","LSTM"))

Pooled_MAPE_forecast
write.csv(Pooled_MAPE_forecast,"Pooled_MAPE_forecast.csv")

MAPEs_backcasts<-data.frame(ARIMA=rev(ARIMA_results_backcasts$ARIMA_errors[,3]),
    KNN=rev(KNN_results_backcasts$KNN_errors[,3]),NNAR=rev(NNAR_results_backcasts$NNAR_errors[,3]),
    GRNN=rev(GRNN_results_backcasts$GRNN_errors[,3]),MLP=rev(MLP_results_backcasts$MLP_errors[,3]),
    ELM=rev(ELM_results_backcasts$ELM_errors[,3]),LSTM=rev(LSTM_results_backcasts$LSTM_errors[,3]))

MAPEs_backcasts$Lead_times<-rev(rownames(ARIMA_results_backcasts$ARIMA_errors))
MAPEs_backcasts

Pooled_MAPE_backcast<-Median_MAPE(MAPE_lead_times=MAPEs_backcasts[,1:7],
                                   Methods=c("ARIMA","KNN","NNAR","GRNN","MLP","ELM","LSTM"))

Pooled_MAPE_backcast
write.csv(Pooled_MAPE_backcast,"Pooled_MAPE_backcast.csv")

par(mfrow=c(2,1),mar=c(4,4,1,1))
boxplot(MAPEs_forecasts[,1],MAPEs_forecasts[,2],MAPEs_forecasts[,3],
MAPEs_forecasts[,4],MAPEs_forecasts[,5],MAPEs_forecasts[,6],MAPEs_forecasts[,7],
main = "",
at = 1:7,
ylim=c(0,60),
ylab="MAPE (%)",
names = c("ARIMA","KNN","NNAR","GRNN","MLP","ELM","LSTM"),
las = 1,
col = c("red","blue","green","yellow","magenta","grey","brown"),
border = "black",
horizontal = F,
notch = F
)

mtext(text = "Forecast error distribution",
      side = 3, #side 2 = left
      line = 0,cex=1.2,font=2,adj =0.01)


boxplot(MAPEs_backcasts[,1],MAPEs_backcasts[,2],MAPEs_backcasts[,3],
MAPEs_backcasts[,4],MAPEs_backcasts[,5],MAPEs_backcasts[,6],MAPEs_backcasts[,7],
main = "",
at = 1:7,
ylab="MAPE (%)",
names = c("ARIMA","KNN","NNAR","GRNN","MLP","ELM","LSTM"),
las = 1,
col = c("red","blue","green","yellow","magenta","grey","brown"),
border = "black",
horizontal = F,
notch = F,
xlab="Time series models"
)


mtext(text = "Backcast error distribution",
      side = 3, #side 2 = left
      line = 0,cex=1.2,font=2,adj =0.01)

n<-dim(MAPEs_forecasts)[1]

MAPE_forecasts<- c(MAPEs_forecasts[,1],MAPEs_forecasts[,2],MAPEs_forecasts[,3],
MAPEs_forecasts[,4],MAPEs_forecasts[,5],MAPEs_forecasts[,6],MAPEs_forecasts[,7])

MAPE_backcasts<- c(MAPEs_backcasts[,1],MAPEs_backcasts[,2],MAPEs_backcasts[,3],
MAPEs_backcasts[,4],MAPEs_backcasts[,5],MAPEs_backcasts[,6],MAPEs_backcasts[,7])

Models<- c(rep("ARIMA",n),rep("KNN",n),rep("NNAR",n),rep("GRNN",n),rep("MLP",n),rep("ELM",n),rep("LSTM",n))

Error_data<- data.frame(Error_forecast=MAPE_forecasts,Error_backcast=MAPE_backcasts,Methods=Models)
dim(Error_data)
write.csv(Error_data,"Error_data.csv")
head(Error_data,n=10)

#For comparing significant difference in forecast errors between methods
kruskal.test(Error_forecast ~ Methods, data = Error_data)

#For comparing significant difference in backcast errors between methods
kruskal.test(Error_backcast ~ Methods, data = Error_data)

#p.adjustment.methods
# c("none", "bonferroni", "sidak", "holm", "hs", "hochberg", "bh", "by")
pairwise.wilcox.test(Error_data$Error_forecast,Error_data$Methods,
                 p.adjust.method = "bonferroni")

pairwise.wilcox.test(Error_data$Error_backcast,Error_data$Methods,
                 p.adjust.method = "bonferroni")
