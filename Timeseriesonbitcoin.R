library(forecast)
library(ggplot2)
library(gridExtra)
library(fGarch)
library(astsa)
library(xts)
library(fpp2)
library(fpp)
library(fma)

### Generate Daily Closing Prices into 3 section


bitcoin<-read.csv("BitcoinStockMarketHistoricalData_2014_2021.csv")

bitcoin<-bitcoin[,2:7]

bitcoin$Open<-as.numeric(bitcoin$Open)
bitcoin$High<-as.numeric(bitcoin$High)
bitcoin$Low<-as.numeric(bitcoin$Low)
bitcoin$Close<-as.numeric(bitcoin$Close)
bitcoin$Adj.Close<-as.numeric(bitcoin$Adj.Close)
bitcoin$Volume<-as.numeric(bitcoin$Volume)




grid.arrange(bitcoin %>% 
               gather(bitcoin, value, Open, High, Low, Close) %>% 
               ggplot(aes( y = value))+
               geom_boxplot()+
               facet_wrap(~bitcoin)+default_theme+ theme(strip.background = element_rect(fill="orange"))+
               ggtitle("Boxplot of Bitcoin Price"),bitcoin %>% ggplot( aes( y=Volume)) +
               geom_boxplot()+default_theme+ theme(strip.background = element_rect(fill="orange"))+
               ggtitle("Boxplot of Trading Volume"),nrow=1,widths = c(2,1.5))  





bitcoin<-read.csv("BitcoinStockMarketHistoricalData_2014_2021.csv")
bitcoin$Close=as.numeric(bitcoin$Close)#daily closing prices
# 3 Section
bitcoin_1<-bitcoin[1:837,]
bitcoin_2<-bitcoin[838:1932,]
bitcoin_3<-bitcoin[1933:2361,]
# there is 4 NA in the column
bitcoincp_1=na.omit(bitcoin_1$Close) 
bitcoincp_2=na.omit(bitcoin_2$Close) 
bitcoincp_3=na.omit(bitcoin_3$Close) 
# TS data
bitcoints_1<-ts(bitcoincp_1,start=c(2014,9,17), frequency = 365.25)
bitcoints_2<-ts(bitcoincp_2,start=c(2017,1,1), frequency = 365.25)
bitcoints_3<-ts(bitcoincp_3,start=c(2020,1,1), frequency = 365.25)

bitcoints<-ts(na.omit(bitcoin$Close) ,start=c(2014,9,17), frequency = 365.25)
dbitcoints<-diff(log(bitcoints))



## Section 1: 2014-9-14 to 2016-12-31


### Lag Plots


lag1<-gglagplot(bitcoin[1:837,]$Close, do.lines = F) +default_theme+
  scale_color_continuous(low = "#38ccf5", high = "#020f73", breaks = c(1, 106, 471), labels = c('2014', '2015', '2016')) + 
  scale_y_continuous(breaks = c(0, 250, 500,750,1000), 
                     labels = c('$0', '$0.25K', '$0.5K', '$0.75K','$1K')) +
  scale_x_continuous(breaks = c(0, 250, 500,750,1000), 
                     labels = c('$0', '$0.25K', '$0.5K', '$0.75K','$1K'))+ggtitle("Lag Plot of Bitcoin Closing Price")

ts_decomposed <- decompose(bitcoints_1, type = c('multiplicative'))
p11<-autoplot(ts_decomposed)+default_theme
grid.arrange(p11,lag1,nrow=1)





# Return Data
dbitcoints_1<-diff(log(bitcoints_1))
# Plot TS 
TS1<-autoplot(bitcoints_1,ylab="Daily Closing Prices",main="Bitcoin Time Series Plot",colour = 'steelblue')+default_theme
TS1_diff<-autoplot(dbitcoints_1,ylab="Daily Return",main="Bitcoin Return Time Series Plot",colour = 'steelblue')+default_theme
# Check if the return data is stationary
adf.test(dbitcoints_1)
p1<-grid.arrange(TS1,TS1_diff,nrow=2)








### ACF/PACF Plot of Return Data



grid.arrange(ggAcf(dbitcoints_1,40)+ggtitle("ACF Plot of Bitcoin Return Data")+default_theme,ggPacf(dbitcoints_1,40)+ggtitle("PACF Plot of Bitcoin Return Data")+default_theme,nrow=1)

# grid.arrange(ggAcf(dbitcoincp^2,40)+ggtitle("ACF Plot of Square of Return Data")+default_theme,ggPacf(dbitcoincp^2,40)+ggtitle("PACF Plot of Square Return Data")+default_theme,nrow=1)


### ARIMA Model


auto.arima(dbitcoints_1)

fit202 <- Arima(dbitcoints_1, order=c(2, 0, 2),include.drift = TRUE)
summary(fit202)

sarima(dbitcoints_1,2,0,2)

The auto.arima suggests ARIMA(1,1,0), however, the  The final model we choose for phase 1 is ARIMA(2,0,2) for the Bitcoin return data. 




## Section 2: 2014-9-14 to 2016-12-31


### Lag Plots


lag2<-gglagplot(bitcoin[838:1932,]$Close, do.lines = F) +default_theme+
  scale_color_continuous(low = "#21d40b", high = "#0a3b04", breaks = c(1, 366, 731), labels = c('2017', '2018', '2019')) + 
  scale_y_continuous(breaks = c(0, 5000, 10000,15000,20000), 
                     labels = c('$0', '$5K', '$10K', '$15K','20K')) +
  scale_x_continuous(breaks = c(0, 5000, 10000,15000,20000), 
                     labels = c('$0', '$5K', '$10K', '$15K','20K'))+ggtitle("Lag Plot of Bitcoin Closing Price")

ts_decomposed <- decompose(bitcoints_2, type = c('multiplicative'))
p22<-autoplot(ts_decomposed)+default_theme
grid.arrange(p22,lag2,nrow=1)







# Return Data
dbitcoints_2<-diff(log(bitcoints_2))

# Plot TS 
TS2<-autoplot(bitcoints_2,ylab="Daily Closing Prices",main="Bitcoin Time Series Plot",
              colour = 'darkseagreen4')+default_theme


TS2_diff<-autoplot(dbitcoints_2,ylab="Daily Return",main="Bitcoin Return Time Series Plot",colour = 'darkseagreen4')+default_theme
adf.test(diff(bitcoints_1))

grid.arrange(TS2,TS2_diff,nrow=2)

adf.test(dbitcoints_2)






### ACF/PACF Plot of Return Data



grid.arrange(ggAcf(dbitcoints_2,40)+ggtitle("ACF Plot of Bitcoin Return Data")+default_theme,ggPacf(dbitcoints_2,40)+ggtitle("PACF Plot of Bitcoin Return Data")+default_theme,nrow=1)

# grid.arrange(ggAcf(dbitcoincp^2,40)+ggtitle("ACF Plot of Square of Return Data")+default_theme,ggPacf(dbitcoincp^2,40)+ggtitle("PACF Plot of Square Return Data")+default_theme,nrow=1)


### ARIMA Model


auto.arima(dbitcoints_2)

fit111 <- Arima(dbitcoints_2, order=c(0, 0, 0),include.drift = TRUE)
summary(fit111)

sarima(dbitcoints_2,0,0,0)

The final model we choose for phase 2 is ARIMA(0,0,0) with intercept 0.0018 for the Bitcoin return data. 





## Section 3: 2020-1-1 to 2021-3-4


### Lag Plots


lag3<-gglagplot(na.omit(bitcoin[1933:2361,])$Close, do.lines = F) +default_theme+
  scale_color_continuous(low = "pink", high = "pink4", breaks = c(1, 345), labels = c('2020', '2021')) + 
  scale_y_continuous(breaks = c(0, 20000, 40000,60000), 
                     labels = c('$0', '$20K', '$40K', '$60K')) +
  scale_x_continuous(breaks = c(0, 20000, 40000,60000), 
                     labels = c('$0', '$20K', '$40K', '$60K'))+ggtitle("Lag Plot of Bitcoin Closing Price")

lag3





# Return Data
dbitcoints_3<-diff(log(bitcoints_3))

# Plot TS 
TS3<-autoplot(bitcoints_3,ylab="Daily Closing Prices",main="Bitcoin Time Series Plot",
              colour = 'rosybrown3')+default_theme


TS3_diff<-autoplot(dbitcoints_3,ylab="Daily Return",main="Bitcoin Return Time Series Plot",colour = 'rosybrown3')+default_theme
adf.test(diff(bitcoints_3))

grid.arrange(TS3,TS3_diff,nrow=2)

# denp<-ggplot(dbitcoints_3, aes(x=dbitcoints_3)) +
#   ggtitle("Density Plot of Return Data")+
#   geom_density(color="rosybrown3")+
#   xlab("Bitcoin Return Data")+
#   geom_vline(aes(xintercept=mean(dbitcoints_3)),
#             color="red", linetype="dashed", size=1)+default_theme
# 
# 
# grid.arrange(p1,denp,nrow=1,widths = c(2,1.5))


The original time series plot is not stationary with random trend. From the time series plot of return data above, it is stationary, and there seems to have volatility clustering and heavy tailed distribution (some outliers) and in addition, we can find a strong asymmetry in the density plot.


### ACF/PACF Plot of Return Data



grid.arrange(ggAcf(dbitcoints_3,40)+ggtitle("ACF Plot of Bitcoin Return Data")+default_theme,ggPacf(dbitcoints_3,40)+ggtitle("PACF Plot of Bitcoin Return Data")+default_theme,nrow=1)

# grid.arrange(ggAcf(dbitcoints_3^2,40)+ggtitle("ACF Plot of Square of Return Data")+default_theme,ggPacf(dbitcoints_3^2,40)+ggtitle("PACF Plot of Square Return Data")+default_theme,nrow=1)


To obtain an interpretable model, we are going to use only AR model for the conditional mean. Thus the pattern of ACF and PACF is not important. Find the order of an AR model using PACF.


### ARIMA Model


auto.arima(dbitcoints_3)

fit101 <- Arima(log(bitcoints_3), order=c(1, 1, 1),include.drift = TRUE)
summary(fit101)

sarima(log(bitcoints_3),1,1,1)





### Forcasting 


autoplot(forecast(fit101), xlab = 'Year',ylab = 'Log of Closing Price (USD)')+default_theme










# All Return Data from 2014 to 2021









# Plot TS 
TS<-autoplot(bitcoints,ylab="Daily Closing Prices",main="Bitcoin Time Series Plot",
             colour = 'sienna3')+default_theme


TS_diff<-autoplot(dbitcoints,ylab="Daily Return",main="Bitcoin Return Time Series Plot",colour = 'sienna3')+default_theme
adf.test(diff(bitcoints))

p111<-grid.arrange(TS,TS_diff,nrow=2)

denp1<-ggplot(dbitcoints, aes(x=dbitcoints)) +
  ggtitle("Density Plot of Return Data")+
  geom_density(color="sienna3")+
  xlab("Bitcoin Return Data")+
  geom_vline(aes(xintercept=mean(dbitcoints)),
             color="red", linetype="dashed", size=1)+default_theme


grid.arrange(p111,denp1,nrow=1,widths = c(2,1.5))


grid.arrange(ggAcf(dbitcoints,40)+ggtitle("ACF Plot of Bitcoin Return Data")+default_theme,ggPacf(dbitcoints,40)+ggtitle("PACF Plot of Bitcoin Return Data")+default_theme,nrow=1)




source("sarima.R")
fit=sarima(dbitcoints,6,0,0)


resi=fit$fit$resi 
bbb<-grid.arrange(ggAcf(resi^2,40),ggPacf(resi^2,40),nrow=1)
par(mfrow=c(2,1))

source("archlmtest.R")
archlmtest(resi,1)$p.value #Find an ARCH effect, so we don't need further investigation.
archlmtest(resi,2)$p.value
archlmtest(resi,3)$p.value
archlmtest(resi,4)$p.value
archlmtest(resi,5)$p.value




fitg611=garchFit(~arma(6,0)+garch(1,1),dbitcoints)
summary(fitg611)

mu=fitg611@fitted
sd=fitg611@sigma.t
#prediction interval
upper=mu+2*sd #assuming normal error e_t
lower=mu-2*sd



par(mfrow=c(2,1))
plot(dbitcoints,type="l",ylab="Bitcoin Returns",main="One-step-ahead Prediction")
lines(mu,lty=3,col="red")
lines(upper,lty=2,col="blue")
lines(lower,lty=2,col="blue")


plot(dbitcoints[2200:2356],type="l",ylab="Bitcoin Returns",main="One-step-ahead Prediction(Last Part of Data)")
lines(mu[2200:2356],lty=3,col="red")
lines(upper[2200:2356],lty=2,col="blue")
lines(lower[2200:2356],lty=2,col="blue")

