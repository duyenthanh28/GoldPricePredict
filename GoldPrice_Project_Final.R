library(forecast)

library(zoo)

library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)



# Set working directory for locating files.
setwd("C:/Users/DELL/Desktop/Time Series/project/Project")

# Create data frame.
price.data <- read.csv("gold_price_data.csv")
tail(price.data)


##################################################################################

# conversion to quarterly data

price.data$quarter <-round_date(
  as.Date(price.data$Date),
  unit = "quarter")

price.data %>%
  group_by(quarter) %>%
  summarize(mean = mean(price.data$Value))
price.data

# till now we have added a new column to the data frame, now we aggregate the series using this added column

df_new = aggregate(price.data$Value, list(price.data$quarter), FUN=mean)

df_new



############################################################################################
price.ts <- ts(df_new$x, 
               start = c(1970, 1), end = c(2020, 1), freq= 4)
price.ts


#Apply the plot() function to create a data plot with the historical data, provide it in your
#report, and explain what time series components can be visualized in this plot. 


# data plot
plot(price.ts, 
     xlab = "Time", ylab = "Revenue (in millions)",
     xaxt="n",bty="l",ylim=c(25,1895),xlim=c(1970.00,2020.75),
     main = "Quaterly Gold Price", col = "black", lwd=2)
axis(1,at=seq(1970,2020,1),labels = format(seq(1970,2020,1)))



don <- xts(x = price.data$Value, order.by = as.Date(price.data$Date))
p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)
p

################################################################################################################


#data patterns 
price.stl <- stl(price.ts,s.window="periodic")
autoplot(price.stl,main=" Quaterly Time Series Components of Gold Price")


# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
autocor <- Acf(price.ts, lag.max = 4, main = "Autocorrelation for price")




###############################################################################################################
#####################################  NAIVE AND SEASONAL NAIVE


# Use accuracy() function and Use round() function to round accuracy measures to three decimal digits.

round(accuracy((naive(price.ts))$fitted, price.ts), 3)

round(accuracy((snaive(price.ts))$fitted, price.ts), 3)


##############################################################################################################
####################################### Train different models and check their accuracy on validation data set.


##Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.

nValid <- 16
nTrain <- length(price.ts) - nValid
train.ts <- window(price.ts, start = c(1970, 1), end = c(1970, nTrain))
valid.ts <- window(price.ts, start = c(1970, nTrain + 1),end = c(1970, nTrain + nValid))

nTrain
nValid
train.ts
valid.ts






####################  MODEL 1 - 2 level Model- Qudratic Regression with Trailing MA



trend.seas <- tslm(train.ts ~ trend+ I(trend^2))
summary(trend.seas)


# Create regression forecast with trend and seasonality for 
# validation period.
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred


# Plot original data and regression forecast for training and 
# validation partitions.
#plots
# Plot original data and regression forecast for training and 
# validation partitions.

#plots

plot(trend.seas.pred$mean, 
     xlab = "Time", ylab = "Price",
     xaxt="n",bty="l",ylim=c(25,2000),xlim=c(1970,2023.25),
     main = "Linear Regression Model with Quadratic Trend and Seasonality",col ="green", lwd=2,lty = "dashed")
axis(1,at=seq(1970,2023.25),labels = format(seq(1970,2023.25)))
lines(trend.seas.pred$fitted, col = "green", lwd = 2)

lines(train.ts, col = "black", lty =1)
lines(valid.ts, col = "black", lty = 1)

lines(c(2016.25,2016.25),c(20,2000))
lines(c(2020.25,2020.25),c(20,2000))

text(1975.65,1950,"Training")
text(2018.05,1950,"Valid")
text(2021.95,1950,"Future")

arrows(1970.05,1860,2016,1860,code = 3,length = 0.1,angle = 30)
arrows(2017.05,1860,2020.00,1860,code = 3,length = 0.1,angle = 30)
arrows(2021.02,1860,2023.75,1860,code = 3,length = 0.1,angle = 30)

# Identify and display residuals for time series based on the regression

trend.seas.reg.res <- trend.seas$residuals
trend.seas.reg.res
# Plot residuals in training partition. 
plot(trend.seas.reg.res, 
     xlab = "Time", ylab = "Price",
     xaxt="n",bty="l",ylim=c(25,2000),xlim=c(1970,2023.25),
     main = "Regression Residuals, Training Partition",col ="green", lwd=2,lty = "dashed")
axis(1,at=seq(1970,2023.25),labels = format(seq(1970,2023.25)))
lines(trend.seas.pred$fitted, col = "green", lwd = 2)

lines(train.ts, col = "black", lty =1)
lines(valid.ts, col = "black", lty = 1)

lines(c(2016.25,2016.25),c(20,2000))
lines(c(2020.25,2020.25),c(20,2000))

text(1975.65,1950,"Training")
text(2018.05,1950,"Valid")
text(2021.95,1950,"Future")

arrows(1970.05,1860,2016,1860,code = 3,length = 0.1,angle = 30)
arrows(2017.05,1860,2020.00,1860,code = 3,length = 0.1,angle = 30)
arrows(2021.02,1860,2023.75,1860,code = 3,length = 0.1,angle = 30)




# Apply trailing MA for residuals with window width k = 2.
ma.trailing.res <- rollmean(trend.seas.reg.res, k = 2, align = "right")
summary(ma.trailing.res)
#Trailing MA forecast of the residuals in the validation period using forecast()
ma.trailing.res.pred <- forecast(ma.trailing.res, h = nValid, level = 0)
ma.trailing.res.pred



# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trailing.res.pred$mean
fst.2level

final.df <- data.frame(valid.ts, trend.seas.pred$mean,ma.trailing.res.pred$mean, 
                       fst.2level)
names(final.df) <- c("Sales","Regression", "Trailing MA", "Combined Forecast")
final.df


round(accuracy(trend.seas.pred$mean, valid.ts), 3) # Regression Model 
round(accuracy(fst.2level, valid.ts), 3)    # 2 level Model( Regression and Trailing MA )




#### Fitting the model for entire data set

# Fit a regression model with quadratic trend for the entire data set.
tot.trend.seas <- tslm(price.ts ~ trend+ I(trend^2))
summary(tot.trend.seas)
#Create regression forecast for future 11 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 11, level = 0)
tot.trend.seas.pred$mean
# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res
# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 2, align = "right")
tot.ma.trail.res
# Create forecast for trailing MA residuals for future 11 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 11, level = 0)
tot.ma.trail.res.pred$mean
# Develop 2-level forecast for future 11 periods by combining regression forecast and trailing MA for residuals for future 11 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level
# Create a table with regression forecast, trailing MA for residuals and total forecast for future 11 periods.
future12.df <- data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                          tot.fst.2level)
names(future12.df) <- c("Regression Forecast", "Trailing MA.Residual Forecast", "Combined Forecast")
future12.df


round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, price.ts), 3) # best result from this 


plot(price.ts, 
     xlab = "Time", ylab = "Price",
     xaxt="n",bty="l",ylim=c(25,2500),xlim=c(1970,2023.25),
     main = "Quadratic Regression With MA",col ="red", lwd=2,lty = "dashed")
axis(1,at=seq(1970,2023.25),labels = format(seq(1970,2023.25)))
lines(tot.trend.seas$fitted ,col = "green", lty =1)  # regression 
lines(tot.trend.seas.pred$mean ,col = "orange", lty =1)  # future regression

lines(tot.trend.seas.res,col = "blue", lty =1) # regression residuals
lines(tot.ma.trail.res,col = "pink", lty =1)  # MA forecasted residuals for training and validation
lines(tot.ma.trail.res.pred$mean, col = "pink", lty = 1) #MA forecasted residuals for future

lines(tot.fst.2level, col = "black", lty =1)


lines(c(2016.25,2016.25),c(20,2500))
lines(c(2020.25,2020.25),c(20,2500))

text(1975.65,2500,"Training")
text(2018.05,2500,"Valid")
text(2021.95,2500,"Future")

arrows(1970.05,2400,2016,2400,code = 3,length = 0.1,angle = 30)
arrows(2017.05,2400,2020.00,2400,code = 3,length = 0.1,angle = 30)
arrows(2021.02,2400,2023.75,2400,code = 3,length = 0.1,angle = 30)


legend(1968,2300, legend = c("Price Time Series", "Quadratic trend Model for Price",
                               "Quadratic trend Forecast for Future", "Price residuals", "MA forecasted residuals for Price and Future",
                              "Combined Forecast ForFuture"), 
       col = c("red", "green" , "orange", "blue","pink", "black"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")



######################### MODEL 2- HW Model with Automatic Selection of Parameters





# Use ets() function with Automated selection for training data
hw.ZZZ <- ets(train.ts, model = "ZZZ")
summary(hw.ZZZ)
# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred
#plots
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Price",
     xaxt="n",bty="l",ylim=c(25,2000),xlim=c(1970,2023.25),
     main = "HW Model with Automatic Selection of Parameters",col ="green", lwd=2,lty = "dashed")
axis(1,at=seq(1970,2023.25),labels = format(seq(1970,2023.25)))
lines(hw.ZZZ.pred$fitted, col = "green", lwd = 2)

lines(train.ts, col = "black", lty =1)
lines(valid.ts, col = "black", lty = 1)

lines(c(2016.25,2016.25),c(20,2000))
lines(c(2020.25,2020.25),c(20,2000))

text(1975.65,1950,"Training")
text(2018.05,1950,"Valid")
text(2021.95,1950,"Future")

arrows(1970.05,1860,2016,1860,code = 3,length = 0.1,angle = 30)
arrows(2017.05,1860,2020.00,1860,code = 3,length = 0.1,angle = 30)
arrows(2021.02,1860,2023.75,1860,code = 3,length = 0.1,angle = 30)

round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

# Fitting the model for entire data set

HW.ZZZ <- ets(price.ts, model = "ZZZ")
summary(HW.ZZZ) 
# Use forecast() function to make predictions using this HW model for
# 11 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 11 , level = 0)
HW.ZZZ.pred

round(accuracy(HW.ZZZ.pred$fitted, price.ts), 3)  





############################   MODEL 3- two level forecasting of regression model with quadratic trend
# +AR(1) model for residuals for validation period


# Using tslm() fun() for creating quadratic trend and seasonal model for training data set.
train.quadtrend.season <- tslm(train.ts ~ trend  + I(trend^2))
summary(train.quadtrend.season)
#using forecast() function to forecast
train.quadtrend.season.pred <- forecast(train.quadtrend.season, h = nValid, level = 0)
train.quadtrend.season.pred

train.quadtrend.season.pred$residuals # residuals
#using Acf() function with the maximum of 8 lags to identify autocorrelation 
#for the regression model residuals of training period
Acf(train.quadtrend.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation plot for Training Residuals with maximum of 8 lags")

# Using Arima() function to fit training residuals for AR(1,0,0) model 
ar1.res <- Arima(train.quadtrend.season$residuals, order = c(1,0,0))
summary(ar1.res)
#Using Acf() function to plot the residuals of the AR(1) model
#(residuals of residuals)
Acf(ar1.res$residuals, lag.max = 8, 
    main = 
      "Autocorrelation plot for the Residuals of Residuals")

#Creating two level forecasting of regression model with quadratic trend 
# +AR(1) model for residuals for validation period

#pred residuals in validation
ar1.res.pred <- forecast(ar1.res, h = nValid, level = 0)
ar1.res.pred

valid.twolevel.pred<- train.quadtrend.season.pred$mean + ar1.res.pred$mean
valid.df<- data.frame(valid.ts, train.quadtrend.season.pred$mean, 
                      ar1.res.pred$mean, valid.twolevel.pred)
names(valid.df) <- c("Revenue", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

round(accuracy(train.quadtrend.season.pred$mean + ar1.res.pred$mean, valid.ts),3)


############# Fitting the model for entire data set

# Using tslm() function for creating quadratic trend and seasonal model for historical data set
quadtrend.season <- tslm(price.ts ~ trend  + I(trend^2) )
summary(quadtrend.season)
# using forecast() function for making predictions with quadratic trend and seasonality 
# into the future 11 quarters.  
quadtrend.season.pred <- forecast(quadtrend.season, h = 11, level = 0)
quadtrend.season.pred
# Using Arima() function to fit AR(1,0,0) model for regression residuals.
# Using forecast() function to make prediction of residuals into the future 11 quarters.
ar1.residuals <- Arima(quadtrend.season$residuals, order = c(1,0,0))
summary(ar1.residuals)
ar1.residuals.pred <- forecast(ar1.residuals, h = 11, level = 0)
ar1.residuals.pred
#Using Acf() function to plot the residuals of the AR(1) model of training data
Acf(ar1.residuals$residuals, lag.max = 8, 
    main = "Autocorrelation plot for AR(1) model Residuals of historic data")

# Two-level forecast for the future 11 periods =
#quadratic trend model  + AR(1) model for residuals.
Twolevel.pred <- quadtrend.season.pred$mean + ar1.residuals.pred$mean
Twolevel.pred

# Creating a data table for quadratic trend model, 
# AR(1) model for residuals forecast for future 11 periods and combined two-level forecast for
# future 11 periods. 
future.df <- data.frame(quadtrend.season.pred$mean, 
                        ar1.residuals.pred$mean, Twolevel.pred)
names(future.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
future.df

# accuracy for entire dataset
round(accuracy(quadtrend.season.pred$fitted + ar1.residuals.pred$fitted, price.ts), 3)


#################### MODEL 4 


# Using Arima() function to fit ARIMA(1,1,1)) model for trend
train.arima <- Arima(train.ts, order = c(1,1,1))
# Summary() to show ARIMA model and its parameters.
summary(train.arima)
#forecasting for validation period
train.arima.pred <- forecast(train.arima, h = nValid, level = 0)
train.arima.pred
#validation accuracy
round(accuracy(train.arima.pred$mean, valid.ts), 3)

## Fitting the model for entire data set

# Using Arima() function to fit ARIMA(1,1,1)(1,1,1) model for the entire data set.
arima <- Arima(price.ts, order = c(1,1,1))
summary(arima)
# Applying forecast for the 11 periods in the future for ARIMA(1,1,1)
arima.pred <- forecast(arima, h = 11, level = 0)
arima.pred
#Entire data set accuracy
round(accuracy(arima.pred$fitted,price.ts), 3)

############################  Model 5 - Auto Arima

#using auto arima function
train.autoarima<- auto.arima(train.ts)
summary(train.autoarima)
#forecasting for validation period
train.autoarima.pred <- forecast(train.autoarima, h = nValid, level = 0)
train.autoarima.pred
#validation accuracy
round(accuracy(train.autoarima.pred$mean, valid.ts), 3)



# Using auto.arima() function for the entire data set..
auto.arima <- auto.arima(price.ts)
summary(auto.arima)
# Applying forecast for the 11 periods in the future for auto ARIMA
auto.arima.pred <- forecast(auto.arima, h = 11, level = 0)
auto.arima.pred
#accuracy for the entire data
round(accuracy(auto.arima.pred$fitted, price.ts), 3)

#### 


# Final Accuracy of all the models on the whole data

round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, price.ts), 3) # best result from this 
round(accuracy(HW.ZZZ.pred$fitted, price.ts), 3)  
round(accuracy(quadtrend.season.pred$fitted + ar1.residuals.pred$fitted, price.ts), 3)
round(accuracy(arima.pred$fitted,price.ts), 3)
round(accuracy(auto.arima.pred$fitted, price.ts), 3)

