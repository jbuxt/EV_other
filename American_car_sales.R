library(forecast)

sales <- read.csv('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/State of tipping report/Positive Tipping/Electric Vehicles/US Car Sales/TOTALSA.csv')

total <- sales$TOTALSA[1:552]
dates <- sales$DATE[1:552]

dt <- as.Date(dates, "%Y-%m-%d")
plot(dt,total,type='l')

total_ts <- ts(total,  frequency=52)
plot(total_ts)

total_stl <- stl(total_ts, s.window=53)

plot(total_stl)
total_res <- total_stl$time.series[,3]
barplot(as.vector(total_res))

res <- as.numeric(total_res)

par(mfrow=c(2,1))

plot(dt,total,type='l',ylab='Total Car sales',xlab='Date',main='US Car Sales with recession start dates')
abline(v=as.Date("1980-01-01"),col='red')
abline(v=as.Date("1981-07-01"),col='red')
abline(v=as.Date("1990-07-01"),col='red')
abline(v=as.Date("2001-03-01"),col='red')
abline(v=as.Date("2007-12-01"),col='red')
abline(v=as.Date("2020-03-01"),col='red')

plot(dt,res,type='l',ylab='Residual sales',xlab='Date',main='Residual of US Car Sales with recession start dates')
abline(v=as.Date("1980-01-01"),col='red')
abline(v=as.Date("1981-07-01"),col='red')
abline(v=as.Date("1990-07-01"),col='red')
abline(v=as.Date("2001-03-01"),col='red')
abline(v=as.Date("2007-12-01"),col='red')
abline(v=as.Date("2020-03-01"),col='red')


#Seperate out time series for certain event

#2008 recession
index1 <- which(dt == '2007-01-01')
index2 <- which(dt == '2016-12-01')
rec_2008 <- total[index1:index2]
dt_2008 <- dt[index1:index2]
plot(dt_2008,rec_2008,type='l')

low_point <- which(rec_2008 == min(rec_2008))
plot(rec_2008,type='l')
low_rec_2008 <- '2009-02-01'
peak_1_2008 <- '2007-12-01'
peak_2_2008 <- '2013-06-01'

abline(v=26,col='red')
abline(v=12,col='blue')
abline(v=78,col='blue')

roi <- 20-rec_2008[26:78]
time_2008 <- 1:length(roi)
plot(time_2008,roi,type='l')

exponential.model <- lm(log(roi)~ time_2008)
linear.model <- lm(roi ~ time)
summary(linear.model)
summary(exponential.model)

coeff1_2008 <- exp(exponential.model$coefficients[1])

coeff2_2008 <- exponential.model$coefficients[2]

plot(time_2008,20-coeff1_2008*exp(coeff2_2008*time_2008))
lines(time_2008,-roi+20,col='red')

plot(rec_2008,type='l')
abline(v=26,col='red')
abline(v=12,col='blue')
abline(v=78,col='blue')
time2 <- time+25

lines(time2,20-coeff1*exp(coeff2*time),col='red')


#Covid impact


index1 <- which(dt == '2018-01-01')
index2 <- which(dt == '2021-12-01')
rec_2020 <- total[index1:index2]
dt_2020 <- dt[index1:index2]

plot(rec_2020,type='l')

which(rec_2020 == min(rec_2020))
low_rec_2020 <- '2020-04-01'
peak_1_2020 <- '2020-02-01'
peak_2_2020 <- '2020-10-01'
plot(rec_2020,type='l',main='Original plot')
abline(v=28,col='red',lty=2)
abline(v=26,col='blue')
abline(v=34,col='blue')
abline(v=38,col='blue')


roi <- 20-rec_2020[28:34]
time_2020 <- 1:length(roi)

plot(time_2020,roi,type='l')
exponential.model <- lm(log(roi)~ time_2020)
linear.model <- lm(roi ~ time)
summary(linear.model)
summary(exponential.model)

coeff1_2020 <- exp(exponential.model$coefficients[1])

coeff2_2020 <- exponential.model$coefficients[2]

time2 <- time_2020+27
lines(time_2020,coeff1_2020*exp(coeff2_2020*time_2020),col='red')
plot(time_2020,roi,type='l')

#1990 recession

index1 <- which(dt == '1988-01-01')
index2 <- which(dt == '1995-12-01')
rec_1990 <- total[index1:index2]
dt_1990 <- dt[index1:index2]

par(mfrow=c(2,1))

plot(rec_1990,type='l')

which(rec_1990 >16)
low_rec_1990 <- '1991-01-01'
peak_1_1990 <- '1990-01-01'
peak_2_1990 <- '1994-02-01'
plot(rec_1990,type='l',main='Original plot')
abline(v=37,col='red',lty=2)
abline(v=25,col='blue')
abline(v=74,col='blue')

roi <- rec_1990[37:74]
time_1990 <- 1:length(roi)

plot(time_1990,roi,type='l')
exponential.model <- lm(log(roi)~ time_1990)
linear.model <- lm(roi ~ time)
summary(linear.model)
summary(exponential.model)

coeff1_1990 <- exp(exponential.model$coefficients[1])

coeff2_1990 <- exponential.model$coefficients[2]

time2 <- time+36
lines(time_1990,coeff1_1990*exp(coeff2_1990*time_1990),col='black')
plot(time_1990,roi,col='red',type='l')


#1980s recession


index1 <- which(dt == '1978-01-01')
index2 <- which(dt == '1987-12-01')
rec_1980 <- total[index1:index2]
dt_1980 <- dt[index1:index2]

par(mfrow=c(1,1))

plot(dt_1980,rec_1980,type='l')

which(rec_1980 == min(rec_1980))
low_rec_1980 <- '1981-12-01'
peak_1_1980 <- '1980-01-01'
peak_2_1980 <- '1984-01-01'
plot(rec_1980,type='l',main='Original plot')
abline(v=48,col='red',lty=2)
abline(v=25,col='blue')
abline(v=76,col='blue')

roi <- rec_1980[48:76]
roi2 <- roi[4:22]
time_1980 <- 1:length(roi)
time2 <- 1:length(roi2)


plot(time,roi,type='l')
exponential.model <- lm(log(roi)~ time_1980)
exponential.model2 <- lm(log(roi2)~ time2)
linear.model <- lm(roi ~ time)
summary(linear.model)
summary(exponential.model)
summary(exponential.model2)


coeff1_1980 <- exp(exponential.model$coefficients[1])
coeff2_1980 <- exponential.model$coefficients[2]

time2 <- time+47
lines(time,coeff1_1980*exp(coeff2_1980*time),col='red')
plot(time,roi,type='l')




#Plot all together on same graph

par(mfrow=c(2,1))

plot(dt,total,type='l',ylab='Total Car sales',xlab='Date',main='US Car Sales with recession start dates')
abline(v=as.Date("1980-01-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("1981-07-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("1990-07-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("2001-03-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("2007-12-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("2020-03-01"),col='darkgrey',lty=2,lwd=2)

#2008 recession
abline(v=as.Date(low_rec_2008),col='red')
#abline(v=as.Date(peak_1_2008),col='blue')
abline(v=as.Date(peak_2_2008),col='blue')

lines(dt_2008[26:78],20-coeff1_2008*exp(coeff2_2008*time_2008),col='red')

#2020 recession
abline(v=as.Date(low_rec_2020),col='red')
#abline(v=as.Date(peak_1_2008),col='blue')
abline(v=as.Date(peak_2_2020),col='blue')

lines(dt_2020[28:34],20-coeff1_2020*exp(coeff2_2020*time_2020),col='red')


#1980 recession
abline(v=as.Date(low_rec_1980),col='red')
#abline(v=as.Date(peak_1_1980),col='blue')
abline(v=as.Date(peak_2_1980),col='blue')

lines(dt_1980[48:76],coeff1_1980*exp(coeff2_1980*time_1980),col='red')


#1990 recession
abline(v=as.Date(low_rec_1990),col='red')
#abline(v=as.Date(peak_1_1980),col='blue')
abline(v=as.Date(peak_2_1990),col='blue')

lines(dt_1990[37:74],coeff1_1990*exp(coeff2_1990*time_1990),col='red')


plot(dt[362:552],total[362:552],type='l',ylab='Total Car sales',xlab='Date',main='US Car Sales with recession start dates = zoomed in')
abline(v=as.Date("1980-01-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("1981-07-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("1990-07-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("2001-03-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("2007-12-01"),col='darkgrey',lty=2,lwd=2)
abline(v=as.Date("2020-03-01"),col='darkgrey',lty=2,lwd=2)

#2008 recession
abline(v=as.Date(low_rec_2008),col='red')
#abline(v=as.Date(peak_1_2008),col='blue')
abline(v=as.Date(peak_2_2008),col='blue')

lines(dt_2008[26:78],20-coeff1_2008*exp(coeff2_2008*time_2008),col='red')



#2020 recession
abline(v=as.Date(low_rec_2020),col='red')
#abline(v=as.Date(peak_1_2008),col='blue')
abline(v=as.Date(peak_2_2020),col='blue')

lines(dt_2020[28:34],20-coeff1_2020*exp(coeff2_2020*time_2020),col='red')





