#Plot ICEV sales in America, Germany and Greece

date <- ym(colnames(ICEV_car_data[,-(1:2)]))

USA_ICEV <- ICEV_car_data[ICEV_car_data$Country == 'USA',]
USA_ICEV <- as.numeric(USA_ICEV[1,-(1:2)])
USA_ICEV <- USA_ICEV[1:180]

USA_diff <- 100*(USA_ICEV-mean(USA_ICEV))/mean(USA_ICEV)
USA_diff_annual <- colSums(matrix(as.numeric(USA_diff),nrow=12))

USA_annual <- colSums(matrix(as.numeric(USA_ICEV),nrow=12))
USA_annual_diff <- 100*(USA_annual-mean(USA_annual,na.rm = TRUE))/mean(USA_annual,na.rm = TRUE)
USA_annual_diff2 <- 100*(USA_annual-USA_annual[1])/USA_annual[1]


Germany_ICEV <- ICEV_car_data[ICEV_car_data$Country == 'Germany',]
Germany_ICEV <- as.numeric(Germany_ICEV[1,-(1:2)])
Germany_ICEV <- Germany_ICEV[1:180]

Germany_diff<- 100*(Germany_ICEV-mean(Germany_ICEV))/mean(Germany_ICEV)
Germany_diff_annual <- colSums(matrix(as.numeric(Germany_diff),nrow=12))

Germany_annual <- colSums(matrix(as.numeric(Germany_ICEV),nrow=12))
Germany_annual_diff <- 100*(Germany_annual-mean(Germany_annual,na.rm = TRUE))/mean(Germany_annual,na.rm = TRUE)
Germany_annual_diff2 <- 100*(Germany_annual-Germany_annual[1])/Germany_annual[1]


Greece_ICEV <- ICEV_car_data[ICEV_car_data$Country == 'Greece',]
Greece_ICEV[1,1:(2+2*12)] <- NA
Greece_ICEV <- as.numeric(Greece_ICEV[1,-(1:2)])
Greece_ICEV <- Greece_ICEV[1:180]

Greece_diff<- 100*(Greece_ICEV-mean(Greece_ICEV,na.rm = TRUE))/mean(Greece_ICEV,na.rm = TRUE)
Greece_diff_annual <- colSums(matrix(as.numeric(Greece_diff),nrow=12))

Greece_annual <- colSums(matrix(as.numeric(Greece_ICEV),nrow=12))
Greece_annual_diff <- 100*(Greece_annual-mean(Greece_annual,na.rm = TRUE))/mean(Greece_annual,na.rm = TRUE)
Greece_annual_diff2 <- 100*(Greece_annual-Greece_annual[3])/Greece_annual[3]


Denmark_ICEV <- ICEV_car_data[ICEV_car_data$Country == 'Denmark',]
Denmark_ICEV[1,1:(2+2*12)] <- NA
Denmark_ICEV <- as.numeric(Denmark_ICEV[1,-(1:2)])
Denmark_ICEV <- Denmark_ICEV[1:180]

Denmark_diff<- 100*(Denmark_ICEV-mean(Denmark_ICEV,na.rm = TRUE))/mean(Denmark_ICEV,na.rm = TRUE)
Denmark_diff_annual <- colSums(matrix(as.numeric(Denmark_diff),nrow=12))

Denmark_annual <- colSums(matrix(as.numeric(Denmark_ICEV),nrow=12))
Denmark_annual_diff <- 100*(Denmark_annual-mean(Denmark_annual,na.rm = TRUE))/mean(Denmark_annual,na.rm = TRUE)
Denmark_annual_diff2 <- 100*(Denmark_annual-Denmark_annual[3])/Denmark_annual[3]


par(mfrow=c(1,1))
plot(USA_annual_diff2,type='l',ylab='Diff in sales',ylim=c(-85,35))
lines(Germany_annual_diff2,col='red')
lines(Denmark_annual_diff2,col='blue')
lines(Greece_annual_diff2,col='grey')
legend('bottomleft',c('USA', 'Germany','Denmark','Greece'),col=c('black','red','blue','grey'),lwd=1)

par(mfrow=c(2,2))
par(mar = c(4, 4, 2, 2)) # Set the margin on all sides to 2

plot(date[1:180],as.numeric(Germany_ICEV[1:180]),type='l',xlab='Year',ylab='ICV Sales',main='Germany')
#abline(v=date[37],col='red')

plot(date[1:180],as.numeric(Denmark_ICEV[1,1:180]),type='l',main='Denmark',xlab='Year',ylab='ICV Sales')
#abline(v=date[37],col='red')
lines(date[50:80],-(-100000 + Denmark_coeff1_2008*exp(Denmark_coeff2_2008*Denmark_time_2008)),col='red')

plot(date[1:180],as.numeric(USA_ICEV[1:180]),type='l',main='USA',xlab='Year',ylab='ICV Sales')
lines(date[49:122],-(-10000000 + coeff1_2008*exp(coeff2_2008*time_2008)),col='red')
#lines(date[49:122],coeff1_2008*exp(coeff2_2008*time_2008),col='red')
#abline(v=date[37],col='red')

plot(date[1:180],as.numeric(Greece_ICEV[1:180]),type='l',main='Greece',xlab='Year',ylab='ICV Sales')
#abline(v=date[37],col='red')


# Calculate return curves for Denmark and USA

#2008 recession - USA

par(mfrow=c(1,1))
index1 <- which(date == '2005-01-01')
index2 <- which(date == '2019-12-01')
USA_2008 <- as.numeric(USA_ICEV[index1:index2])
date_2008 <- date[index1:index2]
plot(date_2008,USA_2008,type='l')
plot(USA_2008,type='l')

low_point <- which(USA_2008 == min(USA_2008))
mean_peak <- mean(USA_2008[1:36])
plot(as.numeric(USA_2008),type='l')
low_USA_2008 <- '2009-02-01'
peak_1_2008 <- '2007-12-01'
peak_2_2008 <- '2015-02-01'
recovered_index <- 122


abline(v=49,col='red')
abline(v=12,col='blue')
abline(v=122,col='blue')

roi <- 10000000 - as.numeric(USA_2008[49:122])
time_2008 <- 1:length(roi)
plot(time_2008,roi,type='l')

exponential.model <- lm(log(roi)~ time_2008)
linear.model <- lm(roi ~ time)
summary(linear.model)
summary(exponential.model)

coeff1_2008 <- exp(exponential.model$coefficients[1])

coeff2_2008 <- exponential.model$coefficients[2]

plot(time_2008,coeff1_2008*exp(coeff2_2008*time_2008))
plot(time_2008,-roi+20,col='red')

par(mfrow=c(2,1))
plot(as.numeric(USA_2008),type='l')
abline(v=49,col='red')
#abline(v=12,col='blue')
abline(v=122,col='blue')
time2 <- time_2008+48

lines(time2,-(-10000000 + coeff1_2008*exp(coeff2_2008*time_2008)),col='red')


#2008 recession - Den

par(mfrow=c(1,1))

index1 <- which(date == '2005-01-01')
index2 <- which(date == '2019-12-01')
Denmark_2008 <- as.numeric(Denmark_ICEV[index1:index2])
date_2008 <- date[index1:index2]
plot(date_2008,Denmark_2008,type='l')
plot(Denmark_2008,type='l')

Denmark_low_point <- which(Denmark_2008 == min(na.omit(Denmark_2008)))
Denmark_mean_peak <- mean(na.omit(Denmark_2008[1:36]))
plot(as.numeric(Denmark_2008),type='l')

lines(ma(Denmark_2008,order=12),col='red')
abline(h=Denmark_mean_peak)
which(ma(Denmark_2008,order=12) > Denmark_mean_peak)


low_Denmark_2008 <- '2009-02-01'
peak_1_2008 <- '2007-12-01'
peak_2_2008 <- '2015-02-01'
Denmark_recovered_index <- 80


abline(v=50,col='red')
abline(v=12,col='blue')
abline(v=80,col='blue')

Denmark_roi <- as.numeric(Denmark_2008[50:80])
Denmark_time_2008 <- 1:length(Denmark_roi)
plot(Denmark_time_2008,Denmark_roi,type='l')

exponential.model <- lm(log(Denmark_roi)~ Denmark_time_2008)
linear.model <- lm(roi ~ time)
summary(linear.model)
summary(exponential.model)

Denmark_coeff1_2008 <- exp(exponential.model$coefficients[1])

Denmark_coeff2_2008 <- exponential.model$coefficients[2]

plot(Denmark_time_2008,Denmark_roi,type='l')
lines(Denmark_time_2008,Denmark_coeff1_2008*exp(Denmark_coeff2_2008*Denmark_time_2008),col='red')
plot(Denmark_time_2008,-Denmark_roi+20,col='red')

par(mfrow=c(2,1))
plot(as.numeric(Denmark_2008),type='l')
plot(date[1:180],as.numeric(Denmark_ICEV[1,1:180]),type='l',main='Denmark',xlab='Year',ylab='ICV Sales')
abline(v=50,col='red')
#abline(v=12,col='blue')
abline(v=80,col='blue')
Denmark_time2 <- Denmark_time_2008+49

lines(date[50:80],Denmark_coeff1_2008*exp(Denmark_coeff2_2008*Denmark_time_2008),col='red')




#Testing testing 






par(mfrow=c(1,1))

index1 <- which(date == '2005-01-01')
index2 <- which(date == '2019-12-01')
Denmark_2008 <- as.numeric(Denmark_ICEV[index1:index2])
date_2008 <- date[index1:index2]
plot(date_2008,Denmark_2008,type='l')
plot(Denmark_2008,type='l')

Denmark_low_point <- which(Denmark_2008 == min(na.omit(Denmark_2008)))
Denmark_mean_peak <- mean(na.omit(Denmark_2008[1:36]))
plot(as.numeric(Denmark_2008),type='l')

lines(ma(Denmark_2008,order=12),col='red')
abline(h=Denmark_mean_peak)
which(ma(Denmark_2008,order=12) > Denmark_mean_peak)


low_Denmark_2008 <- '2009-02-01'
peak_1_2008 <- '2007-12-01'
peak_2_2008 <- '2015-02-01'
Denmark_recovered_index <- 80


abline(v=50,col='red')
abline(v=12,col='blue')
abline(v=80,col='blue')

Denmark_roi <- 100000-as.numeric(Denmark_2008[50:80])
Denmark_time_2008 <- 1:length(Denmark_roi)
plot(Denmark_time_2008,Denmark_roi,type='l')

exponential.model <- lm(log(Denmark_roi)~ Denmark_time_2008)
linear.model <- lm(roi ~ time)
summary(linear.model)
summary(exponential.model)

Denmark_coeff1_2008 <- exp(exponential.model$coefficients[1])

Denmark_coeff2_2008 <- exponential.model$coefficients[2]

plot(Denmark_time_2008,Denmark_roi,type='l')
lines(Denmark_time_2008,Denmark_coeff1_2008*exp(Denmark_coeff2_2008*Denmark_time_2008),col='red')
plot(Denmark_time_2008,-Denmark_roi+20,col='red')

par(mfrow=c(2,1))
plot(as.numeric(Denmark_2008),type='l')
plot(date[1:180],as.numeric(Denmark_ICEV[1,1:180]),type='l',main='Denmark',xlab='Year',ylab='ICV Sales')
abline(v=50,col='red')
#abline(v=12,col='blue')
abline(v=80,col='blue')
Denmark_time2 <- Denmark_time_2008+49

lines(date[50:80],-(-100000 + Denmark_coeff1_2008*exp(Denmark_coeff2_2008*Denmark_time_2008)),col='red')










