
#Code to calculate market share for France, UK and Germany combined, as well as for a large European grouping


#Germany, UK, France = GUF

GUF_full <- c('Germany','France','UK')

GUF_all_sales <- na.omit(all_sales_data2[all_sales_data2$Country == GUF_full[1],])
GUF_ICEV <- na.omit(ICEV_car_data2[ICEV_car_data2$Country == GUF_full[1],])

for (i in 2:length(GUF_full)){
  ts_all_sales <- na.omit(all_sales_data2[all_sales_data2$Country == GUF_full[i],])
  GUF_all_sales <- rbind(GUF_all_sales,ts_all_sales)
  ts_ICEV <- na.omit(ICEV_car_data2[ICEV_car_data2$Country == GUF_full[i],])
  GUF_ICEV <- rbind(GUF_ICEV,ts_ICEV) 
}

GUF_all_sales_sum <- colSums(GUF_all_sales[,-(1:2)])
GUF_ICEV_sum <- colSums(GUF_ICEV[,-(1:2)])

GUF_ICEV_mkt <- 100*GUF_ICEV_sum/GUF_all_sales_sum
GUF_ICEV_mkt_cut <-GUF_ICEV_mkt[-(1:48)] 

plot(GUF_ICEV_mkt,type='l')

euro_ICEV_mkt_cut <- euro_ICEV_mkt[-(1:48)]

#Individual countries

UK_all_sales <-  na.omit(all_sales_data2[all_sales_data2$Country == 'UK',])
UK_all_sales <- as.numeric(UK_all_sales[-(1:2)])
UK_ICEV <- na.omit(ICEV_car_data2[ICEV_car_data2$Country == 'UK',])
UK_ICEV <- as.numeric(UK_ICEV[-(1:2)])
UK_ICEV_mkt <- 100*UK_ICEV/UK_all_sales
UK_ICEV_mkt_cut <-UK_ICEV_mkt[-(1:48)] 


France_all_sales <-  na.omit(all_sales_data2[all_sales_data2$Country == 'France',])
France_all_sales <- as.numeric(France_all_sales[-(1:2)])
France_ICEV <- na.omit(ICEV_car_data2[ICEV_car_data2$Country == 'France',])
France_ICEV <- as.numeric(France_ICEV[-(1:2)])
France_ICEV_mkt <- 100*France_ICEV/France_all_sales
France_ICEV_mkt_cut <-France_ICEV_mkt[-(1:48)] 

Germany_all_sales <-  na.omit(all_sales_data2[all_sales_data2$Country == 'Germany',])
Germany_all_sales <- as.numeric(Germany_all_sales[-(1:2)])
Germany_ICEV <- na.omit(ICEV_car_data2[ICEV_car_data2$Country == 'Germany',])
Germany_ICEV <- as.numeric(Germany_ICEV[-(1:2)])
Germany_ICEV_mkt <- 100*Germany_ICEV/Germany_all_sales
Germany_ICEV_mkt_cut <-Germany_ICEV_mkt[-(1:48)] 

China_all_sales <-  na.omit(all_sales_data2[all_sales_data2$Country == 'China',])
China_all_sales <- as.numeric(China_all_sales[-(1:2)])
China_ICEV <- na.omit(ICEV_car_data2[ICEV_car_data2$Country == 'China',])
China_ICEV <- as.numeric(China_ICEV[-(1:2)])
China_ICEV_mkt <- 100*China_ICEV/China_all_sales
China_ICEV_mkt_cut <-China_ICEV_mkt[-(1:48)] 

USA_all_sales <-  na.omit(all_sales_data2[all_sales_data2$Country == 'USA',])
USA_all_sales <- as.numeric(USA_all_sales[-(1:2)])
USA_ICEV <- na.omit(ICEV_car_data2[ICEV_car_data2$Country == 'USA',])
USA_ICEV <- as.numeric(USA_ICEV[-(1:2)])
USA_ICEV_mkt <- 100*USA_ICEV/USA_all_sales
USA_ICEV_mkt_cut <-USA_ICEV_mkt[-(1:48)] 

plot(date[-(1:48)],UK_ICEV_mkt_cut,type='l')

#ICEV_market_share_global <- global_ICEV_mkt[-(1:48)]

par(mfrow=c(1,1))
plot(China_ICEV_mkt,type='l')
lines(China_ICEV_mkt_cut,col='blue')
abline(v=132,col='red')
ts = China_ICEV_mkt_cut
plot(ts,type='l')
lines(ts[1:132],col='red')
#lines(ma(ts[1:132],order=6),col='red')
plot(ts[1:132],type='l')
lines(ksmooth(1:length(ts[1:132]), ts[1:132], bandwidth=8)$y,col='red')
#ts_res <- ts[1:132] - ma(ts[1:132],order=6)
ts_res <- ts[1:132] - ksmooth(1:length(ts[1:132]), ts[1:132], bandwidth=8)$y
par(mfrow=c(1,1))
barplot(as.numeric(ts_res))

l <- length(ts_res)
wl <- l/2
ar1_mkt_test <- array(NA,dim=(l-wl))
vari_mkt_test <- array(NA,dim=(l-wl))
for (i in 1:(l-wl)){
  ar1_mkt_test[i] <- cor(ts_res[i:(i+wl-1)],ts_res[(i+1):(i+wl)])
  vari_mkt_test[i] <- var(ts_res[i:(i+wl-1)])
}

plot(ar1_mkt_test,type='l')

ar1_mkt_test_tau <- cor.test(1:length(ar1_mkt_test),ar1_mkt_test,test='kendall')$estimate
vari_mkt_test_tau <- cor.test(1:length(vari_mkt_test),vari_mkt_test,test='kendall')$estimate

par(mfrow=c(4,1))
plot(date[-(1:48)],ts,xlab='date',ylab='Market Share',main='UK ICV Market share',type='l')
lines(date[(49:180)],ksmooth(1:length(ts[1:132]), ts[1:132], bandwidth=7)$y,col='red')
barplot(as.numeric(c(ts_res,rep(NA,36))))
plot(c(rep(NA,48),ar1_mkt_test,rep(NA,36)),type='l',ylab='AR(1)',main='AR(1) Tau 0.68')
plot(c(rep(NA,48),vari_mkt_test,rep(NA,36)),type='l',ylab='Variance',main='Variance Tau 0.84')

China_mkt_ar1 <- ar1_mkt_test
China_mkt_vari <- vari_mkt_test
China_res <- ts_res

China_ar1_tau <- ar1_mkt_test_tau
China_vari_tau <- vari_mkt_test_tau

#Plot different market shares together


par(mfrow=c(4,3))
plot(date[-(1:48)],GUF_ICEV_mkt_cut,xlab='Date',ylab='ICV Market Share',main='Germany, UK and France',type='l',ylim=c(0,100),bty='l')
lines(date[(49:180)],ksmooth(1:length(GUF_ICEV_mkt_cut[1:132]), GUF_ICEV_mkt_cut[1:132], bandwidth=8)$y,col='red')
abline(v=date[180],col='blue',lty=3)

plot(date[-(1:48)],China_ICEV_mkt_cut,xlab='Date',ylab='ICV Market Share',main='China',type='l',ylim=c(0,100),bty='l')

lines(date[(49:180)],ksmooth(1:length(China_ICEV_mkt_cut[1:132]), China_ICEV_mkt_cut[1:132], bandwidth=8)$y,col='red')
abline(v=date[180],col='blue',lty=3)

plot(date[-(1:48)],USA_ICEV_mkt_cut,xlab='Date',ylab='ICV Market Share',main='USA',type='l',ylim=c(0,100),bty='l')

lines(date[(49:180)],ksmooth(1:length(USA_ICEV_mkt_cut[1:132]), USA_ICEV_mkt_cut[1:132], bandwidth=8)$y,col='red')
abline(v=date[180],col='blue',lty=3)


barplot(as.numeric(c(GUF_res,rep(NA,36))))
barplot(as.numeric(c(China_res,rep(NA,36))))
barplot(as.numeric(c(USA_res,rep(NA,36))))

plot(date[-(1:48)],c(rep(NA,33),GUF_mkt_ar1,rep(NA,69)),type='l',xlab='Date',ylab='AR(1)',main=expression(paste('AR(1) ',tau, '= 0.78')),bty='l')
plot(date[-(1:48)],c(rep(NA,33),China_mkt_ar1,rep(NA,69)),type='l',xlab='Date',ylab='AR(1)',main=expression(paste('AR(1) ',tau, '= 0.78')),bty='l')
plot(date[-(1:48)],c(rep(NA,33),USA_mkt_ar1,rep(NA,69)),type='l',xlab='Date',ylab='AR(1)',main=expression(paste('AR(1) ',tau, '= -0.52')),bty='l')


plot(date[-(1:48)],c(rep(NA,33),GUF_mkt_vari,rep(NA,69)),type='l',xlab='Date',ylab='Variance',main=expression(paste('Variance ',tau, '= 0.95')),bty='l')
plot(date[-(1:48)],c(rep(NA,33),China_mkt_vari,rep(NA,69)),type='l',xlab='Date',ylab='Variance',main=expression(paste('Variance ',tau, '= 0.87')),bty='l')
plot(date[-(1:48)],c(rep(NA,33),USA_mkt_vari,rep(NA,69)),type='l',xlab='Date',ylab='Variance',main=expression(paste('Variance ',tau, '= -0.10')),bty='l')

bty='l')






#Do significance testing with Chris's method

null_model_ar1_tau <- rep(NA, 10000)
null_model_vari_tau <- rep(NA, 10000)

for (i in 1:10000) {
  resamp <- sample(ts_res)
  ar1_samp <- rep(NA, l-wl+1)
  vari_samp <- rep(NA, l-wl+1)
  for (z in 1:(l-wl+1)) {
    ar1_samp[z] <- ar.ols(resamp[z:(z+wl-1)], aic=FALSE, order.max=1)$ar
    vari_samp[z] <- var(resamp[z:(z+wl-1)])
  }
  null_model_ar1_tau[i] <- cor.test(wl:l, ar1_samp,test='kendall')$estimate
  null_model_vari_tau[i] <- cor.test(wl:l, vari_samp,test='kendall')$estimate
}

ar1_p = length(null_model_ar1_tau[null_model_ar1_tau>ar1_mkt_test_tau])/length(null_model_ar1_tau)
vari_p = length(null_model_vari_tau[null_model_vari_tau>vari_mkt_test_tau])/length(null_model_vari_tau)

par(mfrow=c(2,2))
hist(null_model_ar1_tau,main='AR(1) Tau Sampling')
abline(v=0.78,col='red')
hist(null_model_vari_tau,main='Variance Tau Sampling')
abline(v=0.87,col='red')


hist(null_model_ar1_tau_2,main='AR(1) Tau Sampling - 1/3 wl')
abline(v=0.34,col='red')
hist(null_model_vari_tau_2,main='Variance Tau Sampling - 1/3 wl')
abline(v=0.80,col='red')
