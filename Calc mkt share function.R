
setwd('/Users/joshbuxton/Downloads')

ICEV_car_data2 <- read_excel('./Global_sales_by_country_0423.xlsx',sheet='ICEV')
EV_car_data2 <- read_excel('./Global_sales_by_country_0423.xlsx',sheet='EV')
PHEV_car_data2 <- read_excel('./Global_sales_by_country_0423.xlsx',sheet='PHEV')
HEV_car_data2 <- read_excel('./Global_sales_by_country_0423.xlsx',sheet='HEV')
MHEV_car_data2 <- read_excel('./Global_sales_by_country_0423.xlsx',sheet='MHV')
all_sales_data2 <- read_excel('./Global_sales_by_country_0423.xlsx',sheet='SalesAll')

date <- ym(colnames(ICEV_car_data2[,-(1:2)]))

ICEV_car_data2 <- read_excel(paste0('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/State of tipping report/Positive Tipping/Electric Vehicles/Pre-2016 Adjusted data/Expanded dataset - revised/Global_sales_by_country_', '04', '.xlsx'),sheet='ICEV')
                             
country='France'
mkt_share_ar1_calc <- function(country, bw){

  all_sales <- all_sales_data2[all_sales_data2$Country == country,]
  all_sales <- as.numeric(na.omit(all_sales[,-(1:2)]))
  ICEV_sales <- ICEV_car_data2[ICEV_car_data2$Country == country,]
  ICEV_sales <- as.numeric(na.omit(ICEV_sales[,-(1:2)]))
  HEV_sales <- HEV_car_data2[HEV_car_data2$Country == country,]
  HEV_sales <- as.numeric(na.omit(HEV_sales[,-(1:2)]))
  MHEV_sales <- MHEV_car_data2[MHEV_car_data2$Country == country,]
  MHEV_sales <- as.numeric(na.omit(MHEV_sales[,-(1:2)]))
  PHEV_sales <- PHEV_car_data2[PHEV_car_data2$Country == country,]
  PHEV_sales <- as.numeric(na.omit(PHEV_sales[,-(1:2)]))
  EV_sales <- EV_car_data2[EV_car_data2$Country == country,]
  EV_sales <- as.numeric(na.omit(EV_sales[,-(1:2)]))
  
  
  ICEV_market_share <- 100*ICEV_sales/all_sales
  ICEV_market_share <- ICEV_market_share[-(1:48)]
  #ICEV_market_share_cut <- ICEV_market_share[-(1:84)]
  
  plot(ICEV_market_share[-(1:84)],type='l',main=country)
  abline(v=48,col='red')
  
  ts_res <- ICEV_market_share[1:132] - ksmooth(1:length(ICEV_market_share[1:132]), ICEV_market_share[1:132], bandwidth=bw)$y

  #plot(ICEV_market_share_cut[1:48],type='l')
  #lines(ma(ICEV_market_share_cut[1:48],order=6),col='red')
  #barplot(as.numeric(ts_res))
  
  l <- length(ts_res)
  wl <- l/2
  ar1_mkt <- array(NA,dim=(l-wl))
  vari_mkt <- array(NA,dim=(l-wl))
  skew_mkt <- array(NA,dim=(l-wl))
  
  for (i in 1:(l-wl)){
    ar1_mkt[i] <- cor(ts_res[i:(i+wl-1)],ts_res[(i+1):(i+wl)])
    vari_mkt[i] <- var(ts_res[i:(i+wl-1)])
    skew_mkt[i] <- skewness(ts_res[i:(i+wl-1)])
  }
  
  ar1_mkt_tau <- cor.test(1:length(ar1_mkt),ar1_mkt,test='kendall')$estimate
  vari_mkt_tau <- cor.test(1:length(vari_mkt),vari_mkt,test='kendall')$estimate
  skew_mkt_tau <- cor.test(1:length(skew_mkt),skew_mkt,test='kendall')$estimate
  
  print(paste0(country, " AR1 Tau: ",format(round(ar1_mkt_tau,3))))
  print(paste0(country, " Vari Tau: ",format(round(vari_mkt_tau,3))))
  
  par(mfrow=c(5,1))
  par(mar=c(2,2,2,2))
  plot(date[-(1:48)],ICEV_market_share,type='l',ylab='Mkt share',ylim=c(0,100),main=paste0(country, ' - ICV Mkt Share'))
  abline(v=date[180],col='red')
  plot(date[-(1:48)],ICEV_sales[-(1:48)],type='l',ylab='ICV sales',main='All and ICV Sales',col='blue')
  lines(date[-(1:48)],all_sales[-(1:48)],col='black')
  legend("topleft",legend=c('All Sales','ICV Sales'),col=c('black','blue'),lty=1,cex=1)
  plot(date[-(1:48)],HEV_sales[-(1:48)],type='l',ylab='HEV sales',main='HEV')
  lines(date[-(1:48)],PHEV_sales,col='red')
  legend("topleft",legend=c('HEV','PHEV'),col=c('black','red'),lty=1,cex=1)
  plot(date[-(1:48)],EV_sales,type='l',ylab='EV sales',main='EV')
  plot(date[-(1:48)],c(array(NA,dim=66),ar1_mkt,array(NA,dim=36)),type='l',main='AR1')
  
  par(mfrow=c(5,1))
  par(mar=c(2,2,2,2))
  plot(date[-(1:48)],ICEV_market_share,type='l',ylab='Mkt share',ylim=c(0,100),main=paste0(country, ' - ICV Mkt Share'))
  abline(v=date[180],col='red')
  plot(date[-(1:48)],c(ICEV_market_share[1:132],array(NA,dim=36)),type='l',ylab='Mkt share',main=country)
  lines(date[-(1:48)],c(ksmooth(1:length(ICEV_market_share[1:132]), ICEV_market_share[1:132], bandwidth=bw)$y,array(NA,dim=36)),col='blue')
  legend("bottomleft",legend=c('Mkt Share',paste('Bandwidth:',bw)),col=c('black','blue'),lty=1,cex=1)
  barplot(c(as.numeric(ts_res),array(NA,dim=36)))
  plot(date[-(1:48)],c(array(NA,dim=66),ar1_mkt,array(NA,dim=36)),type='l',main= paste0("AR1 tau: ",format(round(ar1_mkt_tau,2))))
  plot(date[-(1:48)],c(array(NA,dim=66),vari_mkt,array(NA,dim=36)),type='l',main=paste0("Variance Tau: ",format(round(vari_mkt_tau,2))))
}
  
x <- mkt_share_ar1_calc("Sweden",8)


par(mfrow=c(3,1))
plot(ICEV_market_share,type='l',main='France Mkt Share')
abline(v=84,col='red')
abline(v=132,col='red')
plot(ICEV_market_share_cut[1:48],type='l',main='Cut mkt share with ma=4')
lines(ma(ICEV_market_share_cut[1:48],order=4),col='red')

plot(ar1_mkt,type='l',main='AR1')
plot(vari_mkt,type='l')
