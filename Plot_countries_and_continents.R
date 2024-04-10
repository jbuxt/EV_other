library(readxl)
library(lubridate)
library(forecast)

ICEV_car_data2 <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/State of tipping report/Positive Tipping/Electric Vehicles/Pre-2016 Adjusted data/Expanded dataset - revised/Global_sales_by_country_0511.xlsx',sheet='ICEV')
EV_car_data2 <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/State of tipping report/Positive Tipping/Electric Vehicles/Pre-2016 Adjusted data/Expanded dataset - revised/Global_sales_by_country_0511.xlsx',sheet='EV')
PHEV_car_data2 <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/State of tipping report/Positive Tipping/Electric Vehicles/Pre-2016 Adjusted data/Expanded dataset - revised/Global_sales_by_country_0511.xlsx',sheet='PHEV')
HEV_car_data2 <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/State of tipping report/Positive Tipping/Electric Vehicles/Pre-2016 Adjusted data/Expanded dataset - revised/Global_sales_by_country_0511.xlsx',sheet='HEV')
MHEV_car_data2 <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/State of tipping report/Positive Tipping/Electric Vehicles/Pre-2016 Adjusted data/Expanded dataset - revised/Global_sales_by_country_0511.xlsx',sheet='MHV')
all_sales_data2 <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/State of tipping report/Positive Tipping/Electric Vehicles/Pre-2016 Adjusted data/Expanded dataset - revised/Global_sales_by_country_0511.xlsx',sheet='SalesAll')

date <- ym(colnames(ICEV_car_data2[,-(1:2)]))


Norway_ICEV <- ICEV_car_data2[ICEV_car_data2$Country == 'Norway',]
Norway_ICEV <- as.numeric(Norway_ICEV[1,-(1:2)])
Norway_EV <- EV_car_data2[EV_car_data2$Country == 'Norway',]
Norway_EV <- c(array(0,dim=48),as.numeric(Norway_EV[1,-(1:2)]))
Norway_PHEV <- PHEV_car_data2[PHEV_car_data2$Country == 'Norway',]
Norway_PHEV <- c(array(0,dim=48),as.numeric(Norway_PHEV[1,-(1:2)]))
Norway_HEV <- HEV_car_data2[HEV_car_data2$Country == 'Norway',]
Norway_HEV <- as.numeric(Norway_HEV[1,-(1:2)])
Norway_MHEV <- MHEV_car_data2[MHEV_car_data2$Country == 'Norway',]
Norway_MHEV <- as.numeric(Norway_MHEV[1,-(1:2)])

x = data.frame(Norway_ICEV,Norway_EV,Norway_PHEV,Norway_HEV,Norway_MHEV)

Norway_all <- rowSums(x)

Norway_mkt_share <- 100*Norway_ICEV/Norway_all

#Add in Norway to original dataset

all_sales_data2[33,-(1:2)] <- t(Norway_all)


#Select countries to plot for continent level

euro_full <- c('France','Germany','Spain','Italy','Portugal','UK','Ireland','Belgium',
               'Netherlands','Sweden','Norway','Finland','Denmark','Greece','Czech Republic','Romania')

asia_full <- c('China','Japan','Korea','Thailand')

americas_full <- c('USA','Canada','Mexico','Brazil',"Argentina")

global_full <- c(euro_full,asia_full,americas_full)

# Select countries to plot

UK_ICEV <- ICEV_car_data2[ICEV_car_data2$Country == 'UK',]
UK_ICEV <- as.numeric(UK_ICEV[1,-(1:2)])

france_ICEV <- ICEV_car_data2[ICEV_car_data2$Country == 'France',]
france_ICEV <- as.numeric(france_ICEV[1,-(1:2)])

germany_ICEV <- ICEV_car_data2[ICEV_car_data2$Country == 'Germany',]
germany_ICEV <- as.numeric(germany_ICEV[1,-(1:2)])

china_ICEV <- ICEV_car_data2[ICEV_car_data2$Country == 'China',]
china_ICEV <- as.numeric(china_ICEV[1,-(1:2)])

US_ICEV <- ICEV_car_data2[ICEV_car_data2$Country == 'USA',]
US_ICEV <- as.numeric(US_ICEV[1,-(1:2)])

portugal_ICEV <- ICEV_car_data2[ICEV_car_data2$Country == 'Portugal',]
portugal_ICEV <- as.numeric(portugal_ICEV[1,-(1:2)])




#Calculate residual

data <-americas_ICEV_sum[-(1:72)]
data_cut <- data[1:108]

plot(data,type='l')
lines(data_cut,col='red')
ts <- ts(data_cut,frequency=12)

ts_stl <- stl(ts,s.window=7)

plot(ts_stl)

ts_res <- as.numeric(ts_stl$time.series[,3])

l <- length(ts_res)
wl <- l/2
ar1_ICV_test <- array(NA,dim=(l-wl))

vari_ICV_test <- array(NA,dim=(l-wl))
for (i in 1:(l-wl)){
  ar1_ICV_test[i] <- cor(ts_res[i:(i+wl-1)],ts_res[(i+1):(i+wl)])
  vari_ICV_test[i] <- var(ts_res[i:(i+wl-1)])
}

ar1_ICV_americas <- ar1_ICV_test

vari_ICV_americas <- vari_ICV_test

plot(ar1_ICV_test,type='l')
plot(vari_ICV_test,type='l')


ar1_ICV_americas_tau <- cor.test(1:length(ar1_ICV_americas),ar1_ICV_americas,test='kendall')$estimate
vari_ICV_americas_tau <- cor.test(1:length(vari_ICV_test),vari_ICV_test,test='kendall')$estimate


