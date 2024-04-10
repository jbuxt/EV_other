


cumulative_german <- cumsum(germany_sales[84:204])

plot(germany_sales[60:90],type='l')
plot(cumulative_german,type='l')

detect_cumulative <- as_detect(na.omit(cumulative_german))
plot(detect_cumulative,type='l')

test <- as_detect(cumulative_german,lowwl=20,highwl=30)
plot(test,type='l')

#market_share

ev_table <- t(read.csv('CarSales_monthly_EV_2004_2022.csv'))
icev_table <- t(read.csv('CarSales_monthly_petrol_2004_2022.csv'))

years <- ev_table[-1,1]
years <- as.numeric(years)
years <- ym(years)
years_cut <- years[13:216]
norway_ev_sales <- ev_table[-1,2]
norway_ev_sales <- na.omit(as.numeric(norway_ev_sales))
germany_ev_sales <- ev_table[-1,3]
germany_ev_sales <- as.numeric(germany_ev_sales)
netherlands_ev_sales <- ev_table[-1,4]
netherlands_ev_sales <- as.numeric(netherlands_ev_sales)
us_ev_sales <- ev_table[-1,5]
us_ev_sales <- as.numeric(us_ev_sales)
china_ev_sales <- ev_table[-1,6]
china_ev_sales <- as.numeric(china_ev_sales)
uk_ev_sales <- ev_table[-1,7]
uk_ev_sales <- na.omit(as.numeric(uk_ev_sales))

norway_icev_sales <- icev_table[-1,2]
norway_icev_sales <- na.omit(as.numeric(norway_icev_sales))
germany_icev_sales <- icev_table[-1,3]
germany_icev_sales <- as.numeric(germany_icev_sales)
netherlands_icev_sales <- icev_table[-1,4]
netherlands_icev_sales <- as.numeric(netherlands_icev_sales)
us_icev_sales <- icev_table[-1,5]
us_icev_sales <- as.numeric(us_icev_sales)
china_icev_sales <- icev_table[-1,6]
china_icev_sales <- as.numeric(china_icev_sales)
uk_icev_sales <- icev_table[-1,7]
uk_icev_sales <- na.omit(as.numeric(uk_icev_sales))

norway_mkt_share <- norway_ev_sales/(norway_ev_sales + norway_icev_sales)
germany_mkt_share <- germany_ev_sales/(germany_ev_sales + germany_icev_sales)
netherlands_mkt_share <- netherlands_ev_sales/(netherlands_ev_sales + netherlands_icev_sales)
us_mkt_share <- us_ev_sales/(us_ev_sales + us_icev_sales)
china_mkt_share <- china_ev_sales/(china_ev_sales + china_icev_sales)
uk_mkt_share <- uk_ev_sales/(uk_ev_sales + uk_icev_sales)

par(mfrow=c(2,3))
plot(years,norway_mkt_share,type='l',main='Norway',ylim=c(0,1))
plot(years,germany_mkt_share,type='l',main='Germany',ylim=c(0,1))
plot(years,netherlands_mkt_share,type='l',main='Netherlands',ylim=c(0,1))
plot(years,us_mkt_share,type='l',main='USA',ylim=c(0,1))
plot(years,china_mkt_share,type='l',main='China',ylim=c(0,1))
plot(years,uk_mkt_share,type='l',main='UK',ylim=c(0,1))


parr(mfrow=c(2,1))
germany_mkt_detect <- as_detect(germany_mkt_share)

plot()
plot(years,germany_mkt_detect,type='l')



