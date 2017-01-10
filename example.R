library(dataRetrieval)
library(waterdata)

var='SRP..mg.L..as.P'
date.var = 'Datetime..date.and.time.of.sample.collection.'

data <- read.csv(file="Maumeedata.csv", header=T)
ind  = which(data[,c(var)]<=0)
data = data[-ind,c(var, date.var)]
names(data) = c('var', 'date.var')

# this USGS code is not working for loading maumee currently
# usgs.station.id = "04193500"
# sdate = "1970-01-01"; edate = "2016-01-01"
# discharge = importDVs(usgs.station.id,code="00060", sdate=sdate, edate=edate)


discharge = maumee # because usgs is being annoying
write.csv(maumee, file='MaumeeDischarge.csv')
ignore = seq.POSIXt(as.POSIXlt('1978-10-01', tz=Sys.timezone()), as.POSIXlt('1981-10-12', tz=Sys.timezone()),'days')

monthly.loads.SRP = impute.daily.concentrations(data$var, data$date.var, discharge$val, discharge$datetime, ignore)
write.csv(monthly.loads.SRP, file='monthly.loads.SRP.csv')

monthly.loads.SRP.lm = impute.daily.concentrations(data$var, data$date.var, discharge$val, discharge$datetime, ignore,
                                                   use.linear.model = TRUE)

# TP
var='TP..mg.L.as.P'
date.var = 'Datetime..date.and.time.of.sample.collection.'

data <- read.csv(file="Maumeedata.csv", header=T)
ind  = which(data[,c(var)]<=0)
data = data[-ind,c(var, date.var)]
names(data) = c('var', 'date.var')
ignore = seq.POSIXt(as.POSIXlt('1978-10-01', tz=Sys.timezone()), as.POSIXlt('1981-10-12', tz=Sys.timezone()),'days')

monthly.loads.TP = impute.daily.concentrations(data$var, data$date.var, discharge$val, discharge$datetime, ignore, 
                                            use.linear.model = TRUE)
write.csv(monthly.loads.TP, file='monthly.loads.TP.lm.csv')

monthly.loads.TP.mean = impute.daily.concentrations(data$var, data$date.var, discharge$val, discharge$datetime, ignore, 
                                               use.linear.model = FALSE)
write.csv(monthly.loads.TP.mean, file='monthly.loads.TP.mean.csv')

