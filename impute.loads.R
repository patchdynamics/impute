library(xts)
library(dplyr)

impute.daily.loads = function(concentration, concentration.dates, 
                              discharge, discharge.dates, 
                              ignore=c(),
                              use.linear.model=FALSE,
                              window.size=10
){
  data = data.frame(var = concentration, date.var=concentration.dates)
  discharge = data.frame(val=discharge, datetime=discharge.dates)
  
  data$date= as.Date(data[,c('date.var')], format='%m/%d/%Y')
  data.days = aggregate(as.formula('var ~ date'),
                        data, mean)
  
  discharge.xts = xts(discharge$val, order.by=as.POSIXlt(as.character(discharge$datetime), 
                                                         format='%Y-%m-%d', tz=Sys.timezone()))
  
  discharge.xts = discharge.xts['1975-01-01/']
  
  data.xts = xts(data.days$var, order.by=as.POSIXlt(as.character(data.days$date), format='%Y-%m-%d', tz=Sys.timezone()))
  data.xts.merged = merge(data.xts, discharge.xts)
  data.xts = na.trim(data.xts.merged, sides='both', is.na='any')
  names(data.xts) = c('var', 'discharge')
  
  data.xts$imputed = NA
  data.xts$skip = FALSE
  data.xts$skip[ignore] = TRUE
  impute = which(is.na(data.xts$var & data.xts$skip==FALSE))
  nas = which(is.na(data.xts$var))
  for(i in impute){
    d = abs(.index(data.xts)[i] - .index(data.xts))
    d[nas] = NA
    df.sort = data.frame(dist = d, var = data.xts$var, discharge=data.xts$discharge)
    df.sort = dplyr::arrange(df.sort, dist)
    if(use.linear.model){
      lm.model = lm(var ~ discharge, df.sort[1:window.size,])
      imputed.conc = predict(lm.model, data.frame(discharge = data.xts$discharge[i]))
    } else {
      imputed.conc = mean(df.sort[1:window.size,]$var)
    }
    data.xts$imputed[i] = imputed.conc
  }
  
  data.xts$combined = data.xts$var
  data.xts$combined[is.na(data.xts$combined)] = data.xts$imputed[is.na(data.xts$combined)]
  
  data.xts$daily.loads = data.xts$discharge * data.xts$combined * 28.31685 * 60 * 60 * 24
  data.xts$month = .indexmon(data.xts)
  data.xts$year = .indexyear(data.xts)
  data.df = as.data.frame(data.xts)
  monthly.loads = aggregate(list(monthly.loads = data.df$daily.loads),
                            list(month=data.df$month, year=data.df$year)
                            , sum)
  monthly.loads$monthly.loads = monthly.loads$monthly.loads * 1e-9
  return(monthly.loads)
}



