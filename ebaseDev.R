lapply(c('data.table', 'lfe', 'ggplot2', 'xgboost', 'ggthemes'), library, character.only = TRUE)
rm(list = ls())
source('R/mainFunctions.R')
source('R/helperFunctions.R')
#library('ebase')

sampDT <- fread('vignettes/vignetteData.csv')
sampDT[meter == 'J', eventStart:= '2/1/2017']
sampDT[meter == 'J', eventEnd:= '7/1/2017']

# Get model
startTime <- Sys.time()
bList <- baselineModel(dt = sampDT,
                       id.var = meter,
                       use.var = electricity,
                       date.var = date,
                       hour.var = hour,
                       temp.var = temperature,
                       install.date.var = install_date,
                       base.length = 12,
                       date.format = '%m/%d/%Y',
                       include.gboost = TRUE,
                       nra.start.var = eventStart)
Sys.time() - startTime

# bList$predictions[baseline == 0, sum(elct - pElct)/sum(pElct), by = 'meterID']
summary(bList)


meterDT <- unique(sampDT[, .(meterID = meter, inDate = install_date, propertyName = rep('Tits', 3))])
meterDT[, inDate:= as.POSIXct(inDate, tz = 'UTC', format = '%m/%d/%Y')]
#Cumulative Savings
plot(bList,
     type = 'dailysave',
     propertyTable = data.table(meterID = c('I', 'J', 'L'),
                                propertyName = rep('Tits', 3),
                                deemedSavings = rep(0, 3)),
     meterTable = meterDT,
     meterLevel = TRUE)



# Hourly Savings
plot(bList,
     type = 'hourlysave',
     oHours = c(18, 6),
     propertyTable = data.table(meterID = c('I', 'J', 'L'),
                                propertyName = rep('Tits', 3),
                                deemedSavings = rep(0, 3)),
     meterTable = meterDT,
     meterLevel = TRUE)
plot.hourlysave()
