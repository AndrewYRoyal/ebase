lapply(c('data.table', 'dplyr', 'lubridate', 'lfe', 'ggplot2'), library, character.only = TRUE)
rm(list = ls())
source('R/mainFunctions.R')
source('R/helperFunctions.R')

#devtools::use_vignette('ebase_vignette')
sampDT <- fread('sampleData.csv')[meterID %in% c('I', 'L', 'J')]
setnames(sampDT, names(sampDT), c('meter', 'date', 'hour', 'electricity', 'temperature', 'install_date'))
sampDT[meter == 'I', install_date:= '10/30/2017']
sampDT[meter == 'L', install_date:= '3/27/2017']
sampDT[meter == 'J', install_date:= '4/15/2017']


sampDT[, c('date2', 'install_date2'):= list(as.POSIXct(date, format = '%m/%d/%Y'),
                                            as.POSIXct(install_date, format = '%m/%d/%Y'))]
sampDT[meter == 'I' & (date2 >= install_date2), electricity:= electricity*.9]
sampDT[meter == 'L' & (date2 >= install_date2), electricity:= electricity*.9]
sampDT[meter == 'J' & (date2 >= install_date2), electricity:= electricity*.9]

# Cut down data to make more readable
sampDT <- sampDT[date2 < as.POSIXct('12/30/2017', format = '%m/%d/%Y')]

# Non-routine adjustment
sampDT[meter == 'J' &
         (date2 >= as.POSIXct('2/1/2017', format = '%m/%d/%Y') &
            date2 < as.POSIXct('7/1/2017', format = '%m/%d/%Y')),
       electricity:= electricity - 500]

# Export
sampDT[, c('date2', 'install_date2'):= NULL]
fwrite(sampDT, 'vignettes/vignetteData.csv')


###########################################################################
bList <- get_baselineModel(dt = sampDT,
                           id.var = meter,
                           date.var = date,
                           use.var = electricity,
                           hour.var = hour,
                           temp.var = temperature,
                           install.date.var = install_date,
                           base.length = 15)

x = print_loadProfile(bList$predictions, print = TRUE)
x$J

sampDT[meter == 'J', nraStart:= '2/1/2017']
sampDT[meter == 'J', nraEnd:= '7/1/2017']


bList <- get_baselineModel(dt = sampDT,
                           id.var = meter,
                           date.var = date,
                           use.var = electricity,
                           hour.var = hour,
                           temp.var = temperature,
                           install.date.var = install_date,
                           nra.start.var = nraStart,
                           nra.end.var = nraEnd,
                           base.length = 15)

x = print_loadProfile(bList$predictions, print = TRUE)
x$J

# Demonstration
totalPredicted <- sum(bList$predictions[meterID == 'L' & postECM == 1, pElct])
totalSaved <- sum(bList$predictions[meterID == 'L' & postECM == 1, elct - pElct])
percentSaved <- 100*totalSaved/totalPredicted

totalSaved
percentSaved
