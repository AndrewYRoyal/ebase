lapply(c('data.table', 'dplyr', 'lubridate', 'lfe', 'ggplot2'), library, character.only = TRUE)
library('xgboost')
rm(list = ls())
source('R/mainFunctions.R')
source('R/helperFunctions.R')

useDT <- fread('vignettes/vignetteData.csv')
useDT <- get_dataFormat(dt = useDT, id.var = meter,
                        date.var = date,
                        use.var = electricity,
                        hour.var = hour,
                        temp.var = temperature,
                        install.date.var = install_date,
                        base.length = 12)

get_mlPredict <- function(dt, d){
  dt <- dt[dHour == d, c('baseline', 'yr', 'yday', 'mm', 'dHour', 'elct', 'yoy', 'nra', 'temp')]
  if(anyNA(dt[, nra])) dt[, nra:= NULL]
  if(anyNA(dt[, yoy])) dt[, yoy:= NULL]

  baselineM <- xgb.DMatrix(
    data = sparse.model.matrix(elct ~ ., data = dt[baseline == 1, -c('yr', 'yday', 'dHour')])[, -1],
    label = dt[baseline == 1, elct]
  )
  postM <- xgb.DMatrix(
    data = sparse.model.matrix(elct ~ ., data = dt[baseline == 0, -c('yr', 'yday', 'dHour')])[, -1],
    label = dt[baseline == 0, elct]
  )

  cb.cv.predict(save_models = TRUE)
  nIterate <- xgb.cv(data = baselineM, nfold = 10,
                     max_depth = 4, eta = .5, nthread = 8, nrounds = 2000,
                     early_stopping_rounds = 5, objective = 'reg:linear', verbose = 0)$best_iteration
  gbModel <- xgboost(data = baselineM,
                     max_depth = 4, eta = .5, nthread = 8, nrounds = nIterate,
                     early_stopping_rounds = 5, objective = 'reg:linear', verbose = 0)
  dt[baseline == 1, mlElct:= predict(gbModel, baselineM)]
  dt[baseline == 0, mlElct:= predict(gbModel, postM)]
  dt[, dHour:= d]
  return(dt[, .(yr, yday, dHour, mlElct)])
}


start <- Sys.time()
predDT <- rbindlist(lapply(unique(useDT[, dHour]), function(x) get_mlPredict(dt = useDT, d = x)))
Sys.time() - start

useDT2 <- fread('vignettes/vignetteData.csv')
bList <- baselineModel(dt = useDT2, id.var = meter,
                       date.var = date,
                       use.var = electricity,
                       hour.var = hour,
                       temp.var = temperature,
                       install.date.var = install_date,
                       base.length = 12)

predDT <- merge(predDT, bList$predictions, by = c('yr', 'yday', 'dHour'),
                all.x = TRUE, all.y = TRUE)

#================================================
# Generate residual plots
plotDT <- predDT[, lapply(c(mlElct = 'mlElct', pElct = 'pElct', elct = 'elct'),
                          function(x) mean(get(x), na.rm = TRUE)),
                 by = c('meterID', 'yr', 'yday', 'baseline')]

plotDT[, c('mlRes', 'pRes'):= list(elct - mlElct, elct - pElct)]
plotDT <- melt(plotDT, id.vars = c('meterID', 'yr', 'yday', 'baseline', 'elct'),
               measure.vars = c('mlRes', 'pRes'))
plotDT <- plotDT[order(meterID, yr, yday)]
plotDT[, days:= .GRP, by = c('meterID', 'yr', 'yday')]
plotDT[, minDay:= min(days), by = 'meterID']
plotDT[, days:= days - minDay]

require('ggthemes')
ggplot(data = plotDT[meterID == 'L', ],
       aes(x = days, y = value, color = variable, linetype = as.factor(baseline))) +
  theme_gdocs(base_size = 25) +
  geom_line() +
  scale_linetype_manual('Period', values = c(2, 1), labels = c('Post-ECM', 'Baseline')) +
  scale_color_manual('Prediction Type', values = c('red', 'royalblue'), labels = c('GBoost', 'Linear FE')) +
  scale_x_continuous('Day') +
  scale_y_continuous('Residual')
ggsave('plots/pCompareL.pdf', width = 20, height = 8)

ggplot(data = plotDT[meterID == 'I', ],
       aes(x = days, y = value, color = variable, linetype = as.factor(baseline))) +
  theme_gdocs(base_size = 25) +
  geom_line() +
  scale_linetype_manual('Period', values = c(2, 1), labels = c('Post-ECM', 'Baseline')) +
  scale_color_manual('Prediction Type', values = c('red', 'royalblue'), labels = c('GBoost', 'Linear FE')) +
  scale_x_continuous('Day') +
  scale_y_continuous('Residual')
ggsave('plots/pCompareI.pdf', width = 20, height = 8)


#================================================
# Generate performance table
require('stargazer')
meterV <- c(L = 'L', I = 'I')

rmseMLV <- sapply(meterV,
                  function(r) mean(predDT[baseline == 1 & meterID == r, abs(elct - mlElct)]))
rmseLFEV <- sapply(meterV,
                   function(r) mean(predDT[baseline == 1 & meterID == r, abs(elct - pElct)]))

nmbeMLV <- sapply(meterV,
                  function(r) sum(predDT[baseline == 1 & meterID == r, elct - mlElct])/
                    sum(predDT[baseline == 1 & meterID == r, elct]))
nmbeLFEV <- sapply(meterV,
                   function(r) sum(predDT[baseline == 1 & meterID == r, elct - pElct])/
                     sum(predDT[baseline == 1 & meterID == r, elct]))
savMLV <- sapply(meterV,
                 function(r) sum(predDT[postECM == 1 & meterID == r, (elct - mlElct)])/
                   sum(predDT[postECM == 1 & meterID == r, mlElct]))
savLFEV <- sapply(meterV,
                  function(r) sum(predDT[postECM == 1 & meterID == r, (elct - pElct)])/
                    sum(predDT[postECM == 1 & meterID == r, pElct]))

tabM <- matrix(c(rmseLFEV, rmseMLV, nmbeLFEV, nmbeMLV, savLFEV, savMLV, 0.1, 0.1), nrow = 2)
rownames(tabM) <- paste0('Meter ', meterV)
colnames(tabM) <- c('Linear FE', 'GBoost', 'Linear FE', 'GBoost', 'Linear FE', 'GBoost', 'Actual Savings')

stargazer::stargazer(tabM,
                     summary = FALSE,
                     type = 'latex',
                     out = 'plots/compare.tex',
                     digits = 3)




### Linear baseline model achieves ~ 170 error in baseline for meter J

mean(predDT[baseline == 1 & meterID == 'L', abs(elct - pElct)])

mean(predDT[baseline == 1 & meterID == 'I', abs(elct - mlElct)])
mean(predDT[baseline == 1 & meterID == 'I', abs(elct - pElct)])



# Estimated savings
sum(predDT[postECM == 1 & meterID == 'L', (elct - mlElct)])/sum(predDT[postECM == 1 & meterID == 'L', mlElct])
sum(predDT[postECM == 1 & meterID == 'L', (elct - pElct)])/sum(predDT[postECM == 1 & meterID == 'L', pElct])

sum(predDT[postECM == 1 & meterID == 'I', (elct - mlElct)])/sum(predDT[postECM == 1 & meterID == 'I', mlElct])
sum(predDT[postECM == 1 & meterID == 'I', (elct - pElct)])/sum(predDT[postECM == 1 & meterID == 'I', pElct])

