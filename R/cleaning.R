#' Convert Raw Utility Data to Long-form
#' @import data.table
#' @export
ebRawConvert <- function(dat, utility = 'sdge', id = c('account', 'meter'))
{
  id = match.arg(id)
  if(utility == 'sdge'){
    id <- c('account' = 'ACCT_NBR', 'meter' = 'ID_MTR')[id]
    keep <- c('INTRVL_DATE', id, 'CHNL_ID', grep('KWH', names(useDT), value = TRUE))
    dat <- useDT[, .SD, .SDcols = keep]
    setnames(dat, c(id, 'INTRVL_DATE', 'CHNL_ID'), c('meterID', 'day', 'channel'))
    numV <- setdiff(names(dat), c('meterID', 'day', 'channel'))
    dat[, (numV):= lapply(.SD, as.numeric), .SDcols = numV]
    dat <- melt(dat,
                id.vars = c('meterID', 'channel', 'day'), 
                variable = 'hr',
                value.name = 'use',
                variable.factor = FALSE)
    hourDict <- unique(dat$hr); names(hourDict) <- hourDict
    hourDict <- vapply(hourDict, 
                       function(x) as.numeric(regmatches(x, regexpr('\\d+', x, perl = TRUE))) - 1,
                       FUN.VALUE = numeric(1))
    dat[, hr:= hourDict[hr]]
    dateDT <- unique(dat[, .(day, hr)])[, .(day, 
                                            hr, 
                                            date = as.POSIXct(paste0(as.Date(day, tz = 'UTC'), ' ', hr),
                                                              tz = 'UTC', 
                                                              format = '%Y-%m-%d %H'))]
    dat <- merge(dat,
                 dateDT,
                 by = c('day', 'hr'))[, .(meterID, channel, date, use)]
  }
  dat
}

#' Fill Gaps in Date-range of Table
#' @import data.table
#' @export
ebGapFill <- function(dat, id_var = 'meterID')
{
  dat <- unique(na.omit(dat))
  merge(
    dat,
    data.table(id = unique(dat[[id_var]]),
               date = seq.POSIXt(from = min(dat$date), to = max(dat$date), by = 'hour')),
    all.y = TRUE,
    by.x = c('date', id_var), by.y = c('date', 'id'))
}

#' QC Check on Use Data
#' @import data.table
#' @export
ebQC <- function(dataList)
{
  dupCountDict = sapply(dataList, function(dat) sum(duplicated(dat)))
  naPctDict = sapply(dataList, function(dat) round(100 * dat[, sum(is.na(use)) / .N], 2))
  
  dat <- data.table(meterID = names(dataList),
                    dup_count = dupCountDict[names(dataList)],
                    na_pct = naPctDict[names(dataList)])
  cat(uniqueN(dat[na_pct > .5, ]), 'meters with 0.5%+ NA values \n')
  cat(uniqueN(dat[na_pct > 1, ]), 'meters with 1%+ NA values \n')
  cat(uniqueN(dat[dup_count > 0, ]), 'meters with duplicate entries \n')
  dat
}

#' QC Check on Use Data
#' @import data.table
#' @export
ebImpute <- function(dat, value = 'use', method = c('hour_month'))
{
  if(method == 'hour_month') dat[, imputed:= mean(get(value), na.rm = TRUE),
                                 by = .(month(date), hour(date))]
  dat[is.na(get(value)), (value):= imputed]; dat[, imputed:= NULL]
  dat
}

#' Get Nearest Weather Station
#' @import data.table
#' @import rnoaa
#' @export
ebGetStation <-  function(x, y, start.date = NULL, end.date = NULL, include = NULL, exclude = NULL)
{
  out <- isd_stations_search(lon = x, lat = y, radius = 200)
  out <- as.data.table(out)[, .(usaf = usaf,
                                wban = wban,
                                station = paste0(usaf, '-', wban),
                                begin = as.Date(as.character(begin), tz = 'UTC', format = '%Y%m%d'),
                                end = as.Date(as.character(end), tz = 'UTC', format = '%Y%m%d'),
                                distance = distance)]
  if(!is.null(exclude)) out <- out[!(station %in% exclude), ]
  if(!is.null(include)) out <- out[usaf %in% include]
  if(!is.null(start.date)) out <- out[begin <= start.date]
  if(!is.null(end.date)) out <- out[end >= end.date]
  out[which.min(distance), station]
  
}

#' Clean NOAA Temp Helper
#' @import data.table
#' @export
translate_temp <- Vectorize(FUN = function(x){
  out <- regmatches(x, regexpr('\\d+', x, perl = TRUE))
  if(length(out) < 1) out <- NA
  out <- 0.1 * as.numeric(out) * (9 / 5) + 32
  return(out)
})

#' Get NOAA Hourly Weather
#' @import data.table
#' @import rnoaa
#' @export
ebWeather = function(station, start_year, end_year = year(Sys.time()), time_zone = 'PST')
{
  station = strsplit(station, '-')[[1]]
  tryCatch({
    dat <- rbindlist(
      lapply(start_year:end_year, function(year){
        as.data.table(isd(station[1], station[2], year = year))
      }),
      fill = TRUE
    )
    dat = dat[, .(date = as.POSIXct(paste0(date, ' ', as.numeric(substr(time, 1, 2))),
                                    tz = 'UTC', 
                                    format = '%Y%m%d %H'),
                  temperature = temperature,
                  temperature_quality = temperature_quality)]
    dat[temperature == '+9999', temperature:= NA]
    dat[temperature_quality %in% c('2', '3', '6', '7'), temperature:= NA]
    dat[, temperature:= translate_temp(temperature)]
    dat <- dat[, .(station = paste0(station, collapse = '-'),
                   temp = mean(temperature, na.rm = TRUE)),
               by = .(date)]
    if(time_zone == 'PST') dat[, date:= date - 7 * (60 * 60)] #UTC offset
    dat <- ebGapFill(dat, id_var = 'station')
    msng <- sum(is.na(dat$temp))
    cat(msng, sprintf('(%s pct)', round(100 * msng / length(dat$temp), 1)),
        'missing values for station',
        paste0(station, collapse = '-'),
        '\n')
    ebImpute(dat, value = 'temp')
  }, error = function(e) cat(paste0(station, collapse = '-'), 'Not Available \n'))
}