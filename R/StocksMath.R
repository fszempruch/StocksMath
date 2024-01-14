#' Calculate weighted arithmetic mean prices for intraday stocks transactions
#' where volume in weight
#'
#' This function lets you to calculate weighted arithmetic
#' mean prices for intraday stocks transactions for each
#' unique date and hour where volume is weight
#'
#' @param intradayTransactions dataframe with columns: Date, Time, Price, Volume
#' @return dataframe with Date, Time, Price
#' @export


weighted_prices = function(intradayTransactions) {

  intradayTransactions = data.table::data.table(intradayTransactions)
  weighted.mean = function(x) sum(x$Price * x$Volume) / sum(x$Volume)
  result = intradayTransactions[, .(Price = weighted.mean(.SD)), by = .(Date, Time)]

  return(result)
}

#' Logarithmic returns
#'
#' This function lets you to calculate the logarithmic return of investment.
#'
#' @param vector Vector with prices of the company.
#' @return A vector with the logarithmic return of investment.
#' @export

log_returns= function(vector)
{
  roi = 100*log(vector[2:length(vector)]/vector[1:(length(vector)-1)])
  return(roi)
}


#' Simple return of investment
#'
#' This function lets you to calculate the simple return of investment.
#'
#' @param vector Vector with prices of the company.
#' @return A vector with the simple return of investment.
#' @export

simple_returns = function(vector)
{
  roi = (vector[2:length(vector)]-vector[1:(length(vector)-1)])/vector[1:(length(vector)-1)]
  return(roi)
}

#' EWMA volatility- exponentially weighted moving average of volatility
#'
#' This function lets you to calculate EWMA_volatility -
#' exponentially weighted moving average of variance.
#'
#' @param returns Vector with stock returns.
#' @param lambda Lambda parameter
#' @return A vector of calculated EWMA volatility.
#' @export

EWMA_volatility = function(returns, lambda){
  sig = mean(returns^2)
  n = length(returns)+1
  for (i in 2:n) {
    sig[i] = sig[i-1]*lambda + (1-lambda)*(returns[i-1]^2)
  }
  return(sqrt(sig[2:n]))
}

#' Moving Pearson correlation
#'
#' This function lets you to calculate moving Pearson correlation
#' between two series with given time window.
#'
#' @param vector1 Vector of first series.
#' @param vector2 Vector of second series.
#' @param time_window Time window.
#' @return Vector of calculated moving Pearson correlation.
#' @export

moving_pearson = function(vector1, vector2, window){
  df = data.frame(vector1,vector2)
  correlation = rollapply(df, width=window, function(x) cor(x[,1], x[,2]),by.column=FALSE)
  return(correlation)
}


#' Transactional times duraion
#'
#' This function lets you to calculate transactional times duration
#' i.e. the interval between consecutive transactions in seconds,
#' function also can delete overnight TTD
#'
#' @param df dataframe with columns: Date, Time
#' @param delete_overnigh pameter TRUE or FALSE
#' @return dataframe with Date, Time, TTD - "calculated Transactianal times duration"
#' @export

calculate_TTD = function(df, delete_overnight) {

  df$Time = as_datetime(paste(df$Date, df$Time,sep=" "))

  df = df[order(df[,Date], df[,Time])]

  ttd = data.frame(
    Date = df$Date[-1],
    Time = format(as.POSIXct(df$Time[-1]), format = "%H:%M:%S"),
    TTD = abs(as.numeric(df$Time[-1] - df$Time[-nrow(df)], units = "secs"))
  )

  if(delete_overnight == TRUE){
    ttd$Date = with(ttd, ifelse(ttd$Date != lag(ttd$Date), NA, ttd$Time))
    ttd = na.omit(ttd)
  }

  return(ttd)
}

#' Calculate mean Transactional times duraion for given time interval
#'
#' This function lets you to calculate mean transactional times duration
#' for given time interval
#'
#' @param df dataframe with columns: Date, Time, TTD
#' @return dataframe with Date, Time, mean_TTD -
#' "calculated mean Transactianal times duration for each interval"
#' @export

mean_ttd = function(df, period) {
  df = data.table(df)
  df[, Time := round_date(as.POSIXct(Time, format="%H:%M:%S"), period)]
  df[, Time := format(Time, format="%H:%M:%S")]

  result = df[, .(TTD_mean = mean(TTD)), by = .(Time)]

  return(result)
}

