% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trading_days.R
\name{get_trading_range}
\alias{get_trading_range}
\title{Get trading range}
\usage{
get_trading_range(start_date, end_date, unit, lookback = Inf)
}
\arguments{
\item{start_date}{Date or character of start date by "yyyy-mm-dd" format}

\item{end_date}{Date or character of end date by "yyyy-mm-dd" format}

\item{unit}{Floor date unit by "day", "week", "month" or "year"}

\item{lookback}{Lookback period by unit size}
}
\value{
data.frame of start and end date
}
\description{
Get trading range
}
