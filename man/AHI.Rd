% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{AHI}
\alias{AHI}
\title{Calculates the Annual heat index using monthly time series of temperature .}
\usage{
AHI(date, temperature)
}
\arguments{
\item{date}{is a monthly time series Date}

\item{temperature}{is a monthly mean time series temperature in degree centigrade}
}
\value{
AHI function the output of annual heat index vector.
}
\description{
Calculates the Annual heat index using monthly time series of temperature .
}
\examples{
m<-c()
m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
m$temp<-1:12
dt<-data.table::as.data.table(m)
dt[, AHI:= AHI(Date, temp)]
dt
}
