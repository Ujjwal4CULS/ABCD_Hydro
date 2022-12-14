# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Calculates the monthly water balance using ABCD cold region model . 
#' This model work for cold region. 
#' @param para_a describes the amount of runoff and recharge in case the soils are under-saturated. This is dimensionless. This parameter range between 0.8-1 
#' @param para_b  his explains the saturation level of the soils. Unit of this variable is mm. This parameter values range between 20-160
#' @param para_c defines the ratio of groundwater recharge to surface runoff. This is dimensionless.This parameter values range lies between 0.8-1.
#' @param para_d dominants the rate of groundwater discharge. This is dimensionless.This parameter range between 0.001-0.01
#' @param SWS_ini is the solid water storage in the snow cover at the beginning of month. Unit of this variable is mm.
#' @param S_ini is the surface runoff
#' @param G_ini is the initial ground water storage
#' @param p is the precipitation
#' @param PE is the potential evaporation
#' @param temp is monthly temperature
#' @param alpha controlled by negative temperature. This unit is degree Celsius
#' @param beta is influence by negative temperature
#' @param Gmax is the potential maximum ground water storage
#' @return abcd_cr function the output list of  a time-series of water balance components
#' @export
#' @import data.table
#' @examples
#' para_a=.5
#' para_b=200
#' para_c=1
#' para_d=.5
#' SWS_ini=20
#' S_ini=10
#' G_ini=100
#' alpha=0.03
#' beta=.143
#' Gmax=5
#'  m<-c()
#' m$p= c(100:111)
#' m$PE=c(50:61)
#' m$temp=c(-5:6)
#' dt<-data.table::as.data.table(m)
#' dt[, abcd_cr(para_a, para_b, para_c, para_d, SWS_ini, S_ini, G_ini, p, PE, temp, alpha, beta, Gmax)
#'  , by=.(p, PE, temp)]
abcd_cr <- function(para_a, para_b, para_c, para_d, SWS_ini, S_ini, G_ini, p, PE, temp, alpha, beta, Gmax) {
    .Call(`_test_abcd_cr`, para_a, para_b, para_c, para_d, SWS_ini, S_ini, G_ini, p, PE, temp, alpha, beta, Gmax)
}

#' Calculates the monthly water balance using ABCD hydrological model.
#' @param para_a describes the amount of runoff and recharge in case the soils are under-saturated. This is dimensionless. This parameter range between 0-1 
#' @param para_b  his explains the saturation level of the soils. Unit of this variable is mm. This parameter values range between 260 - 1900(Vandewiele et al. 1992)
#' @param para_c defines the ratio of groundwater recharge to surface runoff. This is dimensionless.This parameter values range lies between 0 - 1.
#' @param para_d dominants the rate of groundwater discharge. This is dimensionless.This parameter range between 0 - 1
#' @param S_ini is the surface runoff
#' @param G_ini is the initial ground water storage
#' @param p is the precipitation
#' @param PE is the potential evaporation
#' @return abcd_month_model_cpp function the output list of  a time-series of water balance components
#' A list with the following elements:
#' \itemize{
#' \item SR: Numeric vector with the mo0nthly surface runoff unit mm/month
#' \item ASW: Numeric vector with the Available soil water unit mm/month
#' \item AET: Numeric vector witht the actual evaporation unit mm/month
#' \item SM: Numeric vector with the soil moisture unit mm/month
#' \item GWR: Numeric vector with the ground water recharge mm/month
#' \item GWS: Numeric vector with the Groundwater storage mm/month
#' \item BF: Numeric vector with the base flow mm/month
#' \item TD: Numeric vector with the total recharge mm/month
#' }
#' @export
#' @import data.table
#' @examples
#' para_a=.5
#' para_b=200
#' para_c=1
#' para_d=.5
#' SWS_ini=20
#' S_ini=10
#' G_ini=100
#' alpha=0.03
#' beta=.143
#' Gmax=5
#'  m<-c()
#' m$p= c(100:111)
#' m$PE=c(50:61)
#' m$temp=c(-5:6)
#' dt<-data.table::as.data.table(m)
#' dt[, abcd_month_model_cpp(para_a, para_b, para_c, para_d, S_ini, G_ini, p, PE)
#'  , by=.(p, PE)]
abcd_month_model_cpp <- function(para_a, para_b, para_c, para_d, S_ini, G_ini, p, PE) {
    .Call(`_test_abcd_month_model_cpp`, para_a, para_b, para_c, para_d, S_ini, G_ini, p, PE)
}

#' Calculates the potential evaporation using Oudin method . This method uses the input vector of monthly date time series, temperature and longitude values.
#' @param date  is a monthly time series Date
#' @param tavg  is a monthly mean time series of temperature
#' @param lat  is a vector of longitude
#' @return pet_oudin_cp is monthly potential evaporation with unit mm/day
#' @export
#' @import data.table
#' @examples
#' m<-c()
#' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
#' m$Temp<-c(3.1, 3.5, 5.0, 6.7, 9.3, 12.1, 14.3, 14.1, 11.8, 8.9, 5.5, 3.8)
#' m$lat<-rep(57.1526, 12)
#' dt<-data.table::as.data.table(m)
#' dt[, PET_Hamon:= pet_oudin_cp(Date, Temp, lat)]
#' dt
pet_oudin_cp <- function(date, tavg, lat) {
    .Call(`_test_pet_oudin_cp`, date, tavg, lat)
}

#' Calculates the monthly water balance using ABCD hydrological model.
#' @param para_a describes the amount of runoff and recharge in case the soils are under-saturated. This is dimensionless. This parameter range between 0-1 
#' @param para_b  his explains the saturation level of the soils. Unit of this variable is mm. This parameter values range between 260 - 1900(Vandewiele et al. 1992)
#' @param para_c defines the ratio of groundwater recharge to surface runoff. This is dimensionless.This parameter values range lies between 0 - 1.
#' @param para_d dominants the rate of groundwater discharge. This is dimensionless.This parameter range between 0 - 1
#' @param S_ini is the surface runoff
#' @param G_ini is the initial ground water storage
#' @param p is the precipitation
#' @param PE is the potential evaporation
#' @return abcd_month_model_cpp function the output list of  a time-series of water balance components
#' A list with the following elements:
#' \itemize{
#' \item SR: Numeric vector with the mo0nthly surface runoff unit mm/month
#' \item GW: Numeric vector with the ground water storage with unit mm/month
#' \item AW: Numeric vector with availbale water with unit mm/month
#' \item EO: Numeric vector with the evapotranspiration opportunity mm/month
#' \item SM: Numeric vector with the soil moisture unit mm/month
#' \item AET: Numeric vector witht the actual evaporation unit mm/month
#' }
#' @export
#' @import data.table
#' @examples
#' para_a=.5
#' para_b=200
#' para_c=1
#' para_d=.5
#' SWS_ini=20
#' S_ini=10
#' G_ini=100
#' alpha=0.03
#' beta=.143
#' Gmax=5
#'  m<-c()
#' m$p= c(100:111)
#' m$PE=c(50:61)
#' m$temp=c(-5:6)
#' dt<-data.table::as.data.table(m)
#' dt[, abcd_month_model_cpp(para_a, para_b, para_c, para_d, S_ini, G_ini, p, PE)
#'  , by=.(p, PE)]
abcdseList <- function(para_a, para_b, para_c, para_d, p, PE, S_ini, G_ini) {
    .Call(`_test_abcdseList`, para_a, para_b, para_c, para_d, p, PE, S_ini, G_ini)
}

#' estimate the day using monthly time series of date . 
#' @param date  is a monthly time series Date
#' @return day_cp function the output is the day of date
#' @export
#' @import data.table
#' @examples
#'  m<-c()
#' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
#' dt<-data.table::as.data.table(m)
#' dt[, day:= day_cp(Date)]
#' dt
day_cp <- function(date) {
    .Call(`_test_day_cp`, date)
}

#' estimate the month using date time series data .
#' @param date is a monthly time series Date
#' @return month_cp function the output of the month vector.
#' @export
#' @import data.table
#' @examples
#' m<-c()
#' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
#' dt<-data.table::as.data.table(m)
#' dt[, month:= month_cp(Date)]
#' dt
month_cp <- function(date) {
    .Call(`_test_month_cp`, date)
}

#' Calculates the year using monthly time series of date . 
#' @return year_cp function the output of the year vector.
#' @param date  is a monthly time series Date
#' @export
#' @import data.table
#' @examples
#' m<-c()
#' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
#' dt<-data.table::as.data.table(m)
#' dt[, year:= year_cp(Date)]
#' dt
year_cp <- function(date) {
    .Call(`_test_year_cp`, date)
}

#' Calculates the julian day using monthly time series of date . 
#' @return julianday_cp function the output of julian day vector.
#' @param date  is a monthly time series Date
#' @export
#' @import data.table
#' @examples
#' m<-c()
#' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
#' dt<-data.table::as.data.table(m)
#' dt[, JD:= julianday_cp(Date)]
#' dt
julianday_cp <- function(date) {
    .Call(`_test_julianday_cp`, date)
}

#' Calculates the Annual heat index using monthly time series of temperature . 
#' @return AHI function the output of annual heat index vector.
#' @param date  is a monthly time series Date
#' @param temperature  is a monthly mean time series temperature in degree centigrade
#' @export
#' @import data.table
#' @examples
#' m<-c()
#' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
#' m$temp<-1:12
#' dt<-data.table::as.data.table(m)
#' dt[, AHI:= AHI(Date, temp)]
#' dt
AHI <- function(date, temperature) {
    .Call(`_test_AHI`, date, temperature)
}

#' Calculates the potential evaporation using Thornwaite method . This method uses the input vector of  monthly date time series, temperature and longitude values.
#' @param date  is a monthly time series Date
#' @param tavg  is a monthly mean time series of temperature
#' @param lat  is a vector of longitude
#' @return of pet_thorn_cpp function is monthly potential evaporation in mm/month
#' @export
#' @import data.table
#' @examples
#' m<-c()
#' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
#' m$Temp<-c(3.1, 3.5, 5.0, 6.7, 9.3, 12.1, 14.3, 14.1, 11.8, 8.9, 5.5, 3.8)
#' m$lat<-rep(57.1526, 12)
#' dt<-data.table::as.data.table(m)
#' dt[, PET_Hamon:= pet_thorn_cpp(Date, Temp, lat)]
#' dt
pet_thorn_cpp <- function(date, tavg, lat) {
    .Call(`_test_pet_thorn_cpp`, date, tavg, lat)
}

#' Calculates the potential evaporation using Hamon method . This method uses the input vector of  monthly date time series, temperature and longitude values.
#'
#'
#' @param date  is a monthly time series Date
#' @param tavg  is a monthly mean time series of temperature
#' @param lat  is a vector of longitude
#' @return pet_hamon function estimates the monthly potential evaporation in mm/day
#' @export
#' @examples
#'  m<-c()
#' m$date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
#' m$tavg<-c(3.1, 3.5, 5.0, 6.7, 9.3, 12.1, 14.3, 14.1, 11.8, 8.9, 5.5, 3.8)
#' m$lat<-rep(57.1526, 12)
#'  dt<-data.table::as.data.table(m)
#' dt[, PET_Hamon:= pet_hamon(date, tavg, lat)]
#' dt
pet_hamon <- function(date, tavg, lat) {
    .Call(`_test_pet_hamon`, date, tavg, lat)
}

rcpp_hello_world <- function() {
    .Call(`_test_rcpp_hello_world`)
}

