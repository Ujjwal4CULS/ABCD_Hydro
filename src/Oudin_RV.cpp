#include <Rcpp.h>
using namespace Rcpp;
#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' Calculates the potential evaporation using Oudin method . This method uses the input vector of monthly date time series, temperature and longitude values.
//' @param date  is a monthly time series Date
//' @param tavg  is a monthly mean time series of temperature
//' @param lat  is a vector of longitude
//' @return pet_oudin_cp is monthly potential evaporation with unit mm/day
//' @export
//' @import data.table
//' @examples
//' m<-c()
//' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
//' m$Temp<-c(3.1, 3.5, 5.0, 6.7, 9.3, 12.1, 14.3, 14.1, 11.8, 8.9, 5.5, 3.8)
//' m$lat<-rep(57.1526, 12)
//' dt<-data.table::as.data.table(m)
//' dt[, PET_Hamon:= pet_oudin_cp(Date, Temp, lat)]
//' dt
// [[Rcpp::export]]
NumericVector pet_oudin_cp(Rcpp::StringVector date, NumericVector tavg,  NumericVector lat)
{
	
int arrSize = lat.size();
NumericVector PEt(arrSize);

double phia = lat.size();
NumericVector phi(phia);

double deltaa = lat.size();
NumericVector delta(deltaa);

double wsa = lat.size();
NumericVector ws(wsa);

double dra = lat.size();
NumericVector dr(dra);

double raa = lat.size();
NumericVector ra(raa);




for (int i=0; i<arrSize; i++){
  string dateString="";
  for(int j=0; j<date[i].size(); j++){
    dateString = dateString+date[i][j];	
  }	
  
  std::string year = dateString.substr(0, 4);
  std::string month=dateString.substr(5,2);
  std::string day= dateString.substr(8,10);
  
  int yearInt= std::stoi(year);
  int monthInt= std::stoi(month);
  int dayInt= std::stoi(day);
  
  tm tmDate = {};
  tmDate.tm_year = yearInt - 1900;
  tmDate.tm_mon = monthInt - 1;
  tmDate.tm_mday = dayInt;
  mktime( &tmDate );
  int dayNumber = tmDate.tm_yday;
  
  int JulianDay= dayNumber+15;
double pi = 3.141593;
 phi[i] = pi/180 * lat[i];
 delta[i] = 0.409 * sin((2 * pi/365 * JulianDay) - 1.39);
 ws[i] = acos(-tan(phi[i]) * tan(delta[i]));
 dr[i] = 1 + 0.033 * cos(2 * pi/365 * JulianDay);
 ra[i] = (24 * 60)/pi * 0.0820 * dr[i] * (ws[i] * sin(lat[i]*pi/180) * sin(delta[i]) + cos(lat[i]*pi/180) * cos(delta[i]) * sin(ws[i]));
 if ((tavg[i] + 5.0) >= 0.0 ) {
    PEt[i] = 0.408 * ra[i] * (tavg[i] + 5) / 100;
 }
 else if (tavg[i] == NA) {
    PEt[i] = NA_REAL;
 }
  else {
     PEt[i] = 0.0;
  }

  
}
return PEt;
}
