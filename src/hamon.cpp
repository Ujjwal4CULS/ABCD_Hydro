#include <Rcpp.h>
using namespace Rcpp;
#include <iostream>
#include <string>
#include <math.h>
using namespace std;

//' Calculates the potential evaporation using Hamon method . This method uses the input vector of  monthly date time series, temperature and longitude values.
//'
//'
//' @param date  is a monthly time series Date
//'  @param tavg  is a monthly mean time series of temperature
//'  @param lat  is a vector of longitude
//'  @return is monthly potential evaporation in mm/day
//'  @export
//'  @imports data.table
//' @examples
//'  m<-c()
//' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
//' m$Temp<-c(3.1, 3.5, 5.0, 6.7, 9.3, 12.1, 14.3, 14.1, 11.8, 8.9, 5.5, 3.8)
//' m$lat<-rep(57.1526, 12)
//'  dt<-as.data.table(m)
//' dt[, PET_Hamon:= pet_hamon(Date, Temp, lat)]
//' dt
 // [[Rcpp::export]]
NumericVector pet_hamon(Rcpp::StringVector date, NumericVector tavg,  NumericVector lat)
{
int arrSize = lat.size();
NumericVector out(arrSize);


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
double phi = pi/180 * lat[i];
double delta = 0.409 * sin((2 * pi/365 * JulianDay) - 1.39);
double ws = acos(-tan(phi) * tan(delta));
double ld=(24/pi)*ws;

double eSat = 6.108 * exp(17.27 * tavg[i] / (tavg[i] + 237.3));

double rhoSat = 216.7 * eSat / (tavg[i] + 273.3);
out[i] = 0.1651 * ld/12 * rhoSat;
		
}

return out;	
}


