#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' estimate the day using monthly time series of date . 
//' @param date  is a monthly time series Date
//' @return day_cp function the output is the day of date
//' @export
//' @import data.table
//' @examples
//'  m<-c()
//' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
//' dt<-data.table::as.data.table(m)
//' dt[, day:= day_cp(Date)]
//' dt
// [[Rcpp::export]]
NumericVector day_cp(Rcpp::StringVector date){
int arrSize = date.size();
NumericVector dayInt(arrSize);




for (int i=0; i<arrSize; i++){
	string dateString="";
for(int j=0; j<date[i].size(); j++){
dateString = dateString+date[i][j];	
}	
std::string day= dateString.substr(8,10);
 dayInt[i]= std::stoi(day);

}

return dayInt;

}

#include <Rcpp.h>
using namespace Rcpp;
#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' estimate the month using date time series data .
//' @param date is a monthly time series Date
//' @return month_cp function the output of the month vector.
//' @export
//' @import data.table
//' @examples
//' m<-c()
//' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
//' dt<-data.table::as.data.table(m)
//' dt[, month:= month_cp(Date)]
//' dt
// [[Rcpp::export]]
NumericVector month_cp(Rcpp::StringVector date){
int arrSize = date.size();
NumericVector monthInt(arrSize);


for (int i=0; i<arrSize; i++){
	string dateString="";
for(int j=0; j<date[i].size(); j++){
dateString = dateString+date[i][j];	
}	


std::string month=dateString.substr(5,2);

monthInt[i]= std::stoi(month);

}

return monthInt;

}

	
#include <Rcpp.h>
	using namespace Rcpp;
	
#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' Calculates the year using monthly time series of date . 
//' @return year_cp function the output of the year vector.
//' @param date  is a monthly time series Date
//' @export
//' @import data.table
//' @examples
//' m<-c()
//' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
//' dt<-data.table::as.data.table(m)
//' dt[, year:= year_cp(Date)]
//' dt
// [[Rcpp::export]]
NumericVector year_cp(Rcpp::StringVector date){
int arrSize = date.size();
NumericVector yearInt(arrSize);

for (int i=0; i<arrSize; i++){
	string dateString="";
for(int j=0; j<date[i].size(); j++){
dateString = dateString+date[i][j];	
}	

std::string year = dateString.substr(0, 4);


yearInt[i]= std::stoi(year);

}
return yearInt;

}
#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' Calculates the julian day using monthly time series of date . 
//' @return julianday_cp function the output of julian day vector.
//' @param date  is a monthly time series Date
//' @export
//' @import data.table
//' @examples
//' m<-c()
//' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
//' dt<-data.table::as.data.table(m)
//' dt[, JD:= julianday_cp(Date)]
//' dt
// [[Rcpp::export]]
NumericVector julianday_cp(Rcpp::StringVector date){
int arrSize = date.size();
NumericVector JulianDay(arrSize);




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

JulianDay[i]= dayNumber+15;
}
return JulianDay;
	}
	

	
	
#include <Rcpp.h>
using namespace Rcpp;	
#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' Calculates the Annual heat index using monthly time series of temperature . 
//' @return AHI function the output of annual heat index vector.
//' @param date  is a monthly time series Date
//' @param temperature  is a monthly mean time series temperature in degree centigrade
//' @export
//' @import data.table
//' @examples
//' m<-c()
//' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
//' m$temp<-1:12
//' dt<-data.table::as.data.table(m)
//' dt[, AHI:= AHI(Date, temp)]
//' dt
// [[Rcpp::export]]
	NumericVector AHI(Rcpp::StringVector date, NumericVector temperature){
	  int oldma = date.size();
	  NumericVector years(oldma);
	 years=year_cp(date);
	  int outs = date.size();
	  NumericVector output(outs);
	 
	  double totalYearAvgTemp=0;
	  int currentIndex=0;
	  for(int i=0;i<years.size();i++){
	    totalYearAvgTemp=temperature[i];
	    for( int j=i+1;j<years.size();j++){
	      currentIndex=j;
	      if(years[i]==years[j]){
	        totalYearAvgTemp=totalYearAvgTemp+temperature[j];
	      } else{
	        break;
	      }
	      
	    }
	    
	    if(currentIndex<output.size()){
	      for(int k=i;k<output.size();k++){
	        output[k]=totalYearAvgTemp;
	      }
	      i=currentIndex-1;  
	      currentIndex=currentIndex+1;
	    }
	  }
	  
	  return output;
	 
	}
	
	#include <Rcpp.h>
using namespace Rcpp;
#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' Calculates the potential evaporation using Thornwaite method . This method uses the input vector of  monthly date time series, temperature and longitude values.
//' @param date  is a monthly time series Date
//' @param tavg  is a monthly mean time series of temperature
//' @param lat  is a vector of longitude
//' @return of pet_thorn_cpp function is monthly potential evaporation in mm/month
//' @export
//' @import data.table
//' @examples
//' m<-c()
//' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
//' m$Temp<-c(3.1, 3.5, 5.0, 6.7, 9.3, 12.1, 14.3, 14.1, 11.8, 8.9, 5.5, 3.8)
//' m$lat<-rep(57.1526, 12)
//' dt<-data.table::as.data.table(m)
//' dt[, PET_Hamon:= pet_thorn_cpp(Date, Temp, lat)]
//' dt
// [[Rcpp::export]]
NumericVector pet_thorn_cpp(Rcpp::StringVector date, NumericVector tavg,  NumericVector lat)
{
int arrSize = lat.size();
NumericVector out(arrSize);

double xy = tavg.size();
  NumericVector t(xy); 
  
double xyz = lat.size();
  NumericVector phi(xyz); 
  
  int xa = date.size();
  NumericVector julianDay(xa); 
  
  int da = date.size();
  NumericVector d(da);
  
  int xb = date.size();
  NumericVector delta(xb);
  
  int xc = date.size();
  NumericVector ws(xc);
  
   double xd = date.size();
  NumericVector N(xd);

//double xe = tavg.size();
  //NumericVector t(xe);
  
 double xf = date.size();
  NumericVector alpha(xf); 
  
  int taa = date.size();
  NumericVector ta(taa);
  
   double IA = tavg.size();
  NumericVector I(IA);
  
  double xg = date.size();
  NumericVector pet_nc(xg);

double xh = date.size();
  NumericVector petDaily(xh);

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
 ws[i]= acos(-tan(phi[i]) * tan(delta[i]));
 N[i]=(24/pi)*ws[i];
t[i]= pow((tavg[i]/5),1.514);
}
I= AHI(date, t);

for (int i=0; i<arrSize; i++){
	//int d = 30;
	ta = month_cp(date);
	if( ta[i]==1){
		d[i] = 31;
	}else if(ta[i]==2){
		 d[i] = 28;
	}else if(ta[i]==3){
	 d[i] = 31;
	}else if(ta[i]==4){
		 d[i] = 30;
	}else if(ta[i]==5){
		 d[i] = 31;
	}else if(ta[i]==6){
		 d[i] = 30;
	}else if(ta[i]==7){
		 d[i] = 31;
	}else if(ta[i]==8){
		 d[i] = 31;
	}else if(ta[i]==9){
		 d[i] = 30;
	}else if(ta[i]==10){
		 d[i] = 31;
	}else if(ta[i]==11){
		 d[i] = 30;
	}else if(ta[i]==12){
		 d[i] = 31;
	}
alpha[i] =  (0.000000675*I[i]*I[i]*I[i]) - (0.0000771*I[i]*I[i]) + 0.01792*I[i] + 0.49239;
pet_nc[i] = 16*(pow((10*tavg[i]/I[i]), alpha[i]));

petDaily[i] = pet_nc[i]*(d[i]/30) * (N[i]/12);
petDaily[i] = round(petDaily[i]*100)/100;


}
return petDaily;

}
	
	
	