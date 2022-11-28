#include<Rcpp.h>
using namespace Rcpp;
#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' Calculates the monthly water balance using ABCD hydrological model.
//' @param para_a describes the amount of runoff and recharge in case the soils are under-saturated. This is dimensionless. This parameter range between 0-1 
//' @param para_b  his explains the saturation level of the soils. Unit of this variable is mm. This parameter values range between 260 - 1900(Vandewiele et al. 1992)
//' @param para_c defines the ratio of groundwater recharge to surface runoff. This is dimensionless.This parameter values range lies between 0 - 1.
//' @param para_d dominants the rate of groundwater discharge. This is dimensionless.This parameter range between 0 - 1
//' @param S_ini is the surface runoff
//' @param G_ini is the initial ground water storage
//' @param p is the precipitation
//' @param PE is the potential evaporation
//' @return abcd_month_model_cpp function the output list of  a time-series of water balance components
//' A list with the following elements:
//' \itemize{
//' \item SR: Numeric vector with the mo0nthly surface runoff unit mm/month
//' \item GW: Numeric vector with the ground water storage with unit mm/month
//' \item AW: Numeric vector with availbale water with unit mm/month
//' \item EO: Numeric vector with the evapotranspiration opportunity mm/month
//' \item SM: Numeric vector with the soil moisture unit mm/month
//' \item AET: Numeric vector witht the actual evaporation unit mm/month
//' }
//' @export
//' @import data.table
//' @examples
//' para_a=.5
//' para_b=200
//' para_c=1
//' para_d=.5
//' SWS_ini=20
//' S_ini=10
//' G_ini=100
//' alpha=0.03
//' beta=.143
//' Gmax=5
//'  m<-c()
//' m$p= c(100:111)
//' m$PE=c(50:61)
//' m$temp=c(-5:6)
//' dt<-data.table::as.data.table(m)
//' dt[, abcd_month_model_cpp(para_a, para_b, para_c, para_d, S_ini, G_ini, p, PE)
//'  , by=.(p, PE)]
// [[Rcpp::export]]
List abcdseList(double para_a, double para_b, double para_c, double para_d,  NumericVector p, NumericVector PE, double S_ini, double G_ini) {
  // Calibration period length
  
  List res(6);
  int arrSize = PE.size();
  //   para_a = 0.98;
  //	para_b = 350;
  //para_c = 0.3;
  //	para_d =  1;
  //int W[] = {};
  //double W = p.size();   //available water
  //double Y= p.size();  //evapotranspiration opportunity
  // double S= p.size();  
  //double E= p.size();  //actual evaporation
  //double G= p.size();  //groundwater storage
  
  
  //double tn= p.size();
  //NumericVector Qest(tn);
  
  
  double t = p.size();
  NumericVector W(t);  //available water
  
  double cx = p.size();
  NumericVector Y(cx);  //evapotranspiration opportunity
  
  double gg = p.size();
  NumericVector S(gg);  //soil moisture
  
  double ex = p.size();
  NumericVector E(ex);  //actual evaporation
  
  double mm = p.size();
  NumericVector G(mm);   //groundwater storage
  
  double n = p.size();
  NumericVector Qest(n);  // estimated surface runoff
  
  
  double ax = p.size();
  NumericVector w1(ax); // intermediate value used to calculate Y
  
  double bx = p.size();
  NumericVector w2(bx);  // intermediate value used to calculate Y
  
 
  
  
  
  
  for (int i=0; i<arrSize; i++) {
    //int W = 0;
    if(i == 1){
      W[i] = p[i] + S_ini; //available water
    } 
    else{
      W[i]= p[i] + S[i - 1]; //available water
    }
    //return W;
    //}
    
    //for (int i=0; i<arrSize; i++) {
    // w1 and w2 are intermediate values used to calculate Y
    w1[i] = (W[i] + para_b)/(2 * para_a);
    w2[i] = W[i] * para_b/para_a;
    
    Y[i] = w1[i]- sqrt(w1[i]*w1[i]- w2[i]);  //evapotranspiration opportunity
    S[i] = Y[i] * exp(-1 * PE[i]/para_b);  //soil moisture
    E[i] = Y[i] * (1 - exp(-1 * (PE[i]/para_b)));  //actual evaporation
    
    if(i == 1) {
      G[i] =  (G_ini + para_c *  (round((W[i] - Y[i])*100)/100)/(1 +para_d));
    }else{
      G[i] =  (G[i - 1] + para_c * (round((W[i] - Y[i])*100)/100)/(1 + para_d));
    }
    
    Qest[i] = (1 - para_c) *(round((W[i] - Y[i])*100)/100) + para_d * G[i];
    
  }
  
  res[0] = Qest;
  res[1] = G;
  res[2]= W;
  res[3]= Y;
  res[4]= S;
  res[5]= E;
  
 res = List::create( Named("SR") = Qest , Named("GW") = G, Named("AW") = W , Named("EO") = Y, Named("SM") =S , Named("AET") = E);
  // Coerce to a data.frame
  //res.attr("class") = "data.frame";
  //res.attr("row.names") = Rcpp::seq(1, res.size());
  return res;
  
  
  
  
  //return res;
} // namespace Rcpp
