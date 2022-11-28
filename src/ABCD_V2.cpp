#include<Rcpp.h>
using namespace Rcpp;
#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' Calculates the monthly water balance using ABCD cold region model . 
//'
//'
//' @param date  is a monthly time series Date
//'  @export
//'  @import data.table
//' @examples
//' para_a=.5
//' para_b=200
//' para_c=1
//' para_d=.5
//' S_ini=10
//' G_ini=100
//'  m<-c()
//' m$Date<-seq(as.Date("2014/1/1"), by="month" ,length.out =12)
//' m$p= c(100:111)
//' m$PE=c(50:61)
//' m$temp=c(-5:6)
//' dt<-as.data.table(m)
//' dt[, ABCD_mm:= abcd_month_model_cpp(para_a, para_b, para_c, para_d, S_ini, G_ini, p, PE)]
//' dt

// [[Rcpp::export]]
List abcd_month_model_cpp(double para_a, double para_b, double para_c, double para_d, double S_ini, double G_ini, NumericVector p, NumericVector PE ) {
  // Calibration period length
  
  List res(8);
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

  
  
  double Qta = p.size();
  NumericVector Qt(Qta);  //Numeric vector with the yearly discharges
  
  double Wta = p.size();
  NumericVector Wt(Wta);  //Numeric vector witht the available soil water
  
  double Sta = p.size();
  NumericVector St(Sta);  //Numeric vector with the surface runnoff
  
  double Rta = p.size();
  NumericVector Rt(Rta);
  
  double Gta = p.size();
  NumericVector Gt(Gta);  //actual evaporation
  
 
  double Qbta = p.size();
  NumericVector Qbt(Qbta);   //groundwater storage
  
  
  
  double Eta = p.size();
  NumericVector Et(Eta);  // estimated surface runoff
  
  double Wa = p.size();
  NumericVector W(Wa);
  
  double yya = p.size();
  NumericVector yy(yya);
  
  double Ea = p.size();
  NumericVector E(Ea);
  
  double Sa = p.size();
  NumericVector S(Sa);
  
  double Qda = p.size();
  NumericVector Qd(Qda);
  
  double Qdta = p.size();
  NumericVector Qdt(Qdta);
  
  double Ga = p.size();
  NumericVector G(Ga);
  
  double Qa = p.size();
  NumericVector Q(Qa);
  
  double Qba = p.size();
  NumericVector Qb(Qba);
  
  double Ra = p.size();
  NumericVector R(Ra);
  
 
  
  
  
  
  for (int i=0; i<arrSize; i++) {
    //Available soil water
    W[i] = p[i]+S_ini;
    Wt[i] = W[i];
    //Evapotranspiration potential
    yy[i] = (W[i]+para_b)/(2*para_a)-(pow((W[i]+para_b)/(2*para_a), 2))- pow((W[i]*para_b/para_a), 0.5);
    //Potential evapotranspiration
    E[i] = yy[i]*(1-exp(-PE[i]/para_b));
    Et[i] = E[i];
    //Soil moisture
    S[i] = yy[i]- E[i];
    St[i] = S[i];
    //Runoff
    Qd[i] = (1-para_c)*(W[i]-yy[i]);
    Qdt[i] = Qd[i];
    //GW Recharge
    R[i] = para_c*(W[i]-yy[i]);
    
    //R[i]= (1-para_c)*(W[i]-yy[i]);
    //Rt[i] = R[i];
    // Groundwater storage
    G[i] = (1/(1+para_d))*(R[i]+G_ini);
    Gt[i] = G[i];
    // Base flow
    Qb[i] = para_d*G[i];
    Qbt[i] = Qb[i];
    // Total discharge
    Q[i] = Qb[i]+Qd[i];
    //G_ini[i] = G[i];
    //S_ini[i] = S[i];
    Qt[i] = Q[i];
  }
     res[0] = Qd;
  res[1] = W;
  res[2]= E;
  res[3]= S;
  res[4]= R;
  res[5]= G;
  res[6]= Qb;
  res[7]= Q;
  
 res = List::create(Named("SR")=Qd, Named("ASW")=W, Named("AET") = E , Named("SM") = S, Named("GWR") = R , Named("GWS") = G, Named("BF") =Qb , Named("TD") = Q);
  // Coerce to a data.frame
  //res.attr("class") = "data.frame";
  //res.attr("row.names") = Rcpp::seq(1, res.size());
  return res; 
  }
  