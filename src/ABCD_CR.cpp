#include<Rcpp.h>
using namespace Rcpp;
#include <iostream>
#include <string>
#include <math.h>
using namespace std;
//' Calculates the monthly water balance using ABCD cold region model . 
//' This model work for cold region. 
//' @param para_a describes the amount of runoff and recharge in case the soils are under-saturated. This is dimensionless. This parameter range between 0.8-1 
//' @param para_b  his explains the saturation level of the soils. Unit of this variable is mm. This parameter values range between 20-160
//' @param para_c defines the ratio of groundwater recharge to surface runoff. This is dimensionless.This parameter values range lies between 0.8-1.
//' @param para_d dominants the rate of groundwater discharge. This is dimensionless.This parameter range between 0.001-0.01
//' @param SWS_ini is the solid water storage in the snow cover at the beginning of month. Unit of this variable is mm.
//' @param S_ini is the surface runoff
//' @param G_ini is the initial ground water storage
//' @param p is the precipitation
//' @param PE is the potential evaporation
//' @param temp is monthly temperature
//' @param alpha controlled by negative temperature. This unit is degree Celsius
//' @param beta is influence by negative temperature
//' @param Gmax is the potential maximum ground water storage
//' @return abcd_cr function the output list of  a time-series of water balance components
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
//' dt[, abcd_cr(para_a, para_b, para_c, para_d, SWS_ini, S_ini, G_ini, p, PE, temp, alpha, beta, Gmax)
//'  , by=.(p, PE, temp)]
// [[Rcpp::export]]
List abcd_cr(double para_a, double para_b, double para_c, double para_d, double SWS_ini, double S_ini, double G_ini, NumericVector p,
             NumericVector PE, NumericVector temp,  double alpha, double beta, double Gmax ) {

  
  List res(9);
  int arrSize = PE.size();
  
  
   double para_cra = temp.size();
  NumericVector para_cr(para_cra);
  
   double para_dra = temp.size();
  NumericVector para_dr(para_dra);
  
    double sfa = p.size();
  NumericVector sf(sfa);
  
  
      double SNRFA = p.size();
  NumericVector SNRF(SNRFA);
  
  double SWa = p.size();
  NumericVector SW(SWa);
  
    double yya = p.size();
  NumericVector yy(yya);
  
   
  double Wa = p.size();
  NumericVector W(Wa);
  
  double Ea = p.size();
  NumericVector E(Ea);
  
   double GWSA = p.size();
  NumericVector GWS(GWSA); 
  
    double GWE_BSA = p.size();
  NumericVector GWE_BS(GWE_BSA);
  
    double CGWSA = p.size();
  NumericVector CGWS(CGWSA);
  
  double QA = p.size();
  NumericVector Q(QA);
  
  
  
  
 
  
  
  
  
  for (int i=0; i<arrSize; i++) {
	  
	  if(temp[i]<=0){
		  para_cr[i]=para_c*exp(alpha*temp[i]);
		  para_dr[i]=para_d*exp(alpha*temp[i]);
		  p[i] = 0;
		  sf[i]=p[i];
	//Available snow cover
    SW[i] = sf[i]+SWS_ini;
	// Snow melt surface runoff
	SNRF[i] = 0.0;
	
	  } else if(temp[i]>0){
		  para_cr=para_c;
		  para_dr=para_d;
		  p[i] = p[i];
		  sf[i]=0;
		  
   
	// Snow melt surface runoff
	SNRF[i] = beta*temp[i]*SWS_ini;
	//Available snow cover
    SW[i] = (1-beta*temp[i])*SWS_ini;
		  
		  
	  }
	  
	  // need to cross check
	  if(SW[i] > SWS_ini){
	    p[i]=p[i]+SW[i];
	  }else{
	    p[i]=p[i];
	  }
	  //Available soil water
	   W[i] = p[i]+ S_ini;
    //Wt[i] = W[i];
	 
    
    //Evapotranspiration potential
    yy[i] = (W[i]+para_b)/(2*para_a)-(pow((W[i]+para_b)/(2*para_a), 2))- pow((W[i]*para_b/para_a), 0.5);
    //actual evapotranspiration
    E[i] = yy[i]*(1-exp(-PE[i]/para_b));
    //Et[i] = E[i];
	// ground water storage
	GWS[i] = (1+para_d+((PE[i]-E[i])/Gmax))*(para_c*(W[i]-yy[i])+G_ini);
	 // ground water evaporation at basin scale
	 GWE_BS[i] = (GWS[i]/Gmax)*(PE[i]-E[i]);
	 // change in ground water storage
	 CGWS[i] = para_c*(W[i]-yy[i])-(para_d*GWS[i])- E[i];
	 // total runoff
     Q[i] =(1-para_c)*(W[i]-yy[i])+para_d*GWS[i]+ SNRF[i];
	 
	 
	 
 
  }
     res[0] = SNRF;
  res[1] = SW;
  res[2]= W;
  res[3]= yy;
  res[4]= E;
  res[5]= GWS;
  res[6]= GWE_BS;
  res[7]= CGWS;
  res[8]= Q;
  
 res = List::create(Named("SNRF")=SNRF, Named("SW")=SW, Named("W") = W , Named("yy") = yy, Named("AET") = E , Named("GWS") = GWS, Named("GWE_BS") =GWE_BS , Named("CGWS") = CGWS, Named("TR")=	Q);
  
  return res; 
  }
   