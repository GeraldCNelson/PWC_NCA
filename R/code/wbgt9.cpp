// relative humidity comes from the ISIMIP data in %. In some places in the heatstress code it needs to be a fraction. I'm adjusting the code to make 
// the variable RH (sometimes called hurs using ISIMIP terminology) in % and relh (= RH * 0.01) as the fraction

#include <Rcpp.h>
#include <cmath>
// #include "/Library/Frameworks/R.framework/Versions/4.1-arm64/PrivateHeaders/vg/memcheck.h"
// # include "/usr/include/valgrind/memcheck.h"

// Physical constants
const double stefanb  =  0.000000056696;
const double cp  =  1003.5; // heat capacity of dry air at constant pressure 
const double m_air  =  28.97; 
const double m_h2o  =  18.015;
const double r_gas  =  8314.34;
const double r_air  =  r_gas / m_air;
const double  ratio  =  cp * m_air/ m_h2o;
const double  Pr  =  cp / (cp + (1.25 * r_air));

// Wick constants
const double emis_wick = 0.95; // emissivity;
const double  alb_wick = 0.4; // albedo;
const double diam_wick = 0.007; // diameter (in m)
const double len_wick = 0.0254; // length (in m)

// Globe constants
const double emis_globe = 0.95; // emissivity
const double   alb_globe = 0.05; // albedo
const double   diam_globe = 0.0508; // diameter (in m)

// Surface constants
const double   emis_sfc = 0.999;
const double SurfAlbedo = 0.4;
//const double   alb_sfc = SurfAlbedo;
const double propDirect = 0.8; //Assume a proportion of direct radiation = direct/(diffuse + direct)
const double Pair = 1010; // Atmospheric pressure in hPa

const double pi = 3.14159265358979323846;
//const double pi2 = 6.28318530717958647692;
const double min_speed = 0.1;
const double kVal = 273.15;

//diffusivity constants
const double pcrit13 = pow((36.4 * 218.), (1. / 3.));
const double  Tcrit512 = pow((132. * 647.3),  (5. / 12.));
const double  Tcrit12 = pow((132. * 647.3), 0.5);
const double Mmix = pow((1. / 28.97 + 1. / 18.015), 0.5);

// [[Rcpp::export]]
inline double diffusivity(const double& Tk) { // saturation vapor pressure
  const double d = 0.000364 * pow((Tk / Tcrit12), 2.334) * pcrit13 * Tcrit512 * Mmix / (Pair / 1013.25) * 0.0001;
  return d;
}

// [[Rcpp::export]]
inline double esat(const double& Tk) { // saturation vapor pressure
  //  alternative, to original in heatstress code, from Ken Parsons book (ref from George Havenith)
  //  return  exp(18.956 - 4030.18/(ta + 235.0));
  
  // units are hPa
  // Ta units C, esat return in C
  // from HeatStress code
  // Tk value of air temperature in Kelvin.
  // return saturation vapor pressure (hPa).
  //  return  exp(18.956 - 4030.18/(ta + 235.0));
  //saturation vapor pressure (hPa) over water.  
  // Reference: Buck's (1981) approximation (eqn 3) of Wexler's (1976) formula over liquid water.
  double satvp  = 1.004 * 6.1121 * exp(17.502 * (Tk - kVal) / (Tk - 32.18));
  // Rcpp::Rcout << "esat function "  << "Tk " << Tk <<  " satvp " << satvp  << std::endl;
  return satvp;
}

// [[Rcpp::export]]
inline double emis_atm(const double& Tk, const double& relh) { 
  double e = relh * esat(Tk);
  return 0.575 * pow(e, 0.143);
}

// thermal radiation calculation; comments below from George Havenith
//See pdf file name below in section B2, equation 9 for the standard globe in forced convection.
//When Tg is given this normally applies to the standard 15 cm globe.
//As our air speeds will be above 0.4 m/s I assume in almost all cases (?) I would assume forced convection.
//The document with section B2, equation 9 is called "Ergonomics of the thermal environment Instruments for measuring physical quantities"
// File BS EN ISO 7726-2001--[2014-12-16--03-16-38 PM].pdf is available upon request.

// The formula below can be found at https://en.wikipedia.org/wiki/Mean_radiant_temperature,

// [[Rcpp::export]]
inline double thermalRadiance(double tas, double wind, double Tg) {
  //Tg is the temperature of the black globe, in C. It is calculated below in fwbgTg
  double temp1 = pow(Tg + kVal, 4.0);
  double temp2 = pow(wind, 0.6) * (Tg - tas);
  double temp3 = (temp1 + 2.5e+8 * temp2);
  double temp4 = pow(temp3, 0.25);
  double tr =  temp4 - kVal;
  // Rcpp::Rcout << "tas " << tas <<  " wind " << wind <<  " Tg " << Tg <<  " tr " << tr <<  " temp1 " << temp1 <<  " temp2 " << temp2 <<  " temp3 " << temp3 <<  " temp4 " << temp4  << std::endl;
  if (temp4 < 0) {
    //  Rcpp::Rcout << "temp4 " << temp4  << std::endl;
  }
  return  tr;
}

// [[Rcpp::export]]
inline double Tdew(const double& tas, const double& RH) {
  // Source: Lawrence, M. G. (2005). The Relationship between Relative Humidity and the Dewpoint Temperature in Moist Air: A Simple Conversion and Applications. Bulletin of the American Meteorological Society, 86(2), 225–234. doi: 10.1175/BAMS-86-2-225
  // constants for new dewpoint temperature
  double a1 = 17.625;
  double b1 = 243.04;
  double relh = RH/100;
  
  double dew = (b1 * (log(relh) + (a1 * tas)/(b1 + tas))) / (a1 - log(relh) - (a1 * tas)/(b1 + tas));
  return dew + kVal;
}

// [[Rcpp::export]]
inline double get_pwc_utci(const double& utci) {
  double pwc = 100. / (1. + pow(45.33 / utci, -4.30));
  // Rcpp::Rcout << "get_pwc_utci" << std::endl;
  
  const double hn = 6.;
  const double level1 = 15.8;
  const double level2 = 35.6;
  const double level3 = 42.5;
  const double level4 = 50.8;
  
  const double l4minusl3 = level4 - level3;
  const double l3minusl2 = level3 - level2;
  const double l2minusl1 = level2 - level1;
  
  if (utci >= level4) {
    pwc -=  2. * hn + 4.86;
  } else if (utci >= level3) {
    pwc += ((utci - level3)/l4minusl3) * (-(2.   * hn + 4.86)) + (-1. * (utci - level4)/l4minusl3) * (-(1.1 * hn + 0.98));
  } else if (utci >= level2) {
    pwc += ((utci - level2)/l3minusl2) * (-(1.1 * hn + 0.98)) + (-1. * (utci - level3)/l3minusl2) * (-(0.65 * hn + 1.3));
  } else if (utci > level1) {
    pwc += ((utci - level1)/l2minusl1) * (-(0.65 * hn + 1.3));
  }
  //  if (pwc < 0) pwc = 0.; // pwc can't be less than 0
  return pwc;
}

// [[Rcpp::export]]
inline double get_pwc_wbgt(const double& wbgt) {
  double pwc = 100. / (1. + pow(33.63 / wbgt, -6.33));
  const double hn = 6.;
  const double level1 = 12.6;
  const double level2 = 29.4;
  const double level3 = 33.4;
  const double level4 = 36.1;
  
  const double l4minusl3 = level4 - level3;
  const double l3minusl2 = level3 - level2;
  const double l2minusl1 = level2 - level1;
  
  if (wbgt >= level4) {
    pwc -=  2. * hn + 4.86;
  } else if (wbgt >= level3) {
    pwc += ((wbgt - level3)/l4minusl3) * (-(2.   * hn + 4.86)) + (-1. * (wbgt - level4)/l4minusl3) * (-(1.1 * hn + 0.98));
  } else if (wbgt >= level2) {
    pwc += ((wbgt - level2)/l3minusl2) * (-(1.1 * hn + 0.98)) + (-1. * (wbgt - level3)/l3minusl2) * (-(0.65 * hn + 1.3));
  } else if (wbgt > level1) {
    pwc += ((wbgt - level1)/l2minusl1) * (-(0.65 * hn + 1.3));
  }
  return pwc;
}

// old version of get_pwc. keeping it around temporarily but commented out
// // [[Rcpp::export]]
// inline double get_pwc(const double& wbgt) {
//   double pwc = 100. / (1. + pow(33.63 / wbgt, -6.33));
//   double hn = 6.;
//   if (wbgt >= 35.) {
//     pwc -=  2. * hn + 4.86;
//     pwc = pwc < 0. ? 0. : pwc;
//   } else if (wbgt >= 33.) {
//     pwc += ((wbgt - 33.)/2.) * (-(2.  * hn + 4.86)) + (-1. * (wbgt - 35.)/2.) * (-(1.1 * hn + 0.98));
//   } else if (wbgt >= 29.) {
//     pwc += ((wbgt - 29.)/4.) * (-(1.1 * hn + 0.98)) + (-1. * (wbgt - 33.)/4.) * (-(0.65 * hn + 1.3));
//   } else if (wbgt > 15.) {
//     pwc += ((wbgt - 15.)/14.) * (-(0.65 * hn + 1.3));
//   } else {
//     pwc = 100;
//   }
//   return pwc;
// }

// [[Rcpp::export]]
inline double get_utci(double ta, double tg, double va, double hurs) {
  
  // Ta       : air temperature, degree Celsius
  // ehPa    : water vapour pressure, hPa=hecto Pascal
  // Tmrt   : mean radiant temperature, degree Celsius
  // va  : wind speed 10 m above ground level in m/s
  // tg the temperature of the black globe, in C. It is calculated below in fwbgTg but read in from an earlier calculation
  
  // Rcpp::Rcout << "get_utci" << std::endl;
  double Tk = ta + kVal;
  double satVapPres = esat(Tk); // in hPa
  double pa = satVapPres * hurs/1000.; // vapor pressure in kPa
  double Tmrt = thermalRadiance(ta, va, tg);
  double dtm = Tmrt - ta;
  va = va < 0.5 ? 0.5 : va > 17. ? 17. : va;
  // Rcpp::Rcout  << "get_utci " <<  " ta " << ta  << " tg " << tg  <<  " va " << va   <<  " hurs " << hurs  << " satVapPres " << satVapPres <<  " pa " << pa <<  " dtm " << dtm   << std::endl;
  
  //RH: more efficient than calling pow 5 times
  double ta2 = ta*ta;
  double ta3 = ta2*ta;
  double ta4 = ta3*ta;
  double ta5 = ta4*ta;
  double ta6 = ta5*ta;
  
  double va2 = va*va;
  double va3 = va2*va;
  double va4 = va3*va;
  double va5 = va4*va;
  double va6 = va5*va;
  
  double dtm2 = dtm*dtm;
  double dtm3 = dtm2*dtm;
  double dtm4 = dtm3*dtm;
  double dtm5 = dtm4*dtm;
  double dtm6 = dtm5*dtm;
  
  double pa2 = pa*pa;
  double pa3 = pa2*pa;
  double pa4 = pa3*pa;
  double pa5 = pa4*pa;
  double pa6 = pa5*pa;
  
  double dtm2_pa = dtm2 * pa;
  double dtm2_pa2 = dtm2 * pa2;
  double dtm3_pa = dtm3 * pa;
  double dtm3_pa2 = dtm3 * pa2;
  double dtm3_pa3 = dtm3 * pa3;
  
  double ta2_va = ta2 * va;
  double ta2_va2 = ta2 * va2;
  double ta3_va = ta3 * va;
  double va_pa = va * pa;
  double va_dtm = va * dtm;
  double utci_out =   ta + 
    0.607562052 + 
    -0.0227712343 * ta + 0.000806470249 * ta2 + -0.000154271372 * ta3 + -3.24651735e-06 * ta4 + 7.32602852e-08 * ta5 + 1.35959073e-09 * ta6 + 
    -2.2583652 * va + 0.0880326035 * ta * va + 0.00216844454 * ta2_va + -1.53347087e-05 * ta3_va + -5.72983704e-07 * ta4 * va + -2.55090145e-09 * ta5 * va + -0.751269505 * va2 + 
    -0.00408350271 * ta * va2 + -5.21670675e-05 * ta2_va2 + 1.94544667e-06 * ta3 * va2 + 1.14099531e-08 * ta4 * va2 + 
    0.158137256 * va3 + -6.57263143e-05 * ta * va3 + 
    2.22697524e-07 * ta2 * va3 + -4.16117031e-08 * ta3 * va3 + 
    -0.0127762753 * va4 + 9.66891875e-06 * ta * va4 + 2.52785852e-09 * ta2 * va4 + 
    0.000456306672 * va5 + -1.74202546e-07 * ta * va5 + -5.91491269e-06 * va6 + 
    0.398374029 * dtm + 0.000183945314 * ta * dtm + -0.00017375451 * ta2 * dtm + -7.60781159e-07 * ta3 * dtm + 3.77830287e-08 * ta4 * dtm + 5.43079673e-10 * ta5 * dtm +
    -0.0200518269 * va_dtm + 0.000892859837 * ta * va_dtm + 3.45433048e-06 * ta2 * va_dtm + -3.77925774e-07 * ta3 * va_dtm + -1.69699377e-09 * ta4 * va_dtm + 
    0.000169992415 * va2 * dtm + -4.99204314e-05 * ta * va2 * dtm + 2.47417178e-07 * ta2_va2 * dtm + 1.07596466e-08 * ta3 * va2 * dtm + 8.49242932e-05 * va3 * dtm + 
    1.35191328e-06 * ta * va3 * dtm + -6.21531254e-09 * ta2 * va3 * dtm + 
    -4.99410301e-06 * va4 * dtm + -1.89489258e-08 * ta * va4 * dtm + 
    8.15300114e-08 * va5 * dtm + 
    0.00075504309 * dtm2 + 
    -5.65095215e-05 * ta * dtm2 + 
    -4.52166564e-07 * ta2 * dtm2 + 
    2.46688878e-08 * ta3 * dtm2 + 
    2.42674348e-10 * ta4 * dtm2 + 
    0.00015454725 * va * dtm2 + 
    5.2411097e-06 * ta * va * dtm2 + 
    -8.75874982e-08 * ta2_va * dtm2 + 
    -1.50743064e-09 * ta3_va * dtm2 + 
    -1.56236307e-05 * va2 * dtm2 + 
    -1.33895614e-07 * ta * va2 * dtm2 + 
    2.49709824e-09 * ta2_va2 * dtm2 + 
    6.51711721e-07 * va3 * dtm2 + 
    1.94960053e-09 * ta * va3 * dtm2 + 
    -1.00361113e-08 * va4 * dtm2 + 
    -1.21206673e-05 * dtm3 + -2.1820366e-07 * ta * dtm3 + 7.51269482e-09 * ta2 * dtm3 + 9.79063848e-11 * ta3 * dtm3 + 
    1.25006734e-06 * va * dtm3 + -1.81584736e-09 * ta * va * dtm3 +  -3.52197671e-10 * ta2_va * dtm3 +  -3.3651463e-08 * va2 * dtm3 + 1.35908359e-10 * ta * va2 * dtm3 +   4.1703262e-10 * va3 * dtm3 + 
    -1.30369025e-09 * dtm4 + 4.13908461e-10 * ta * dtm4 + 9.22652254e-12 * ta2 * dtm4 + -5.08220384e-09 * va * dtm4 +  -2.24730961e-11 * ta * va * dtm4 +  1.17139133e-10 * va2 * dtm4 + 
    6.62154879e-10 * dtm5 +  4.0386326e-13 * ta * dtm5 + 1.95087203e-12 * va * dtm5 + 
    -4.73602469e-12 * dtm6 + 
    5.12733497 * pa + 
    -0.312788561 * ta * pa + 
    -0.0196701861 * ta2 * pa + 
    0.00099969087 * ta3 * pa + 
    9.51738512e-06 * ta4 * pa + 
    -4.66426341e-07 * ta5 * pa + 
    0.548050612 * va_pa + 
    -0.00330552823 * ta * va_pa + 
    -0.0016411944 * ta2 * va_pa + 
    -5.16670694e-06 * ta3 * va_pa + 
    9.52692432e-07 * ta4 * va_pa + 
    -0.0429223622 * va2 * pa + 
    0.00500845667 * ta * va2 * pa + 
    1.00601257e-06 * ta2_va2 * pa + 
    -1.81748644e-06 * ta3 * va2 * pa + 
    -0.00125813502 * va3 * pa + 
    -0.000179330391 * ta * va3 * pa + 
    2.34994441e-06 * ta2 * va3 * pa + 
    0.000129735808 * va4 * pa + 
    1.2906487e-06 * ta * va4 * pa + 
    -2.28558686e-06 * va5 * pa + 
    -0.0369476348 * dtm * pa + 
    0.00162325322 * ta * dtm * pa + 
    -3.1427968e-05 * ta2 * dtm * pa + 
    2.59835559e-06 * ta3 * dtm * pa + 
    -4.77136523e-08 * ta4 * dtm * pa + 
    0.0086420339 * va_dtm * pa + 
    -0.000687405181 * ta * va_dtm * pa + 
    -9.13863872e-06 * ta2 * va_dtm * pa + 
    5.15916806e-07 * ta3 * va_dtm * pa + 
    -3.59217476e-05 * va2 * dtm * pa + 
    3.28696511e-05 * ta * va2 * dtm * pa + 
    -7.10542454e-07 * ta2_va2 * dtm * pa + 
    -1.243823e-05 * va3 * dtm * pa + 
    -7.385844e-09 * ta * va3 * dtm * pa + 
    2.20609296e-07 * va4 * dtm * pa + 
    -0.00073246918 * dtm2_pa + 
    -1.87381964e-05 * ta * dtm2_pa + 
    4.80925239e-06 * ta2 * dtm2_pa + 
    -8.7549204e-08 * ta3 * dtm2_pa + 
    2.7786293e-05 * va * dtm2_pa + 
    -5.06004592e-06 * ta * va * dtm2_pa + 
    1.14325367e-07 * ta2_va * dtm2_pa + 
    2.53016723e-06 * va2 * dtm2_pa + 
    -1.72857035e-08 * ta * va2 * dtm2_pa + 
    -3.95079398e-08 * va3 * dtm2_pa + 
    -3.59413173e-07 * dtm3_pa + 
    7.04388046e-07 * ta * dtm3_pa + 
    -1.89309167e-08 * ta2 * dtm3_pa + 
    -4.79768731e-07 * va * dtm3_pa + 
    7.96079978e-09 * ta * va * dtm3_pa + 
    1.62897058e-09 * va2 * dtm3_pa + 
    3.94367674e-08 * dtm4 * pa + 
    -1.18566247e-09 * ta * dtm4 * pa + 
    3.34678041e-10 * va * dtm4 * pa + 
    -1.15606447e-10 * dtm5 * pa + 
    -2.80626406 * pa2 + 
    0.548712484 * ta * pa2 + 
    -0.0039942841 * ta2 * pa2 + 
    -0.000954009191 * ta3 * pa2 + 
    1.93090978e-05 * ta4 * pa2 + 
    -0.308806365 * va * pa2 + 
    0.0116952364 * ta * va * pa2 + 
    0.000495271903 * ta2_va * pa2 + 
    -1.90710882e-05 * ta3_va * pa2 + 
    0.00210787756 * va2 * pa2 + 
    -0.000698445738 * ta * va2 * pa2 + 
    2.30109073e-05 * ta2_va2 * pa2 + 
    0.00041785659 * va3 * pa2 + 
    -1.27043871e-05 * ta * va3 * pa2 + 
    -3.04620472e-06 * va4 * pa2 + 
    0.0514507424 * dtm * pa2 + 
    -0.00432510997 * ta * dtm * pa2 + 
    8.99281156e-05 * ta2 * dtm * pa2 + 
    -7.14663943e-07 * ta3 * dtm * pa2 + 
    -0.000266016305 * va_dtm * pa2 + 
    0.000263789586 * ta * va_dtm * pa2 + 
    -7.01199003e-06 * ta2 * va_dtm * pa2 + 
    -0.000106823306 * va2 * dtm * pa2 + 
    3.61341136e-06 * ta * va2 * dtm * pa2 + 
    2.29748967e-07 * va3 * dtm * pa2 + 
    0.000304788893 * dtm2_pa2 + 
    -6.42070836e-05 * ta * dtm2_pa2 + 
    1.16257971e-06 * ta2 * dtm2_pa2 + 
    7.68023384e-06 * va * dtm2_pa2 + 
    -5.47446896e-07 * ta * va * dtm2_pa2 + 
    -3.5993791e-08 * va2 * dtm2_pa2 + 
    -4.36497725e-06 * dtm3_pa2 + 
    1.68737969e-07 * ta * dtm3_pa2 + 
    2.67489271e-08 * va * dtm3_pa2 + 
    3.23926897e-09 * dtm4 * pa2 + 
    -0.0353874123 * pa3 + 
    -0.22120119 * ta * pa3 + 
    0.0155126038 * ta2 * pa3 + 
    -0.000263917279 * ta3 * pa3 + 
    0.0453433455 * va * pa3 + 
    -0.00432943862 * ta * va * pa3 + 
    0.000145389826 * ta2_va * pa3 + 
    0.00021750861 * va2 * pa3 + 
    -6.66724702e-05 * ta * va2 * pa3 + 
    3.3321714e-05 * va3 * pa3 + 
    -0.00226921615 * dtm * pa3 + 
    0.000380261982 * ta * dtm * pa3 + 
    -5.45314314e-09 * ta2 * dtm * pa3 + 
    -0.000796355448 * va_dtm * pa3 + 
    2.53458034e-05 * ta * va_dtm * pa3 + 
    -6.31223658e-06 * va2 * dtm * pa3 + 
    0.000302122035 * dtm2 * pa3 + 
    -4.77403547e-06 * ta * dtm2 * pa3 + 
    1.73825715e-06 * va * dtm2 * pa3 + 
    -4.09087898e-07 * dtm3_pa3 + 
    0.614155345 * pa4 + 
    -0.0616755931 * ta * pa4 + 
    0.00133374846 * ta2 * pa4 + 
    0.00355375387 * va * pa4 + 
    -0.000513027851 * ta * va * pa4 + 
    0.000102449757 * va2 * pa4 + 
    -0.00148526421 * dtm * pa4 + 
    -4.11469183e-05 * ta * dtm * pa4 + 
    -6.80434415e-06 * va_dtm * pa4 + 
    -9.77675906e-06 * dtm2 * pa4 + 
    0.0882773108 * pa5 + 
    -0.00301859306 * ta * pa5 + 
    0.00104452989 * va * pa5 + 
    0.000247090539 * dtm * pa5 + 
    0.00148348065 * pa6;
  if (utci_out < -100) {
    //  Rcpp::Rcout <<  " utci_out " << utci_out  << " ta " << ta << " tg " << tg  <<  " va " << va  
    //   <<  " hurs " << hurs  << " satVapPres " << satVapPres <<  " pa " << pa <<  " dtm " << dtm <<  " thermalR " << Tmrt   << std::endl;
  }
  return utci_out;
}

inline std::vector<double> seq(double start, double end, double increment) {
  std::vector<double> out;
  //	if (increment <= 0) return out; 
  //	if (start >= end) return out;
  // Rcpp::Rcout << "seq " <<  std::endl;
  size_t s = floor((end - start) / increment);
  out.reserve(s);
  for (size_t i=0; i<=s; i++) {
    double val = start + i * increment;
    out.push_back(val);
  }
  return out;
}

// [[Rcpp::export]]
inline double viscosity(const double& Tk) { 
  //viscosity of air, kg/(m s)
  // https://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html?vA=290&units=K#
  double visc = (((Tk / 97.0) - 2.9) / 0.4 * (-0.034)) + 1.048;
  return 0.0000026693 * pow((28.97 * Tk), 0.5) / (pow(3.617, 2.0) * visc);
}

// [[Rcpp::export]]
inline double thermal_cond(const double& viscosity) {
  //Thermal conductivity of air, W/(m K). Reference: BSL, page 257.
  return viscosity * (cp + 1.25 * r_air);
}

// [[Rcpp::export]]
inline double h_evap(const double& Tk) {
  return (313.15 - Tk) / 30.0 * (-71100.0) + 2407300.0;
}

// [[Rcpp::export]]
inline double h_cylinder_in_air(const double& Tk, const double& speed) {
  double density = Pair * 100. / (r_air * Tk); // 
  double okspeed = speed < min_speed ?  min_speed : speed;
  // Reynolds number,  different than the one in h_sphere_in_air
  double Re = okspeed * density * diam_wick / viscosity(Tk); 
  // Nusselt number, different than the one in h_sphere_in_air 
  double Nu = 0.281 * pow(Re, 0.6) * pow(Pr, 0.44); 
  //replaces 
  double thermal_con = thermal_cond(viscosity(Tk));
  return Nu * thermal_con / diam_wick;
}

// [[Rcpp::export]]
inline double h_sphere_in_air(const double& Tk, const double& speed) { 
  //Convective heat transfer coefficient for flow around a sphere, W/(m2 K). 
  // Reference: Bird, Stewart, and Lightfoot (BSL), page 409
  
  double thermal_con = thermal_cond(viscosity(Tk));
  double density = Pair * 100. / (r_air * Tk); // 
  double okspeed = speed < min_speed ?  min_speed : speed;
  // Reynolds number for sphere
  double Re = okspeed * density * diam_globe /  viscosity(Tk);
  // Nusselt number for sphere
  double Nu = 2.0 + 0.6 * pow(Re, 0.5) * pow(Pr, 0.3333); 
  return thermal_con * Nu / diam_globe; 
}

const double toRad = pi/180.; // 
// const double toDeg = 180./pi; // 

// inline double degToRad(double& angle) {
//   return toRad * angle;
// }
// 
// inline double radToDeg(double& angle) {
//   return toDeg * angle;
// }

// [[Rcpp::export]]
inline double calZenith(const int& doy, const int& year_num,  double lat_deg) { 
  // return zenith in degrees
  const double DECL1 = 0.006918;
  const double DECL2 = 0.399912;
  const double DECL3 = 0.070257;
  const double DECL4 = 0.006758;
  const double DECL5 = 0.000907;
  const double DECL6 = 0.002697;
  const double DECL7 = 0.00148;
  
  const double utc_hour = 12.0;
  const double TimeOffset = 0.0;
  const double TrueSolarTime = (utc_hour * 60.0) + TimeOffset;
  double HaDeg = ((TrueSolarTime/4.0) - 180.0);
  // double HaRad = degToRad(HaDeg);
  double HaRad = toRad * HaDeg;
  
  //  double lat_rad = degToRad(lat_deg);
  // Rcpp::Rcout << "toRad " << toRad << " lat_deg " << lat_deg << std::endl;
  double lat_rad = toRad * lat_deg;
  // get number of days in a year. Deals with leap years.
  int dpy = (year_num % 400 == 0 || year_num % 4 == 0) ? 366 : 365;
  
  //Evaluate the fractional year in radians 
  double Gamma = 2. * pi * (((doy * 1.0) - 1.0) + (utc_hour/24.0))/(dpy * 1.0); 
  
  double Decli = DECL1 - DECL2 * cos(Gamma) + DECL3 * sin(Gamma) - DECL4 * cos(2. * Gamma) 
    + DECL5 * sin(2 * Gamma) -  DECL6 * cos(3. * Gamma) + DECL7 * sin(3. * Gamma); 
  double CosZen = (sin(lat_rad) * sin(Decli) + cos(lat_rad) * cos(Decli) * cos(HaRad));
  CosZen = CosZen > 1. ? 1. : (CosZen < -1. ? 1. : CosZen);
  
  return acos(CosZen); // returns in rads
}

// function to be minimized. Returns globe temperature in degC.
// Tglobe_prev is the value of Tair over which the optimization occurs. The range is Tair-2, Tair+10

// [[Rcpp::export]]
inline double fr_tg(const double &Tglobe_prev, const double &Tair, const double &hurs, const double &speed, 
                    const double &radiation, const double &zenith_rad, const double &viscosity_out, 
                    const double &emis_atm_out) {
  // Rcpp::Rcout << "The zenith value into fr_tg is " << zenith_rad << std::endl;
  double cza = cos(zenith_rad); //# cosine of zenith angle
  // Tsfc is surface temperature; Tair is air temp.
  // Since we don't have separate values for these Liljegren, et al, set them equal. 
  // double Tsfc = Tair[i];
  double Tref_globe = 0.5 * (Tglobe_prev + Tair);
  //Convective heat transfer coefficient for flow around a sphere, W/(m2 K)
  double h_sphere = h_sphere_in_air(Tref_globe, speed);
  double Tglobe = pow((0.5 * (emis_atm_out * pow(Tair, 4.) + emis_sfc * 
                      pow(Tair, 4.)) - h_sphere / (emis_globe * stefanb) * 
                      (Tglobe_prev - Tair) + radiation / (2. * emis_globe * stefanb) * 
                      (1. - alb_globe) * (propDirect * (1. / (2. * cza) - 1.) + 1. + SurfAlbedo)), 0.25);
  return  fabs(Tglobe - Tglobe_prev); //fabs returns double abs
}

// function to be minimized for tnwb

// [[Rcpp::export]]
inline double fr_tnwb(const double &Twb_prev, const double &Tair, const double &speed, 
                      double &radiation, const double &zenith_rad, const double &viscosity_out,
                      const double &emis_atm_out, const double &eair, const double &density) {
  // Rcpp::Rcout << "The zenith value into fr_tnwb is " << zenith_rad << std::endl;
  // Rcpp::Rcout << "fr_tnwb" << std::endl;
  const double irad = 1.0;
  double Tref_cylinder = 0.5 * (Twb_prev + Tair);
  double Fatm =  stefanb * emis_wick * (0.5 * (emis_atm_out * pow(Tair, 4.0) + emis_sfc *
                                        pow(Tair, 4.0)) - pow(Twb_prev, 4.0)) + (1.0 - alb_wick) * radiation * 
                                        ((1.0 - propDirect) * (1.0 + 0.25 * diam_wick/len_wick) +
                                        ((tan(zenith_rad)/3.1416) + 0.25 * diam_wick/len_wick) * propDirect + SurfAlbedo);
  double diff = diffusivity(Tref_cylinder);
  double Sc = viscosity_out / (density * diff);
  double h_cyl = h_cylinder_in_air(Twb_prev, speed);
  double ewick = esat(Twb_prev); 
  double evap = h_evap(Twb_prev);
  double Twb = Tair - evap / ratio * (ewick - eair) / (Pair - ewick) * pow((Pr / Sc), 0.56) + Fatm / h_cyl * irad;
  //  Rcpp::Rcout << "2 " << Fatm <<  " " << diff <<  "Sc " << Sc <<  "h_cyl " << h_cyl <<  "ewick " << ewick<<  "evap " << evap <<  " " << Twb <<  "Twb_prev " << Twb_prev << std::endl;
  return fabs(Twb - Twb_prev);
}

// [[Rcpp::export]]
inline double optim_fTnwb_2steps(const double &Tair, const double &hurs, const double &speed, double &radiation, 
                                 const double &zenith_rad, const double &viscosity, const double &emis_atm_out, const double &eair, const double &density, const double &tolerance) {
  // Rcpp::Rcout << "optim_fTnwb_2steps" <<std::endl;
  
  double tas = Tair - kVal;
  double Tdew_out = Tdew(tas, hurs);
  // double delta = Tair +10 - Tdew_out - 1;
  // if (delta < 0) Rcpp::Rcout << "Tdew_out " << Tdew_out << "Tair " << Tair << " delta " << delta << " tas " << tas << "hurs " << hurs    << std::endl;
  
  std::vector<double> rng = seq(Tdew_out-1.0, Tair+10.0, tolerance);
  size_t m = rng.size();
  
  double tst1 = fr_tnwb(rng[0], Tair, speed, radiation, zenith_rad, viscosity, emis_atm_out, eair, density);
  for (size_t i=10; i<m; i+=10) {		  
    //   if (i > m) Rcpp::Rcout << "i greater than m " <<   std::endl;
    double tst2 = fr_tnwb(rng[i], Tair, speed, radiation, zenith_rad, viscosity, emis_atm_out, eair, density);
    if (tst2 > tst1) {
      size_t off = i==10 ? 0 : i-20;
      double tstA = fr_tnwb(rng[off], Tair, speed, radiation, zenith_rad, viscosity, emis_atm_out, eair, density);
      off++;
      for (size_t j=off; j<i; j++) { 
        //if (i > j) Rcpp::Rcout << "i greater than j in optim_fTnwb_2steps" << "  i " << i <<" j " << j <<  std::endl;
        
        double tstB = fr_tnwb(rng[j], Tair, speed, radiation, zenith_rad, viscosity, emis_atm_out, eair, density);
        if (tstB > tstA) {
          return rng[j-1] - kVal;
        }
        tstA = tstB;
      }
      // Rcpp::Rcout << "rng[i-1] " << rng[i-1] << std::endl;
      
      return rng[i-1] - kVal;
    }
    tst1 = tst2;
  }  
  //Rcpp::Rcout << "rng[m-1] " << rng[m-1] << std::endl;
  //Rcpp::Rcout << "rng[m-1] - kVal " << rng[m-1] - kVal << std::endl;
  
  return rng[m-1] - kVal;
}

double optim_fTg_2steps(const double &Tair, const double &hurs, const double &speed,  double &radiation, 
                        const double &zenith_rad, const double &viscosity, const double &emis_atm_out, const double &tolerance) {
  //Rcpp::Rcout << "The zenith value sent to fr_tg from optim_fTg_2steps is " << zenith_rad << std::endl;
  // Rcpp::Rcout << "optim_fTg_2steps " <<  std::endl;
  
  std::vector<double> rng = seq(Tair-2.0, Tair+10.0, tolerance);
  size_t m = rng.size(); 
  double tst1 = fr_tg(rng[0], Tair, hurs, speed, radiation, zenith_rad, viscosity, emis_atm_out); 
  for (size_t i=10; i<m; i+=10) {		  
    //if (i > m) Rcpp::Rcout << "i greater than m " <<  std::endl;
    double tst2 = fr_tg(rng[i], Tair, hurs, speed, radiation, zenith_rad, viscosity, emis_atm_out); 
    if (tst2 > tst1) {
      size_t off = i==10 ? 0 : i-20;
      double tstA = fr_tg(rng[off], Tair, hurs, speed, radiation, zenith_rad, viscosity, emis_atm_out); 
      off++;
      for (size_t j=off; j<i; j++) { 
        //if (i > j) Rcpp::Rcout << "i greater than j in optim_fTg_2steps" <<  std::endl;
        double tstB = fr_tg(rng[j], Tair, hurs, speed, radiation, zenith_rad, viscosity, emis_atm_out); 
        if (tstB > tstA) {
          return rng[j-1] - kVal;
        }
        tstA = tstB;
      }
      return rng[i-1] - kVal;
    }
    tst1 = tst2;
  }  
  return rng[m-1] - kVal; //return is in C
}

// Tg output only

// [[Rcpp::export]]
inline std::vector<double> fwbgTg(const Rcpp::NumericVector tas, const Rcpp::NumericVector hurs,
                                  const Rcpp::NumericVector wind, const Rcpp::NumericVector srad, const Rcpp::NumericVector lat, 
                                  const Rcpp::NumericVector year, const Rcpp::NumericVector doy, const double& tolerance, const int& optim=2) {
  size_t n = tas.size();
  // Rcpp::Rcout << "fwbgTg " <<  std::endl;
  
  std::vector<double> out;
  out.reserve(n); 		
  
  for (size_t i=0; i<n; i++) {
    //if (i > n) Rcpp::Rcout << "i greater than n " <<  std::endl;
    if (std::isnan(tas[i]) || std::isnan(hurs[i]) || std::isnan(wind[i]) || std::isnan(srad[i])) { 
      out.push_back(NAN);
      continue; 
    }
    
    double radiation = srad[i];
    double zenith_rad = calZenith(doy[i], year[i], lat[i]); 
    double relh = hurs[i] * 0.01;
    double Tair = tas[i] + kVal;
    double emis_atm_out = emis_atm(Tair, relh);
    
    double visc = viscosity(Tair);
    //Fix up out-of bounds problems with zenith. 
    if(radiation > 0.   & zenith_rad > 1.57) zenith_rad = 1.57; // 90°
    if(radiation > 15.  & zenith_rad > 1.54)  zenith_rad = 1.54; // 88°
    if(radiation > 900. & zenith_rad > 1.52) zenith_rad = 1.52; // 87°
    if(radiation < 10.  & zenith_rad == 1.57) radiation = 0.;
    
    double Tg = optim_fTg_2steps(Tair, hurs[i], wind[i], radiation, zenith_rad, visc, emis_atm_out, tolerance);
    if (Tg - tas[i] < 0) {
      //   Rcpp::Rcout << "Tg " << Tg << " tas[i] " << tas[i] << " hurs[i] " << hurs[i] << " wind[i] " << wind[i]  << std::endl;
    }
    out.push_back(Tg);
  }
  return out; // Tg is in C
}

// [[Rcpp::export]]
inline std::vector<double> fwbgTnwb(const Rcpp::NumericVector tas, const Rcpp::NumericVector hurs,
                                    const Rcpp::NumericVector wind, const Rcpp::NumericVector srad, const Rcpp::NumericVector lat, 
                                    Rcpp::DateVector dates, const double tolerance, const int optim=2, std::string output="pwc") {
  std::vector<double> out;
  size_t ds = dates.size();
  // size_t n = tas.ncol();
  out.reserve(tas.size());
  // Rcpp::Rcout << "fwbgTnwb " <<  std::endl;
  
  std::vector<int> year, doy;
  year.reserve(ds); doy.reserve(ds);
  for (size_t i=0; i<ds; i++) {
    Rcpp::Date d = dates[i];
    year.push_back(d.getYear());
    doy.push_back(d.getYearday());
  }
  
  for (size_t i=0; i<ds; i++) {
    if (std::isnan(tas[i]) || std::isnan(hurs[i]) || std::isnan(wind[i]) || std::isnan(srad[i])) {
      out.push_back(NAN);
      continue;
    }
    double radiation = srad[i];
    double zenith_rad = calZenith(doy[i], year[i], lat[i]);  // zenith is in degrees
    double relh = hurs[i] * 0.01;
    double Tair = tas[i] + kVal;
    double eair = relh * esat(Tair);	
    double emis_atm_out = emis_atm(Tair, relh);
    
    //Fix up out-of bounds problems with zenith.
    if(radiation > 0.   & zenith_rad > 1.57) zenith_rad = 1.57; // 90°
    if(radiation > 15.  & zenith_rad > 1.54)  zenith_rad = 1.54; // 88°
    if(radiation > 900. & zenith_rad > 1.52) zenith_rad = 1.52; // 87°
    if(radiation < 10.  & zenith_rad == 1.57) radiation = 0.;
    
    double density = Pair * 100./(Tair * r_air); // 
    double viscosity_out = viscosity(Tair);
    // Rcpp::Rcout << "1 " << eair <<  " " << emis_atm_out <<  " " << zenith_rad<<  " " << viscosity_out << std::endl;
    //   Rcpp::Rcout << radiation <<  " " << zenith_rad<<  " " << density<<  " " << viscosity_out << std::endl;
    // Rcpp::Rcout << "The zenith value sent to optim_fTnwb_2steps from fwbgTnwb is " << zenith_deg << std::endl;
    double Tnwb = optim_fTnwb_2steps(Tair, hurs[i], wind[i], radiation, zenith_rad, viscosity_out, emis_atm_out, eair, density, tolerance);
    out.push_back(Tnwb);
    
    //    Rcpp::Rcout << "out:i " << out[i] << std::endl;
  }
  return out;
}

// [[Rcpp::export]]
inline std::vector<double> Tg_out(Rcpp::NumericMatrix tas, Rcpp::NumericMatrix hurs,
                                  Rcpp::NumericMatrix wind, Rcpp::NumericMatrix srad, const Rcpp::NumericMatrix lat, 
                                  const Rcpp::NumericMatrix year, const Rcpp::NumericMatrix doy,
                                  double tolerance, const int optim=2, std::string output="pwc") {
  std::vector<double> out;
  size_t n = tas.ncol();
  out.reserve(tas.size());
  // Rcpp::Rcout << "Tg_out " <<  std::endl;
  
  for (size_t i=0; i<n; i++) {
    std::vector<double> x = fwbgTg(tas(Rcpp::_, i), hurs(Rcpp::_, i), wind(Rcpp::_, i), srad(Rcpp::_, i), lat(Rcpp::_, i), year(Rcpp::_, i), doy(Rcpp::_, i), tolerance, optim); 
    out.insert(out.end(), x.begin(), x.end());
  }
  return out;
}

// new fwbgTemp function that reads in Tg rather than calculating it
// [[Rcpp::export]]
void fwbgTemp(std::vector<double> &out,
              const Rcpp::NumericVector tas, const Rcpp::NumericVector hurs,
              const Rcpp::NumericVector wind, const Rcpp::NumericVector srad, const Rcpp::NumericVector Tg, const Rcpp::NumericVector lat_deg, 
              const Rcpp::NumericVector year, const Rcpp::NumericVector doy, const double& tolerance, const int& optim=2, std::string output="pwc_wbgt_out") {
  //Rcpp::Rcout << "fwbgTemp " <<  std::endl;
  //  Rcpp::Rcout << "output " << output <<  std::endl;
  size_t n = tas.size();
  //  size_t h = hurs.size();
  //  size_t w = wind.size();
  // size_t s = srad.size();
  // size_t t = Tg.size();
  // size_t y = year.size();
  // size_t d = doy.size();
  
  // potential values of output
  bool pwc_wbgt_out = (output.compare("pwc_wbgt_out") == 0);
  bool pwc_utci_out = (output.compare("pwc_utci_out") == 0);
  bool utci_out = (output.compare("utci_out") == 0);
  bool wbgt_out = (output.compare("wbgt_out") == 0);
  bool fwbgTnwb_out = (output.compare("fwbgTnwb_out") == 0);
  
  for (size_t i=0; i<n; i++) {
    if (std::isnan(tas[i]) || std::isnan(hurs[i]) || std::isnan(wind[i]) || std::isnan(srad[i])) { 
      //   Rcpp::Rcout << "fwbgTemp " << "tas[i] " << tas[i] <<  " hurs[i] " << hurs[i] <<  " wind[i] " << wind[i] <<  " srad[i] " << srad[i] << std::endl;
      
      out.push_back(NAN);
      continue; 
    }
    double radiation = srad[i];
    // Rcpp::Rcout<< "i " << i << " doyi " << doy[i] <<  " yeari " <<  year[i] << std::endl;
    double zenith_rad = calZenith(doy[i], year[i], lat_deg[i]); 
    double relh = hurs[i] * 0.01;
    double Tair = tas[i] + kVal;
    double eair = relh * esat(Tair);	
    double emis_atm_out = emis_atm(Tair, relh);
    double density = Pair * 100./(Tair * r_air); // 
    double visc = viscosity(Tair);
    double Tnwb;
    
    Tnwb = optim_fTnwb_2steps(Tair, hurs[i], wind[i], radiation, zenith_rad, visc, emis_atm_out, eair, density, tolerance);
    //push_back adds a new element at the end of the vector, after its current last element
    
    // here too, you could remove the ifs and just hard-code the one you want
    // but that would also be inconvenient
    
    if (pwc_wbgt_out) {
      if (tas[i] < 12.) out.push_back(100.); // if else below sets pwc = 100 when temp is below lower value of range defined in Foster, et al 2021, Table 3
      else {
        double wbgt = 0.7 * Tnwb + 0.2 * Tg[i] + 0.1 * tas[i];
        double u = get_pwc_wbgt(wbgt); 
        out.push_back(u);
      }
    } else if (pwc_utci_out) { 
      if (tas[i] < 12.) out.push_back(100.); // if else below sets pwc = 100 when temp is below lower value of range defined in Foster, et al 2021, Table 3
      else {
        double utci = get_utci(tas[i], Tg[i], wind[i], hurs[i]);
        double u = get_pwc_utci(utci);
        out.push_back(u); }
    }
    else if (wbgt_out) { 
      double wbgt = 0.7 * Tnwb + 0.2 * Tg[i] + 0.1 * tas[i];
      //    Rcpp::Rcout << "wbgt output " << wbgt <<  std::endl;
      double u = wbgt;
      out.push_back(u);
    } else if (utci_out) { 
      //    Rcpp::Rcout << "output " << output <<  std::endl;
      double utci = get_utci(tas[i], Tg[i], wind[i], hurs[i]);
      // if (utci < -1000.) {
      //   Rcpp::Rcout << "utci " << utci << " tas[i] " << tas[i] << " Tg[i] " << Tg[i] << " wind[i] " << wind[i] << "  hurs[i] " << hurs[i]  << std::endl;
      // }
      double u = utci;
      out.push_back(u);
    } else if (fwbgTnwb_out) { 
      out.push_back(Tnwb);
    }
    else {
      //   double wbgt = 0.7 * Tnwb + 0.2 * Tg[i] + 0.1 * tas[i];
      Rcpp::Rcout << "No valid boolean " <<  std::endl;
      Rcpp::Rcout << "output " << output <<  std::endl;
      //   
      //   out.push_back(wbgt);
    }
  }
  //  Rcpp::Rcout << "n " << n <<  "h " <<  h << " w " <<  w << " s " <<  s << " t " << t << " y " <<  y << " d " << d << std::endl;
  
}

//function called from R, data sent to fwbgTemp which returns x. What x is, is determined by output value, interpreted in fwbgTemp
// [[Rcpp::export]]
std::vector<double> pwc_lapp(Rcpp::NumericMatrix tas, Rcpp::NumericMatrix hurs,
                             Rcpp::NumericMatrix wind, Rcpp::NumericMatrix srad, Rcpp::NumericMatrix Tg, Rcpp::NumericMatrix lat,
                             Rcpp::NumericMatrix year, Rcpp::NumericMatrix doy,
                             double tolerance, int optim=2, std::string output="pwc") {
  
  std::vector<double> out;
  // size_t ds = dates.size();
  size_t n = tas.ncol();
  //  size_t m = tas.nrow();
  //  size_t l = lat.size();
  out.reserve(tas.size());
  // Rcpp::Rcout << "pwc_lapp " <<  "latsize " <<  l <<  " datessize " <<  ds <<  " tasncol " <<  n <<  " tasnrow " <<  m <<  std::endl;
  // Rcpp::Rcout << "lat " <<  lat << std::endl;
  // std::vector<int> year, doy;
  // year.reserve(ds); doy.reserve(ds);
  // for (size_t i=0; i<ds; i++) {
  //   
  //   Rcpp::Date d = dates[i];
  //   year.push_back(d.getYear());
  //   doy.push_back(d.getYearday());
  //  }
  
  // now we pass "out" by reference, and have it filled by fwbgTemp  
  for (size_t i=0; i<n; i++) { 
    // Rcpp::Rcout << "pwc_lapp " <<   "tas[i] " << tas[i] << " hurs[i] " << hurs[i] <<  " wind[i] " << wind[i] <<  " srad[i] " << srad[i] << std::endl;
    // Rcpp::Rcout << "size lat " << l << " i " << i << " lat[i] " << lat[i] << " tas[i] " << tas[i]  << std::endl;
    //std::vector<double> x = 
    fwbgTemp(out, tas(Rcpp::_, i), hurs(Rcpp::_, i), wind(Rcpp::_, i), srad(Rcpp::_, i), Tg(Rcpp::_, i), lat(Rcpp::_, i), year(Rcpp::_, i), doy(Rcpp::_, i), tolerance, optim, output); 
    //    out.insert(out.end(), x.begin(), x.end());
    //  Rcpp::Rcout << "year " << year[i] <<  std::endl;
    
    
  }
  return out;
}


// // [[Rcpp::export]]
// Rcpp::NumericVector pwc_1hr(const Rcpp::NumericVector tas, const Rcpp::NumericVector hurs) {
//   Rcpp::Rcout << "pwc_1hr " <<  std::endl;
//   
//   size_t n = tas.size();
//   Rcpp::NumericVector pwc(tas.size());
//  // pwc = 100.;
//  // Rcpp::Rcout << "pwc " << pwc <<  std::endl;
//   
//   for (size_t i=0; i<n; i++) {
//     if (std::isnan(tas[i]) || std::isnan(hurs[i])) { 
//       pwc.push_back(NAN);
//     }  else if (tas[i] > 12.) {
//       double temp1 = (-12.28*log(hurs[i]) + 87.99)/tas[i];
//       double temp2 = (-2.21*log(hurs[i]) + 2.63);
//       pwc[i] = 100/(1 + pow(temp1, temp2));
//     } else if (tas[i] <= 12.) {pwc[i] = 100;}
//   }
//     return pwc;
// }


// The main function 

// uncomment the below to compare with HeatStress package
// commented out to have just the version that reads in Tg


// // [[Rcpp::export]]
// std::vector<double> fwbgTemp(const Rcpp::NumericVector tas, const Rcpp::NumericVector hurs,
//                              const Rcpp::NumericVector wind, const Rcpp::NumericVector srad, double lat, 
//                              const std::vector<int>& year, const std::vector<int>& doy, const double& tolerance, const int& optim=2, std::string output="pwc_wbgt_out") {
//   
//   size_t n = tas.size();
//   
//   std::vector<double> out;
//   out.reserve(n); 		
//   
//   // potential values of output
//   bool pwc_wbgt_out = (output.compare("pwc_wbgt_out") == 0);
//   bool pwc_utci_out = (output.compare("pwc_utci_out") == 0);
//   bool utci_out = (output.compare("ucti_out") == 0);
//   bool wbgt_out = (output.compare("wbg_out") == 0);
//   bool Tg_out = (output.compare("Tg_out") == 0);
//   
//   
//   degToRad(lat);
//   for (size_t i=0; i<n; i++) {
//     
//     if (std::isnan(tas[i]) || std::isnan(hurs[i]) || std::isnan(wind[i]) || std::isnan(srad[i])) { 
//       out.push_back(NAN);
//       continue; 
//       // commented out because out is now used for several potential outputs so the 15 degree cutoff for tas is irrelevant for many. Not sure if this is right.
//       //   // 15 is the temp at which PWC stays at 100 %
//       // } else if (tas[i] < 15.) { 
//       //   if (pwc_wbgt_out | pwc_utci_out) {
//       //     out.push_back(100.); // 
//       //     continue;
//       //   }
//     }
//     
//     double radiation = srad[i];
//     double zenith = getZenith(doy[i], year[i], lat, radiation); 
//     double RH = hurs[i] * 0.01;
//     double Tair = tas[i] + kVal;
//     double eair = RH * esat(Tair);	
//     // replaces call to emis_atm function
//     double emis_atm = 0.575 * pow(eair, 0.143);
//     double density = Pair * 100./(Tair * r_air); // 
//     double visc = viscosity(Tair);
//     
//     double Tnwb, Tg;
//     if (optim ==2) {
//       Tnwb = optim_fTnwb_2steps(Tair, hurs[i], wind[i], radiation, zenith, visc, emis_atm, eair, density, tolerance);
//       Tg = optim_fTg_2steps(Tair, hurs[i], wind[i], radiation, zenith, visc, emis_atm, tolerance);
//     } else 	if (optim ==3) {
//       Tnwb = optim_fTnwb_fwdbwd(Tair, hurs[i], wind[i], radiation, zenith, visc, emis_atm, eair, density, tolerance);
//       Tg = optim_fTg_fwdbwd(Tair, hurs[i], wind[i], radiation, zenith, visc, emis_atm, tolerance);
//     } else if (optim ==1) {
//       Tnwb = optim_fTnwb_steps(Tair, hurs[i], wind[i], radiation, zenith, visc, emis_atm, eair, density, tolerance);
//       Tg = optim_fTg_steps(Tair, hurs[i], wind[i], radiation, zenith, visc, emis_atm, tolerance);
//     } else { //if (optim ==0) {
//       Tnwb = optim_fTnwb_stdmin(Tair, hurs[i], wind[i], radiation, zenith, visc, emis_atm, eair, density, tolerance);
//       Tg = optim_fTg_stdmin(Tair, hurs[i], wind[i], radiation, zenith, visc, emis_atm, tolerance);
//     } 
//     
//     //push_back adds a new element at the end of the vector, after its current last element
//     double wbgt = 0.7 * Tnwb + 0.2 * Tg + 0.1 * tas[i];
//     double utci = get_utci(tas[i], Tg, wind[i], hurs[i]);
//     if (pwc_wbgt_out) {
//       double pwc = get_pwc_wbgt(wbgt);
//       out.push_back(pwc);
//     } else if (pwc_utci_out) { 
//       double u = get_pwc_utci(utci);
//       out.push_back(u);
//     } else if (wbgt_out) { 
//       double u = wbgt;
//       out.push_back(u);
//     } else if (utci_out) { 
//       double u = utci;
//       out.push_back(u);
//     } else if (Tg_out) { 
//       double u = Tg;
//       out.push_back(u);
//     } else {
//       out.push_back(wbgt);
//       //out.push_back(Tnwb);
//       //out.push_back(Tg);
//     }
//   }
//   return out;
// }