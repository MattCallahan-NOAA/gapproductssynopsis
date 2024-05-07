#include <TMB.hpp>

template<class Type>
Type dlnorm(Type x, Type meanlog, Type sdlog, int give_log=0){
  Type logres;
  logres = dnorm( log(x), meanlog, sdlog, true) - log(x);
  if(give_log)return logres; else return exp(logres);
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(len);
  DATA_VECTOR(age);

  PARAMETER(k);
  PARAMETER(linf);
  PARAMETER(log_sigma);
  PARAMETER(t0);

  int n = len.size();
  vector<Type> eta(n);
  Type nll = 0.0;
  REPORT(eta);

  for(int i = 0; i < n; i++){
    eta(i) = linf * (1 - exp(-k * (age(i) - t0)));
    nll -= dlnorm(len(i), log(eta(i)) - pow(exp(log_sigma), 2)/2, exp(log_sigma), true);
  }

  return nll;
}
