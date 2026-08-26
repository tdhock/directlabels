#include <Rcpp.h>
#include "isoreg_dp.h"

// [[Rcpp::export]]
Rcpp::List isoreg_dp_interface
(const Rcpp::NumericVector data_vec
 ){
  int N_data = data_vec.size();
  if(N_data<1){
    Rcpp::stop("no data");
  }
  Rcpp::IntegerVector N_clusters_vec(1);
  Rcpp::IntegerVector cluster_size_vec(N_data);
  Rcpp::NumericVector cluster_mean_vec(N_data);
  int status = isoreg_dp
    (data_vec.size(),
     data_vec.begin(),
     N_clusters_vec.begin(),
     cluster_size_vec.begin(),
     cluster_mean_vec.begin());
  if(status == ERROR_DATA_MUST_BE_FINITE){
    Rcpp::stop("data must be finite"); 
  }
  return Rcpp::List::create
    (Rcpp::Named("N_clusters", N_clusters_vec),
     Rcpp::Named("cluster_size", cluster_size_vec),
     Rcpp::Named("cluster_mean", cluster_mean_vec));
}
