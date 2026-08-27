#include "isoreg_dp.h"
#include <math.h>//isfinite

int isoreg_dp
(const int N_data,
 const double *data_ptr, // size=N_data.
 // inputs above, outputs below.
 // all arrays should be allocated to the right sizes before calling this function.
 int *last_cluster_ptr, // size=1.
 int *size_ptr, // size=N_data.
 double *mean_ptr // size=N_data.
 ){
  *last_cluster_ptr = -1;
  for(int i=0; i<N_data; i++){
    int samples=1;
    double total=data_ptr[i];
    if(!isfinite(total))return ERROR_DATA_MUST_BE_FINITE;
    while(*last_cluster_ptr >= 0 && total/samples <= mean_ptr[*last_cluster_ptr]){
      samples += size_ptr[*last_cluster_ptr];
      total += size_ptr[*last_cluster_ptr] * mean_ptr[*last_cluster_ptr];
      (*last_cluster_ptr)--;
    }
    (*last_cluster_ptr)++;
    size_ptr[*last_cluster_ptr] = samples;
    mean_ptr[*last_cluster_ptr] = total/samples;
  }
  for(int i=*last_cluster_ptr+1; i<N_data; i++){
    size_ptr[i]=0;
    mean_ptr[i]=INFINITY;
  }
  return 0;
}
    
