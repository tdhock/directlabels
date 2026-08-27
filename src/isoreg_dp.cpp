#include "isoreg_dp.h"
#include <math.h>//isfinite

int isoreg_dp
(const int N_data,
 const double *data_ptr, // size=N_data.
 // inputs above, outputs below.
 // all arrays should be allocated to the right sizes before calling this function.
 int *size_ptr, // size=N_data.
 double *mean_ptr // size=N_data.
 ){
  int last_cluster = -1;
  for(int i=0; i<N_data; i++){
    int samples=1;
    double total=data_ptr[i];
    if(!isfinite(total))return ERROR_DATA_MUST_BE_FINITE;
    while(last_cluster >= 0 && total/samples <= mean_ptr[last_cluster]){
      samples += size_ptr[last_cluster];
      total += size_ptr[last_cluster] * mean_ptr[last_cluster];
      last_cluster--;
    }
    last_cluster++;
    size_ptr[last_cluster] = samples;
    mean_ptr[last_cluster] = total/samples;
  }
  for(int i=last_cluster+1; i<N_data; i++){
    size_ptr[i]=0;
    mean_ptr[i]=INFINITY;
  }
  return 0;
}
    
