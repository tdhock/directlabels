#include "isoreg_dp.h"
#include <math.h>//isfinite

int isoreg_dp
(const int N_data,
 const double *data_ptr,
// inputs above, outputs below
 int *N_clusters_ptr,
 int *size_ptr,
 double *mean_ptr){
  *N_clusters_ptr = -1;
  for(int i=0; i<N_data; i++){
    int samples=1;
    double total=data_ptr[i];
    if(!isfinite(total))return ERROR_DATA_MUST_BE_FINITE;
    while(*N_clusters_ptr >= 0 && total/samples <= mean_ptr[*N_clusters_ptr]){
      samples += size_ptr[*N_clusters_ptr];
      total += size_ptr[*N_clusters_ptr] * mean_ptr[*N_clusters_ptr];
      (*N_clusters_ptr)--;
    }
    (*N_clusters_ptr)++;
    size_ptr[*N_clusters_ptr] = samples;
    mean_ptr[*N_clusters_ptr] = total/samples;
  }
  for(int i=*N_clusters_ptr+1; i<N_data; i++)size_ptr[i]=0;
  return 0;
}
    
