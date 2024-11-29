#include <Rcpp.h>
#include <iostream>
#include <chrono>
using namespace Rcpp;
using namespace std;
using namespace std::chrono;

// [[Rcpp::export]]
List bubbleSort(NumericVector x){
  NumericVector xord = clone(x);
  int k = 0;
  
  high_resolution_clock::time_point t1 = high_resolution_clock::now();
  
  for(int i = 0; i < x.size(); i++){
    for(int j = i + 1; j < x.size(); j++){
      if(xord(j) < xord(i)) {
        k++;
        swap(xord(j), xord(i));
      }
    }
  }
  
  high_resolution_clock::time_point t2 = high_resolution_clock::now();
  
  duration<double> time_span = duration_cast< duration<double> >(t2 - t1);
  
  List res;
  res["intercanvis"] = k;
  res["temps"] = time_span.count();
  res["xord"] = wrap(xord);
  return res;
}

// [[Rcpp::export]]
void i_quickSort(vector<double>& x, int n, int esq, int dre){
  int pos_pivot;
  
  if (esq < dre) {
    int pivot = x[dre];
    pos_pivot = esq;
    for (int i = esq; i < dre; i++) {
      if (x[i] < pivot) {
        std::swap(x[i], x[pos_pivot]);
        pos_pivot++;
      }
    }
    std::swap(x[dre], x[pos_pivot]);
    
    i_quickSort(x, n, esq, pos_pivot - 1);
    i_quickSort(x, n, pos_pivot + 1, dre);
  }
}

// [[Rcpp::export]]
List quickSort(NumericVector x, int n){
  std::vector<double> xord = as<std::vector<double>>(x);
  
  high_resolution_clock::time_point t1 = high_resolution_clock::now();
  
  i_quickSort(xord, xord.size(), 0, xord.size() - 1);
  
  high_resolution_clock::time_point t2 = high_resolution_clock::now();
  
  duration<double> time_span = duration_cast< duration<double> >(t2 - t1);
  
  List res;
  res["temps"] = time_span.count();
  res["xord"] = wrap(xord);
  return res;
}

// [[Rcpp::export]]
void i_mergeSort(vector<double>& x, int n, int esq, int dre){
  int mig;
  if (esq < dre) {
    mig = (esq+dre)/2;
    i_mergeSort(x, n, esq, mig);
    i_mergeSort(x, n, mig + 1, dre);
    // fusio
    int n_elem = dre - esq + 1;
    vector<double> aux(n_elem);
    int i_aux = 0;
    
    for (int k = esq; k < (mig + 1); k++) {
      aux[i_aux] = x[k];
      i_aux++;
    }
    
    for (int k = dre; k >= (mig + 1); k--) {
      aux[i_aux] = x[k];
      i_aux++;
    }
    
    int i = 0;
    int j = n_elem - 1;
    int k = esq;
    while (i <= j) {
      if (aux[i] <= aux[j]) {
        x[k] = aux[i];
        i++;
      } else {
        x[k] = aux[j];
        j--;
      }
      k++;
    }
  }
}

// [[Rcpp::export]]
List mergeSort(NumericVector x, int n){
  std::vector<double> xord = as<std::vector<double>>(x);
  
  high_resolution_clock::time_point t1 = high_resolution_clock::now();
  
  i_mergeSort(xord, xord.size(), 0, xord.size() - 1);
  
  high_resolution_clock::time_point t2 = high_resolution_clock::now();
  
  duration<double> time_span = duration_cast< duration<double> >(t2 - t1);
  
  List res;
  res["temps"] = time_span.count();
  res["xord"] = wrap(xord);
  return res;
}

// El codi que hi ha a continuació s'executarà després de compilar l'arxiu amb
// la funció sourceCpp()

/*** R
a = c(4,6,3,4,6,23)
bubbleSort(a)
quickSort(a, length(a))
mergeSort(a, length(a))
*/
