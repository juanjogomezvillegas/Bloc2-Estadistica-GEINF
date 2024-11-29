rm(list = ls())
library(Rcpp)

sourceCpp('funcions_ordenacio.cpp')

library(tidyverse)
avalua_escenari = function(iseed, n){
  set.seed(iseed)
  x = sample(1:n)
  resultat = ordena_seleccio_llista(x)
  resultat$k
}
# Es creen alguns escenaris
simulacions = expand_grid('experiment' = 1:50,
                          'mida' = seq(50, 1000, 50))

simulacions = mutate(simulacions,
                     'operacions' = pmap_dbl(list(experiment, mida),
                                             avalua_escenari)
)

ggplot(data = simulacions) +
  geom_point(aes(x = mida, y = operacions/mida))

dim(filter(simulacions, operacions >= 10000))

# -------------------------------------------------------------------

cppFunction('int suma(int x, int y, int z) {
  int suma = x + y + z;
  return suma;
}')

suma(c(1,3,2), c(3,4,5), c(2,4,3))

pmap_dbl(list(c(1,3,2), c(3,4,5), c(2,4,3)), suma)

cppFunction("Rcpp::NumericVector bubbleSort(Rcpp::NumericVector x){
  int n = x.size();
  std::vector<double> xord = as<std::vector<double>>(x);
  for(int i = 0; i < n; i++){
    for(int j = i+1; j < n; j++){
      if(xord[j] < xord[i]) std::swap(xord[j], xord[i]);
    }
  }
  return Rcpp::wrap(xord);
}")
bubbleSort(c(4,6,3,4,6,23))
