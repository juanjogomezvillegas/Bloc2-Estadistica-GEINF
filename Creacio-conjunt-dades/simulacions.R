# netejem variables d'R
rm(list = ls())

# importem les llibreries d'R necessaries
library(Rcpp)
library(tidyverse)
library(dplyr)

# Executem codi C++ dins d'R
sourceCpp('funcions_ordenacio.cpp')

# funció que genera un vector "aleatori"
generar_vector = function(iseed, n, ordenat){
  set.seed(iseed)
  if (ordenat) sort(sample(1:n))
  else sample(1:n)
}

# funcions que calculen el temps de cada algoritme amb diferents vectors
avalua_bubbleSort = function(iseed, n, ordenat = FALSE){
  resultat = bubbleSort(generar_vector(iseed, n, ordenat))
  resultat$temps
}

avalua_quickSort = function(iseed, n, ordenat = FALSE){
  resultat = quickSort(generar_vector(iseed, n, ordenat), n)
  resultat$temps
}

avalua_mergeSort = function(iseed, n, ordenat = FALSE){
  resultat = mergeSort(generar_vector(iseed, n, ordenat), n)
  resultat$temps
}

# funcions per fer alguns grafics amb ggplot2
histograma = function(X) {
  ggplot() +  
    geom_histogram(aes(x = X, y= after_stat(density)), bins = 8, col = 'black', fill = "#E0E0E0") +
    geom_function(aes(col = 'Normal'), linewidth = 1,
                  fun = function(x) dnorm(x, mean(X), sd(X))) +
    geom_function(aes(col = 'Exponencial'), linewidth = 1,
                  fun = function(x) dexp(x, 1/mean(X))) +
    geom_function(aes(col = 'Uniforme'), linewidth = 1,
                  fun = function(x) dunif(x, min(X), max(X))) +
    labs(title = 'Histograma d\'X i densitats teòriques', 
         y = 'Densitat', col = 'Models')
}

grafic.pp = function(X) {
  dX = tibble(x = sort(X))
  dF = mutate(dX, Fx = sapply(x, function(x) mean(X <= x)))
  dF = dF %>%
    mutate(
      Fx_norm = pnorm(x, mean(X), sd(X)),
      Fx_exp = pexp(x, 1/mean(X)),  
      Fx_unif = punif(x, min(X), max(X)))
  ggplot(data = dF) +
    geom_point(aes(x = Fx_norm, y = Fx, col = 'Normal')) +
    geom_point(aes(x = Fx_exp, y = Fx, col = 'Exponencial')) +
    geom_point(aes(x = Fx_unif, y = Fx, col = 'Uniforme')) +
    geom_abline(intercept = 0, slope = 1, linetype = 'dotted') +
    labs(x = 'Funció de distribució teòrica', 
         y = 'Funció de distribució de la mostra', 
         col = 'Model')
}

grafic.qq = function(X) {
  dX = tibble(x = sort(X))
  dQ = mutate(dX, prob = ppoints(n()))
  dQ = mutate(dQ,
              qNorm = qnorm(prob, mean = mean(X), sd = sd(X)), 
              qExp = qexp(prob, 1/mean(X)), 
              qUnif = qunif(prob, min = min(X), max = max(X)))
  ggplot() +
    geom_point(data = dQ, aes(x = qNorm, y = x, col = 'Normal')) +
    geom_point(data = dQ, aes(x = qExp, y = x, col = 'Exponencial')) +
    geom_point(data = dQ, aes(x = qUnif, y = x, col = 'Uniforme')) +
    geom_abline(intercept = 0, slope = 1, linetype = 'dotted') +
    labs(col = 'Models', 
         x = 'Quantil teòric',
         y = 'Quantil mostral')
}

# generem les simulacions
simulacions = expand_grid('experiment' = 1:50, 'mida' = seq(10, 1000, 10))

algoritmes = tibble(algoritme = c("bubblesort","quicksort","mergesort"), 
                    estable = c(TRUE,FALSE,TRUE), 
                    implementacio.recursiva = c(FALSE,TRUE,TRUE))

simulacions = mutate(simulacions, 
                     'bubblesort' = pmap_dbl(list(experiment, mida), avalua_bubbleSort), 
                     'quicksort' = pmap_dbl(list(experiment, mida), avalua_quickSort), 
                     'mergesort' = pmap_dbl(list(experiment, mida), avalua_mergeSort), 
                     'ord.bubblesort' = pmap_dbl(list(experiment, mida, TRUE), avalua_bubbleSort), 
                     'ord.quicksort' = pmap_dbl(list(experiment, mida, TRUE), avalua_quickSort), 
                     'ord.mergesort' = pmap_dbl(list(experiment, mida, TRUE), avalua_mergeSort))

# guardem el conjunt de dades
save(simulacions, algoritmes, file = "simulacions.RData")
