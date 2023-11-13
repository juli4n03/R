#PACOTES NECESSARILY
#------------
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(openxlsx)
library(stringi)
library(stringr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
#------------


#IMPORTAÇÃO DE BASES
#------------
DadosDengue = read.csv("C:\\Users\\JulianoCesardaSilva\\Downloads\\DadosDenguev2.csv", sep = ";")

names(DadosDengue)
#------------

#SIMULAÇÃO
#------------
# Função CUSUM com h e k variando ao longo do tempo
calculate_CUSUM_dynamic <- function(X, h, k) {
  n <- length(X)
  C <- numeric(n)
  for (t in 2:n) {
    C[t] <- max(0, C[t - 1] + X[t] - h[t] - k[t])
  }
  return(C)
}

# Função para calcular k_t com base na fórmula proposta
calculate_kt <- function(lambda0, lambda1) {
  return((lambda0 - lambda1) / log(lambda1 / lambda0))
}

# Função para gerar uma sequência de h_t para um valor desejado de ARL_0
generate_ht <- function(n, start_value, end_value) {
  return(seq(from = start_value, to = end_value, length.out = n))
}

# Definindo parâmetros
lambda0 <- 5
lambda1 <- 10
h_start <- 7
h_end <- 3
n <- 100

# Simulando dados de Poisson
lambda1 <- 5 # média sob controle
lambda2 <- 10 # média após mudança
n <- 100 # número de observações
change_point <- 50 # ponto de mudança

# Gerando a série temporal
X <- c(rpois(change_point, lambda1), rpois(n - change_point, lambda2))

# Calculando h_t e k_t
h_t <- generate_ht(n, h_start, h_end)
k_t <- rep(calculate_kt(lambda1, lambda2), n)

# Calculando CUSUM
C <- calculate_CUSUM_dynamic(X, h_t, k_t)

# Plotando resultados
plot(C, type = "l", main = "CUSUM Chart", xlab = "Time", ylab = "CUSUM")
abline(h = 0, col = "red", lty = 2)

# Adicionando o limite de decisão (por exemplo, o dobro da média da série temporal)
decision_limit <- 2 * mean(X)
abline(h = decision_limit, col = "blue", lty = 2)
#------------


#APLICAÇÃO
#------------
DadosDengue <- DadosDengue %>%
  arrange(Ano, Mês)

X <- DadosDengue$Casos


#PARÂMETROS
lambda0 <- mean(X[1:50])# média antes da mudança
lambda1 <- mean(X[51:length(X)])  # média após mudança
h_start <- 7  #valor inicial para h
h_end <- 3  #valor final para h


h_t <- generate_ht(length(X), h_start, h_end)
k_t <- rep(calculate_kt(lambda1, lambda2), length(X))

#CUSUM
C <- calculate_CUSUM_dynamic(X, h_t, k_t)

#GRÁFICO
plot(C, type = "l", main = "Casos de dengue", xlab = "Tempo", ylab = "CUSUM")
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(2, 1))
plot(X[1:60])

#LIMITES
decision_limit <- 3 * sd(X)
abline(h = decision_limit, col = "blue", lty = 2)
#------------
