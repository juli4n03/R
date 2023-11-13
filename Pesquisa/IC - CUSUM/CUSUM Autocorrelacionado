#-----PACOTES NECESSARIOS

library(MASS)
library(corrplot)
library(stats)
library(dplyr)
library(tidyr)
library(tseries)
library(forecast)
library(ggplot2)
library(gridExtra)



#-----FUNCOES

testes_acf <- function(data) {
  num_rows <- nrow(data)
  
  for (i in 1:num_rows) {
    row_name <- rownames(data)[i]
    cat("ACF da linha", i, "(", row_name, "):\n")
    acf(data[i, ], main = paste("ACF da linha", i, "(", row_name, ")"))
  }
}

gerar_amostra <- function(amostras, observacoes, ar_coef, media_alvo, sd) {
  # Verificar a estacionariedade dos coeficientes AR
  raizes <- polyroot(c(1, -ar_coef))
  raio <- abs(ar_coef)
  
  if (raio >= 1) {
    cat("Os coeficientes AR não são estacionários. Tente usar outros coeficientes.\n")
    return(NULL)
  }
  
  # Criando amostra
  base <- arima.sim(n = amostras, model = list(ar = ar_coef))
  
  # Criando matriz vazia
  matriz_vazia <- matrix(0, nrow = amostras, ncol = observacoes)
  matriz_vazia[, 1] <- base
  
  # Preenchendo matriz com colunas correlacionadas
  for (i in 2:observacoes) {
    noise <- rnorm(amostras, mean = media_alvo, sd = sd)
    matriz_vazia[, i] <- base + noise
  }
  
  return(matriz_vazia)
}


testes_independencia <- function(correlacao) {
  # Criar um data frame vazio com as colunas necessárias
  resultados <- data.frame(Linha1 = integer(),
                           Linha2 = integer(),
                           Metodo = character(),
                           Coeficiente = numeric(),
                           P_valor = numeric())
  
  # Obter o número de linhas da matriz
  n_linhas <- nrow(correlacao)
  linhas <- 1:n_linhas
  metodos <- c("pearson", "spearman", "kendall")
  
  # Iterar através das combinações de linhas e métodos de correlação
  for (i in linhas) {
    for (j in linhas) {
      if (i < j) {
        for (metodo in metodos) {
          teste <- cor.test(t(correlacao[i, ]), t(correlacao[j, ]), method = metodo)
          coeficiente <- teste$estimate
          p_valor <- teste$p.value
          
          # Armazenar os resultados no data frame
          resultados <- resultados %>%
            add_row(Linha1 = i,
                    Linha2 = j,
                    Metodo = metodo,
                    Coeficiente = coeficiente,
                    P_valor = p_valor)
        }
      }
    }
  }
  
  # Retornar os resultados em uma tabela
  return(resultados)
}

teste_estacionariedade <- function(data) {
  num_rows <- nrow(data)
  resultados <- data.frame(Serie = integer(),
                           Teste_ADF = character(),
                           P_valor = numeric())
  
  for (i in 1:num_rows) {
    adf_test <- adf.test(data[i, ])
    p_valor <- adf_test$p.value
    
    resultados <- resultados %>%
      add_row(Serie = i,
              Teste_ADF = ifelse(p_valor <= 0.05, "Estacionária", "Não estacionária"),
              P_valor = p_valor)
  }
  return(resultados)
}

analise_residuos <- function(data, ar_coef) {
  num_rows <- nrow(data)
  resultados <- data.frame(Serie = integer(),
                           Shapiro_test = character(),
                           P_valor = numeric())
  
  for (i in 1:num_rows) {
    ar_fit <- Arima(data[i, ], order = c(length(ar_coef), 0, 0))
    residuos <- ar_fit$residuals
    shapiro_test <- shapiro.test(residuos)
    p_valor <- shapiro_test$p.value
    
    resultados <- resultados %>%
      add_row(Serie = i,
              Shapiro_test = ifelse(p_valor <= 0.05, "Não normal", "Normal"),
              P_valor = p_valor)
  }
  return(resultados)
}

estabilidade_coefs <- function(ar_coef) {
  raizes <- polyroot(c(1, -ar_coef))
  raio <- max(Mod(raizes))
  
  if (raio < 1) {
    cat("Os coeficientes são estáveis.\n")
  } else {
    cat("Os coeficientes não são estáveis.\n")
  }
}

analise_sensibilidade <- function(amostras_range, observacoes_range, ar_coef_list, media_alvo_range, sd_range) {
  resultados <- data.frame(Num_amostras = integer(),
                           Num_observacoes = integer(),
                           AR_coef = character(),
                           Media_alvo = numeric(),
                           SD = numeric(),
                           Serie = integer(),
                           Teste_ADF = character(),
                           P_valor = numeric())
  
  for (amostras in amostras_range) {
    for (observacoes in observacoes_range) {
      for (ar_coef in ar_coef_list) {
        for (media_alvo in media_alvo_range) {
          for (sd in sd_range) {
            matriz_amostras <- gerar_amostra(amostras, observacoes, ar_coef, media_alvo, sd)
            
            if (!is.null(matriz_amostras)) {
              estacionariedade <- teste_estacionariedade(matriz_amostras)
              estacionariedade$Num_amostras <- amostras
              estacionariedade$Num_observacoes <- observacoes
              estacionariedade$AR_coef <- paste(ar_coef, collapse = ", ")
              estacionariedade$Media_alvo <- media_alvo
              estacionariedade$SD <- sd
              resultados <- rbind(resultados, estacionariedade)
            }
          }
        }
      }
    }
  }
  
  return(resultados)
}



CUSUM <- function(dados_fase1, dados_fase2, media_alvo, k, amostras, fase = "todas") {
  
  if (fase == "fase1") {
    matriz <- as.matrix(dados_fase1)
  } else if (fase == "fase2") {
    matriz <- as.matrix(dados_fase2)
  } else if (fase == "todas") {
    matriz <- as.matrix(dados_fase1)
    matriz2 <- as.matrix(dados_fase2)
    matriz <- rbind(matriz, matriz2)
  } else {
    stop("Fase inválida! Escolha 'fase1', 'fase2' ou 'todas'")
  }
  
  # Verifica o número de colunas da matriz
  if (ncol(matriz) == 1) {
    
    # Vetor com o valor-alvo
    z <- matrix(rep(media_alvo, amostras), nrow = amostras, ncol = 1)
    
    # Calculando a soma cumulativa
    dif <- matriz - z
    soma_acu <- cumsum(dif)
    
    # Cálculo dos limites
    desvio_padrao <- sd(matriz)
    LSC <- k * desvio_padrao
    LIC <- -k * desvio_padrao
    
  } else {
    
    #Calculando a média de cada linha
    media_linhas <- apply(matriz, 1, mean)
    
    #Vetor com o valor-alvo
    z <- rep(media_alvo, nrow(matriz))
    
    #Calculando a soma cumulativa
    dif = media_linhas - z; dif
    soma_acu = cumsum(dif); soma_acu
    
    #Cálculo dos limites
    desvio_padrao <- sd(as.vector(matriz))
    LSC = k * (desvio_padrao/sqrt(ncol(matriz))) ; LSC
    LIC = -k * (desvio_padrao/sqrt(ncol(matriz))) ; LIC
    
  }
  
  # Criar um dataframe com os dados
  df <- data.frame(Amostras = 1:length(soma_acu),
                   "Soma cumulativa" = soma_acu,
                   LSC = rep(LSC, length(soma_acu)),
                   LIC = rep(LIC, length(soma_acu)))
  
  # Criar um gráfico usando ggplot
  p <- ggplot(df, aes(x = Amostras, y = `soma_acu`)) +
    geom_line(col = "black") +
    geom_point(col = "black") +
    geom_line(aes(y = LSC), col = "red", linetype = "dashed") +
    geom_line(aes(y = LIC), col = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = "Gráfico de Soma Cumulativa",
         x = "Amostras",
         y = "Soma cumulativa")
  
  # Mostrando LSC e LIC no console
  cat("LSC =", round(LSC, 2), "\n")
  cat("LIC =", round(LIC, 2), "\n")
  
  # Retorna o gráfico
  return(p)
}

#-------------AMOSTRA


set.seed(8)
num_samples <- 5
num_observations <- 10

#FASE 1
#Força da correlação
Sigma <- matrix(0.99, num_observations, num_observations) + diag(0.5, num_observations)
data <- mvrnorm(n = num_samples, mu = rep(10, num_observations), Sigma = Sigma)
dados_fase1 <- as.data.frame(data); dados_fase1

#FASE 2
data2 <- mvrnorm(n = num_samples, mu = rep(12, num_observations), Sigma = Sigma)
dados_fase2 <- as.data.frame(data2); dados_fase2
amostra = rbind(dados_fase1,dados_fase2); amostra


#Análise de Diagnóstico
testes_independencia(amostra)


#-----------
pairs(amostra, main = "Gráfico de Pares da Matriz de Correlação")

corrplot(cor(amostra), method = "circle", type = "upper", tl.col = "black",
         tl.srt = 45, tl.cex = 0.8, cl.cex = 0.8, addCoef.col = "black")

CUSUM(dados_fase1, dados_fase2, 10, 3, 5, "fase1")
CUSUM(dados_fase1, dados_fase2, 10, 3, 5, "fase2")
CUSUM(dados_fase1, dados_fase2, 10, 3, 10, "todas")




