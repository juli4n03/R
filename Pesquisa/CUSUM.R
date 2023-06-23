#FASE 1
#-----------
dados_fase1 = c(9.45,7.99,9.29,11.66,12.16,10.18,8.04,11.46,9.20,10.34,9.03,11.47,
          10.51,9.40,10.08,9.37,10.62,10.31,8.52,10.84); dados_fase1


dados_fase1 = matrix(dados_fase1); dados_fase1
#------------


#FASE 2
#------------
dados_fase2 = c(10.90,9.33,12.29,11.50,
          10.60,11.08,10.38,11.62,11.31,10.52); dados_fase2


dados_fase2 = matrix(dados_fase2); dados_fase2
#------------


#SIMULAÇÃO
#----------

#Parametros
desvio_padrao = 40 
media_alvo = 10 #fixo
media_alvo2 = 11 
observacoes = 15
amostras = 10
k = 3 

set.seed(12)
dados_fase1 = matrix(rnorm((amostras*observacoes),
                       mean = media_alvo,
                       sd = desvio_padrao),
                 nrow = amostras, ncol = observacoes); dados_fase1

set.seed(11)
dados_fase2 = matrix(rnorm((amostras*observacoes),
                       mean = media_alvo2,
                       sd = desvio_padrao),
                 nrow = amostras, ncol = observacoes); dados_fase2

#---------

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


CUSUM(dados_fase1, dados_fase2, 10, 1, 3, 20, fase = "fase1")
CUSUM(dados_fase1, dados_fase2, 10, 1, 3, 20, fase = "fase2")
CUSUM(dados_fase1, dados_fase2, 10, 1, 3, 40, fase = "todas")



par(mfrow=c (1,1))


