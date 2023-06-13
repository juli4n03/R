
#MANIPULATION EM BASE DE DADOS
#-------------------------------------------
  
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
SEMEAR      = read.csv("C:/Users/JulianoCesardaSilva/Desktop/Backtest/Ponto de corte/Estudo 05062023/Base semear/Base_Backtest_Semear.csv", sep = ";")
BACKTEST    = read.csv("C:/Users/JulianoCesardaSilva/Desktop/Backtest/Ponto de corte/Estudo 05062023/Base backetest/CDCM_Semear_SPC_BVS_Serasa_QUODv2.csv", sep = ";")
Crivo       = read.csv("C:/Users/JulianoCesardaSilva/Desktop/Backtest/Ponto de corte/Estudo 05062023/Base crivo/CDCM-crivo_062022a082022.csv", sep = ";")
  #------------
  
  
  #EMPILHAR BASES
  #------------
base_empilhada <- bind_rows(Base_Backtest_2021,
                            Base_Backtest_2022)
  #------------
  
  #APLICAR FILTRO
  #------------
SEMEAR <- subset(SEMEAR, SEMEAR$Produto == "CDC-M")
  #------------
  
  
  #CONVERSÃO DE DATAS
  #------------
Crivo$Data = as.Date(Crivo$Data, format = "%m/%d/%Y")
BACKTEST$SEMEAR_Data_Proposta = as.Date(BACKTEST$SEMEAR_Data_Proposta, format = "%m/%d/%Y")
SEMEAR$Data.Operação = as.Date(SEMEAR$Data.Operação, format = "%d/%m/%Y")
  #------------
  
  
  #CRIANDO SAFRA
  #------------
Crivo$Safra = NULL

SEMEAR <- SEMEAR %>% mutate(safra = format(Data.Operação, "%Y%m"))
BACKTEST <- BACKTEST %>% mutate(safra = format(SEMEAR_Data_Proposta, "%Y%m"))
Crivo <- Crivo %>% mutate(safra = format(Data, "%Y%m"))
  #------------
  
  
  #RENOMEANDO COLUNA
  #------------
names(Crivo)[names(Crivo) == "CPF"] <- "cpf"

BACKTEST_Enriquecida <- rename(BACKTEST_Enriquecida, 'Renda' = 'cliente_valorrenda')
BACKTEST_Enriquecida <- rename(BACKTEST_Enriquecida, 'Profissão' = 'origemrenda')
BACKTEST_Enriquecida <- rename(BACKTEST_Enriquecida, 'Idade' = 'cc_Cliente_Idade')

  #------------
  
  
  #PADRONIZANDO CPF
  #------------
SEMEAR$cpf = str_pad(SEMEAR$cpf, 11, pad = "0")
BACKTEST$cpf = str_pad(BACKTEST$cpf, 11, pad = "0")
Crivo$cpf = str_pad(Crivo$cpf, 11, pad = "0")
  #------------
  
  
  #CRIANDO CKEY
  #------------
SEMEAR = SEMEAR %>% mutate(key = paste0(cpf,Data.Operação))
BACKTEST = BACKTEST %>% mutate(key = paste0(cpf,SEMEAR_Data_Proposta))
Crivo = Crivo %>% mutate(key = paste0(cpf,Data))
  #------------
  
  
  #LEFT_JOIN
  #------------
length(intersect(BACKTEST_Enriquecida$key, Crivo$key))
BACKTEST_Enriquecida =left_join(BACKTEST, SEMEAR, by = c("key"), na_matches = "never")
  #------------
  
  
  #EXCLUINDO_COLUNAS
  #------------
BACKTEST_Enriquecida$Proposta = NULL
BACKTEST_Enriquecida$Código.Produto = NULL
BACKTEST_Enriquecida$Varejista = NULL
BACKTEST_Enriquecida$Loja = NULL
BACKTEST_Enriquecida$Data.Operação = NULL
BACKTEST_Enriquecida$Produto = NULL
BACKTEST_Enriquecida$flag_status = NULL
BACKTEST_Enriquecida$Proposta.Aprovada = NULL
BACKTEST_Enriquecida$Status.Aprovação = NULL
BACKTEST_Enriquecida$Já.Cliente = NULL
BACKTEST_Enriquecida$Proposta.Aprovação = NULL
BACKTEST_Enriquecida$Contrato = NULL
BACKTEST_Enriquecida$Data.Aprovação = NULL
BACKTEST_Enriquecida$Ever30m2 = NULL
BACKTEST_Enriquecida$Ever30m3 = NULL
BACKTEST_Enriquecida$Ever60m4 = NULL
BACKTEST_Enriquecida$Ever60m6 = NULL
BACKTEST_Enriquecida$UF = NULL
BACKTEST_Enriquecida$Estado = NULL
BACKTEST_Enriquecida$Região = NULL
BACKTEST_Enriquecida$valortotal = NULL
BACKTEST_Enriquecida$qtdparcelas = NULL
BACKTEST_Enriquecida$Data = NULL
BACKTEST_Enriquecida$safra.y = NULL
BACKTEST_Enriquecida$cpf.y = NULL
  #------------ 
  
  
  #RETIRANDO NA'S
  #-------------
names(BACKTEST_Enriquecida_Crivo)

BACKTEST_Enriquecida_Crivo = as.data.table(BACKTEST_Enriquecida_Crivo)
setkey(BACKTEST_Enriquecida_Crivo, cpf)

BACKTEST_Enriquecida_Crivo_NAs <- BACKTEST_Enriquecida_Crivo[complete.cases(BACKTEST_Enriquecida_Crivo[, .(cpf)])]

length(intersect(BACKTEST_Enriquecida$key, Crivo$key))
  #-------------
  
  #REMOVENDO DUPLICADOS
  #-------------
BACKTEST_Enriquecida_Crivo_NAsv2 <- BACKTEST_Enriquecida_Crivo_NAs[!duplicated(BACKTEST_Enriquecida_Crivo_NAs$SEMEAR_Proposta), ]
  #-------------
  
  #EXPORTATION DE BASES
  #------------
write.csv(BACKTEST_Enriquecida_Crivo_NAsv2, file = "C:/Users/JulianoCesardaSilva/Desktop/Backtest/Ponto de corte/Estudo 05062023/CDCM_V.A(semear)_Crivo.csv")
  #------------

#-------------------------------------------







