#PACOTES------------
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
library(lubridate)



#IMPORTACAO DE BASES------------
  COVIDBR_2020_Parte1 = read.csv("C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR_2020_Parte1_02.csv", sep = ";")
  COVIDBR_2020_Parte2 = read.csv("C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR_2020_Parte2_02.csv", sep = ";")
  COVIDBR_2021_Parte1 = read.csv("C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR_2021_Parte1_02.csv", sep = ";")
  COVIDBR_2021_Parte2 = read.csv("C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR_2021_Parte2_02.csv", sep = ";")
  COVIDBR_2022_Parte1 = read.csv("C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR_2022_Parte1_02.csv", sep = ";")
  COVIDBR_2022_Parte2 = read.csv("C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR_2022_Parte2_02.csv", sep = ";")
  COVIDBR_2023_Parte1 = read.csv("C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR_2023_Parte1_02.csv", sep = ";")
  COVIDBR_2023_Parte2 = read.csv("C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR_2023_Parte2_02.csv", sep = ";")
  
  
  COVIDBR_RDATA = load(file = "C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\base_empilhada.RData")
  COVIDBR = readRDS(file = "C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR.rds")


#
  
  
#DIVIDIR VETOR EM FAIXAS------------
  faixas = 30
  EP_Semear$faixa_score <- cut(EP_Semear$score_tu, breaks = seq(min(EP_Semear$score_tu), max(EP_Semear$score_tu), faixas))
  
  EP_tabela <- EP_Semear %>%
    group_by(faixa_score) %>%
    summarise(count = n(),
              inadimplencia = sum(Mix) / count) %>%
    mutate(faixa_score = as.numeric(gsub(".*\\((.+)\\,.+\\]", "\\1", faixa_score)) + faixas / 2) 
  
  EP_tabela_Recompra %>% write.table('clipboard', sep = ';', row.names = F, dec = ',')
  EP_tabela_Novo %>% write.table('clipboard', sep = ';', row.names = F, dec = ',')


  
#EMPILHAR BASES------------
  base_empilhada <- bind_rows(COVIDBR_2020_Parte1,
                              COVIDBR_2020_Parte2,
                              COVIDBR_2021_Parte1,
                              COVIDBR_2021_Parte2,
                              COVIDBR_2022_Parte1,
                              COVIDBR_2022_Parte2,
                              COVIDBR_2023_Parte1,
                              COVIDBR_2023_Parte2,)
  



  
#ADICIONAR STRING------------
  Serasa_EP <- dplyr::rename_with(Serasa_EP, ~ paste0('SERASA_', .))
  names(Serasa_EP)
  
#CONVERSAO DE DATAS------------
EP_Backtest$SEMEAR_Data_Proposta = as.Date( EP_Backtest$SEMEAR_Data_Proposta, format = "%m/%d/%Y")
EP_Crivo$Data= as.Date(EP_Crivo$Data, format = "%m/%d/%Y")




#CRIANDO SAFRA------------
EP_Crivo = EP_Crivo %>% mutate(safra = format(Data, "%Y%m")) 




#PADRONIZANDO CPF------------
names(EP_Crivo)[names(EP_Crivo) == "CPF"] <- "cpf"
EP_Crivo$cpf = str_pad(EP_Crivo$cpf, 11, pad = "0")
EP_Backtest$cpf = str_pad(EP_Backtest$cpf, 11, pad = "0")




#PADRONIZANDO PROPOSTA------------
names(EP_Backtest)[names(EP_Backtest) == "SEMEAR_Proposta"] <- "Proposta"
names(EP_Crivo)[names(EP_Crivo) == "Proposta"] <- "Proposta"
class(EP_Backtest$Proposta)
class(EP_Crivo$Proposta)
EP_Crivo$Proposta = as.character(EP_Crivo$Proposta)
EP_Backtest$Proposta = as.character(EP_Backtest$Proposta)


EP_Crivo$Proposta = str_pad(EP_Crivo$Proposta, 11, pad = "0")
EP_Backtest$Proposta = str_pad(EP_Backtest$Proposta, 11, pad = "0")




#CRIANDO CKEY------------
EP_Crivo = EP_Crivo %>% mutate(key = paste0(Proposta,Data))
EP_Backtest = EP_Backtest %>% mutate(key = paste0(Proposta,SEMEAR_Data_Proposta))





#LEFT_JOIN------------
length(intersect(EP_Crivo$key,EP_Backtest$key))
length(intersect(EP_Crivo$Proposta,EP_Backtest$Proposta))
EP_Corte_v1 = left_join(EP_Backtest, EP_Crivo, by = c("Proposta"), na_matches = "never")
EP_Corte_v2 = left_join(EP_Backtest, EP_Crivo, by = c("key"), na_matches = "never")




#RENOMEANDO COLUNA------------
names(CDCE_Backtest)[names(CDCE_Backtest) == "SEMEAR_Proposta"] <- "Proposta"

BACKTEST_Enriquecida <- rename(BACKTEST_Enriquecida, 'Renda' = 'cliente_valorrenda')
BACKTEST_Enriquecida <- rename(BACKTEST_Enriquecida, 'Profissão' = 'origemrenda')
BACKTEST_Enriquecida <- rename(BACKTEST_Enriquecida, 'Idade' = 'cc_Cliente_Idade')


#

#EXCLUINDO COLUNAS------------
names(COVIDBRv1)
COVIDBRv1 = subset(COVIDBRv1, select = c(data,casosNovos))


#RETIRANDO NA'S-------------
names(BACKTEST_Enriquecida_Crivo)

EP_Corte_v2 = as.data.table(EP_Corte_v2)
setkey(EP_Corte_v2, Proposta.y)

EP_Corte_v3 <- EP_Corte_v2[complete.cases(EP_Corte_v2[, .(Proposta.y)])]



#REMOVENDO DUPLICADOS-------------
names(EP_Corte_v3)
duplicados <- EP_Corte_v2[duplicated(EP_Corte_v2[,c('Proposta.x')]),]
num_duplicados <- sum(duplicated(EP_Corte_v3[,c('Proposta.x')]))
print(duplicados) #linhas
print(num_duplicados) #qtde linhas

EP_Corte_v4 <- EP_Corte_v3[!duplicated(EP_Corte_v3$Proposta.x), ]


duplicados <- EP_Crivo[duplicated(EP_Crivo[,c('Proposta')]),]
num_duplicados <- sum(duplicated(EP_Crivo[,c('Proposta')]))
View(duplicados) #linhas
print(num_duplicados) #qtde linhas

EP_Crivo %>% group_by(Proposta) %>% count() %>% filter(n>1) %>%
  left_join(EP_Crivo, by = "Proposta") %>%
  View

duplicados %>% arrange(Data) %>% View()





#OPERADORES LOGICOS------------
 "E  = &"
 "OU = |"




 
#APLICAR FILTRO------------
 
#PRODUTO
COVIDBRv1 = subset(COVIDBR, COVIDBR$regiao == "Brasil")
EP_Corte_ccrec = subset(EP_Corte_v4, EP_Corte_v4$SEMEAR_Produto == "EP Recompra")

#DATA
EP_Corte_ccnew_JunJul2022 = EP_Corte_ccnew %>% filter(SEMEAR_Data_Proposta >= as.Date("2022-06-01") & 
                                                      SEMEAR_Data_Proposta <= as.Date("2022-07-31"))
EP_Corte_ccrec_JunJul2022 = EP_Corte_ccrec %>% filter(SEMEAR_Data_Proposta >= as.Date("2022-06-01") & 
                                                      SEMEAR_Data_Proposta <= as.Date("2022-07-31"))


#EXPORTACAO DE BASES------------
#------------GERAL------------#
write.csv(LEAD0_v2, file = "D:\\OneDrive - StepWise\\Projetos\\Banco JHFS\\Bases\\Reunião 06_10_2023\\Bases\\Enriquecidas\\LEAD0_v2.csv", row.names = FALSE)

#------------RDATA------------#
save(base_empilhada, file = "C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\base_empilhada.RData")

#------------RDS------------#
saveRDS(COVIDBRv1, file = "C:\\Users\\JulianoCesardaSilva\\Downloads\\Covid19\\COVIDBR.rds")

  







