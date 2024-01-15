################################################################################
#################################### PACOTES ###################################
################################################################################

{
  library(tidyr)
  library(dplyr)
  library(lubridate)
  library(openxlsx)
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
  options(digits = 8)
}

#
# IMPORTANDO RDATA
#

{
  setwd('D:\\OneDrive - StepWise\\Projetos\\TriBanco\\Exercício Modelagem\\')
  load('tb_Teste_FastModel.RData')
}

tb_juliano <- read.csv('tb_juliano_v2.csv')

#
# EXPORTANDO EM CSV
#
write.csv(CAT_tb_juliano, file = 'D:\\OneDrive - StepWise\\Projetos\\TriBanco\\Exercício Modelagem\\CAT_tb_juliano.csv', row.names = FALSE)

################################################################################
############################### TRATAMENTO DE DADOS ############################
################################################################################

{
  str(tb_juliano, list.len = ncol(tb_juliano)) %>%
    capture.output() %>%
    write.table('D:\\OneDrive - StepWise\\Projetos\\TriBanco\\Exercício Modelagem\\str_dados.csv', row.names = F)
}

{
  #
  # MUDANDO VARIÁVEIS
  #  
  tb_juliano <- tb_juliano %>%
    mutate(
      CD_PROPOSTA = as.character(CD_PROPOSTA),
      CPF_CNPJ = as.character(CPF_CNPJ),
      idade = as.numeric(idade),
      codigo_operacao = as.character(codigo_operacao),
      codigo_proposta = as.character(codigo_proposta),
      prop_cpf = as.character(prop_cpf),
      TELPROP_DDD_mFreq = as.character(TELPROP_DDD_mFreq),
      TELPROP_DDD_mFreq_EQ_p01 = as.character(TELPROP_DDD_mFreq_EQ_p01),
      qtd_EmprPag_tot = as.numeric(qtd_EmprPag_tot)
    )
  
  #
  # REMOVENDO VARIÁVEIS
  #
  
  tb_juliano <- subset(tb_juliano, select = -c(SAFRA,
                                               Ordem_Funil,
                                               PRODUTO,
                                               STATUS_PROPOSTA,
                                               AGRUP_STATUS_PROPOSTA,
                                               DESCR_STATUS_PROPOSTA,
                                               DESCR_CANAL,
                                               DESCR_PRODUTO,
                                               DESCR_TIPO_CARTAO,
                                               elegivel,
                                               codigo_operacao,
                                               codigo_proposta,
                                               sucesso,
                                               resultado,
                                               instante,
                                               prop_cpf,
                                               i_credlink,
                                               amostra_runif))
  
  
  
  #
  # DEPARA GRAU DE INSTRUÇÃO
  #
  
  tb_juliano <- tb_juliano %>%
    mutate(GRAUINSTRv2 = case_when(
      GRAUINSTR %in% c("DOUTORADO COMPLETO", "MESTRADO COMPLETO", "EDUCACAO SUPERIOR COMPLETA", "EDUCACAO SUPERIOR INCOMPLETA") ~ "Grupo 1",
      GRAUINSTR %in% c("ENSINO MEDIO COMPLETO") ~ "Grupo 2",
      GRAUINSTR %in% c("ENSINO FUNDAMENTAL COMPLETO", "ENSINO MEDIO INCOMPLETO") ~ "Grupo 3",
      GRAUINSTR %in% c("ALFABETIZADO", "ANALFABETO", "6º AO 9º ANO DO ENSINO FUNDAMENTAL INCOMPLETO", "5º ANO COMPLETO DO ENSINO FUNDAMENTAL") ~ "Grupo 4",
      TRUE ~ NA_character_
    ))
  
  tb_juliano = subset(tb_juliano, select = -c(GRAUINSTR))
}


################################################################################
################################### MODELAGEM ##################################
################################################################################

{
  #
  # Categoralização de variáveis
  #
  
  CAT_tb_juliano <-
    tb_juliano %>% 
    mutate(
      CON_idade = 
        as.factor(ifelse(is.na(idade),"C1",
                         ifelse(idade <= 29.05499999999999971578,"C1",
                                ifelse(idade <= 41.07500000000000284217,"C0",
                                       ifelse(idade <= 52.97499999999999431566,"C3",
                                              "C4"))))),
      
      CAT_TELPROP_DDD_mFreq = 
        as.factor(ifelse(is.na(TELPROP_DDD_mFreq) | trimws(TELPROP_DDD_mFreq) == "","C2",
                         ifelse(TELPROP_DDD_mFreq %in% c("95", "79", "71"),"C1",
                                ifelse(TELPROP_DDD_mFreq %in% c("98", "41", "81"),"C2",
                                       ifelse(TELPROP_DDD_mFreq %in% c("92", "83", "54", "93", "61", "82", "34", "42", "66"),"C3",
                                              ifelse(TELPROP_DDD_mFreq %in% c("74", "48", "43", "33", "51", "87", "84", "45", "64", "94", "28", "77", "31", "97", "22", "86", "12", "47", "99", "89", "35", "32", "37", "13", "46"),"C0",
                                                     ifelse(TELPROP_DDD_mFreq %in% c("85", "11", "67", "69", "38", "49", "27", "14"),"C5",
                                                            ifelse(TELPROP_DDD_mFreq %in% c("91", "68", "75", "53", "65", "16", "19", "73", "21", "88", "17"),"C6",
                                                                   "C7")))))))),
      
      
      CAT_TELPROP_sexo_mFreq =
        as.factor(ifelse(is.na(TELPROP_sexo_mFreq) | trimws(TELPROP_sexo_mFreq) == "","C0",
                         ifelse(TELPROP_sexo_mFreq %in% c("VALOR_FORA_DO_DOMINIO"), "C0",
                                ifelse(TELPROP_sexo_mFreq %in% c("M"), "C0",
                                       "C3")))),
      
      
      CON_RENDA_estimada =
        as.factor(ifelse(is.na(RENDA_estimada),"C0",
                         ifelse(RENDA_estimada <= 1529.005000000000109139,"C0",
                                ifelse(RENDA_estimada <= 2461.034999999999854481,"C2",
                                       "C3")))),
      
      CAT_CD_UF_NASCIMENTO_PROPONENTE =
        as.factor(ifelse(is.na(CD_UF_NASCIMENTO_PROPONENTE) | trimws(CD_UF_NASCIMENTO_PROPONENTE) == "","C1",
                         ifelse(CD_UF_NASCIMENTO_PROPONENTE %in% c("RR", "AP", "SE", "DF", "EX"),"C1",
                                ifelse(CD_UF_NASCIMENTO_PROPONENTE %in% c("RO", "PE"),"C2",
                                       ifelse(CD_UF_NASCIMENTO_PROPONENTE %in% c("AM", "BA", "AL"),"C3",
                                              ifelse(CD_UF_NASCIMENTO_PROPONENTE %in% c("SC", "MG", "RN", "MS", "MT"),"C0",
                                                     ifelse(CD_UF_NASCIMENTO_PROPONENTE %in% c("PA", "AC", "PB", "RS", "MA", "PR", "SP", "ES"),"C5",
                                                            ifelse(CD_UF_NASCIMENTO_PROPONENTE %in% c("CE", "RJ"),"C6",
                                                                   "C7")))))))),
      
      
      CON_RENDA_menor =
        as.factor(ifelse(is.na(RENDA_menor),"C0",
                         ifelse(RENDA_menor <= 4.104995000000000615614,"C0",
                                ifelse(RENDA_menor <= 340.4499999999999886313,"C1",
                                       ifelse(RENDA_menor <= 1101.929994999999962602,"C1",
                                              "C2"))))),
      
      CAT_SOC_tem_cnpj = as.factor(ifelse(is.na(SOC_tem_cnpj) | trimws(SOC_tem_cnpj) == "","C0",
                                          ifelse(SOC_tem_cnpj %in% c("0"),"C0",
                                                 "C2"))),
      
      CAT_TELPROP_qtd_whatsapp = as.factor(ifelse(is.na(TELPROP_qtd_whatsapp),"C0",
                                                  ifelse(TELPROP_qtd_whatsapp <= 1.5,"C0",
                                                         "C2"))),
      
      CAT_RISCO_LOJA_MOTOR_N = as.factor(ifelse(is.na(RISCO_LOJA_MOTOR_N) | trimws(RISCO_LOJA_MOTOR_N) == "","C0",
                                                ifelse(RISCO_LOJA_MOTOR_N %in% c("D"),"C0",
                                                       ifelse(RISCO_LOJA_MOTOR_N %in% c("C"),"C0",
                                                              ifelse(RISCO_LOJA_MOTOR_N %in% c("B"),"C0",
                                                                     ifelse(RISCO_LOJA_MOTOR_N %in% c("E"),"C0",        
                                                                            "C2")))))),
      
      
      CAT_GRAUINSTRv2 = as.factor(ifelse(is.na(GRAUINSTRv2) | trimws(GRAUINSTRv2) == "","C1",
                                         ifelse(GRAUINSTRv2 %in% c("Grupo 3", "Grupo 4"),"C1",
                                                ifelse(GRAUINSTRv2 %in% c("Grupo 2"),"C2",
                                                       "C0"))))
    )
  
  
  #
  # Recategoralização de variáveis
  #
  
  #CAT_TELPROP_DDD_mFreq
  levels(CAT_tb_juliano$CAT_TELPROP_DDD_mFreq)[levels(CAT_tb_juliano$CAT_TELPROP_DDD_mFreq) %in% 'C3'] = 'C0'
  levels(CAT_tb_juliano$CAT_TELPROP_DDD_mFreq)[levels(CAT_tb_juliano$CAT_TELPROP_DDD_mFreq) %in% 'C2'] = 'C1'
  levels(CAT_tb_juliano$CAT_TELPROP_DDD_mFreq)[levels(CAT_tb_juliano$CAT_TELPROP_DDD_mFreq) %in% 'C1'] = 'C0'
  levels(CAT_tb_juliano$CAT_TELPROP_DDD_mFreq)[levels(CAT_tb_juliano$CAT_TELPROP_DDD_mFreq) %in% 'C5'] = 'C0'
  
  #CAT_CD_UF_NASCIMENTO_PROPONENTE
  levels(CAT_tb_juliano$CAT_CD_UF_NASCIMENTO_PROPONENTE)[levels(CAT_tb_juliano$CAT_CD_UF_NASCIMENTO_PROPONENTE) %in% 'C7'] = 'C6'
  levels(CAT_tb_juliano$CAT_CD_UF_NASCIMENTO_PROPONENTE)[levels(CAT_tb_juliano$CAT_CD_UF_NASCIMENTO_PROPONENTE) %in% 'C6'] = 'C5'
  levels(CAT_tb_juliano$CAT_CD_UF_NASCIMENTO_PROPONENTE)[levels(CAT_tb_juliano$CAT_CD_UF_NASCIMENTO_PROPONENTE) %in% 'C5'] = 'C0'
  levels(CAT_tb_juliano$CAT_CD_UF_NASCIMENTO_PROPONENTE)[levels(CAT_tb_juliano$CAT_CD_UF_NASCIMENTO_PROPONENTE) %in% 'C2'] = 'C3'
  
}

CAT_tb_juliano %>% filter(amostra == "DES")%>% group_by(CON_RENDA_estimada) %>% summarise(qntd = n(), mean(FLAG_FPD2))


################################################################################
################################### ESTIMACAO ##################################
################################################################################

{
  #
  # GLM
  #
  modelo <- glm((1 - FLAG_FPD2) ~ 
                  CON_idade +
                  CAT_TELPROP_DDD_mFreq +
                  CAT_TELPROP_sexo_mFreq +
                  CON_RENDA_estimada +
                  CAT_CD_UF_NASCIMENTO_PROPONENTE+
                  CON_RENDA_menor +
                  CAT_SOC_tem_cnpj +
                  CAT_TELPROP_qtd_whatsapp +
                  CAT_RISCO_LOJA_MOTOR_N +
                  CAT_GRAUINSTRv2
                , family = binomial(link = 'logit')
                , data = CAT_tb_juliano
                , subset = (amostra == 'DES'));summary(modelo)
  
}

################################################################################
################################# FECHAMENTO MODELO ############################
################################################################################

{
  quebras <- 
    CAT_tb_juliano %>% 
    mutate(
      xbeta = predict(modelo, newdata = .)
      ,aux = trunc(500 + xbeta * (100/log(2))) 
      
    ) %>% 
    filter(amostra == 'DES') %>% 
    .$aux %>% 
    quantile(probs = c(0, 0.025, 0.07, 0.15, 0.3, 0.5, 0.7, 0.85, 0.93, 0.975, 1))
  # saveRDS(quebras, 'RDATA/quebras_behaviour_final.rds')
  
  quebras %>% cbind
  
  base_escorada <- 
    CAT_tb_juliano %>% 
    mutate(
      
      prob = predict(modelo, newdata = ., type = 'response')
      ,xbeta = -log( (1/prob) - 1 ) 
      ,aux = trunc(500 + xbeta * (100/log(2))) 
      
      ,score_final =
        ifelse( aux <= quebras[1] ,  0 ,
                ifelse( aux <= quebras[2] , (0   + ( ( aux - quebras[1] ) * ( 100 - 0    ) ) / (quebras[2]  - quebras[1] )),
                        ifelse( aux <= quebras[3] , (101 + ( ( aux - quebras[2] ) * ( 200 - 101  ) ) / (quebras[3]  - quebras[2] )),
                                ifelse( aux <= quebras[4] , (201 + ( ( aux - quebras[3] ) * ( 300 - 201  ) ) / (quebras[4]  - quebras[3] )),
                                        ifelse( aux <= quebras[5] , (301 + ( ( aux - quebras[4] ) * ( 400 - 301  ) ) / (quebras[5]  - quebras[4] )),
                                                ifelse( aux <= quebras[6] , (401 + ( ( aux - quebras[5] ) * ( 500 - 401  ) ) / (quebras[6]  - quebras[5] )),
                                                        ifelse( aux <= quebras[7] , (501 + ( ( aux - quebras[6] ) * ( 600 - 501  ) ) / (quebras[7]  - quebras[6] )),
                                                                ifelse( aux <= quebras[8] , (601 + ( ( aux - quebras[7] ) * ( 700 - 601  ) ) / (quebras[8]  - quebras[7] )),
                                                                        ifelse( aux <= quebras[9] , (701 + ( ( aux - quebras[8] ) * ( 800 - 701  ) ) / (quebras[9]  - quebras[8] )),
                                                                                ifelse( aux <= quebras[10], (801 + ( ( aux - quebras[9] ) * ( 900 - 801  ) ) / (quebras[10] - quebras[9] )),
                                                                                        ifelse( aux <= quebras[11], (901 + ( ( aux - quebras[10]) *  (1000 - 901 ) ) / (quebras[11] - quebras[10])), 1000 )))))))))))
      
      ,score_final = trunc(score_final))
  # ) %>% 
  # select(-aux)
  
  summary(base_escorada$score_final)
  hist(base_escorada$score_final)
  
  # concentracao
  base_escorada %>% 
    count(score_final) %>% 
    arrange(desc(n)) %>% 
    mutate(
      part = n/nrow(base_escorada)
    ) %>% 
    slice(1:10)
  
  #
  # Minimo para Report
  #
  # score_final
  # amostra
  # conceito
  # CATs + CONs
  # Variavel Chave: CPF, DOCUMENTO, CD_PROP, etc...
  #
  
  # Base para Report
  base_escorada_report_tmp <- 
    base_escorada %>% 
    select(
      CD_PROPOSTA,
      CPF_CNPJ,
      DT_INCL_d,
      FLAG_FPD2,
      CON_idade,
      CAT_TELPROP_DDD_mFreq,
      CAT_TELPROP_sexo_mFreq,
      CON_RENDA_estimada,
      CAT_CD_UF_NASCIMENTO_PROPONENTE,
      CON_RENDA_estimada,
      CAT_CD_UF_NASCIMENTO_PROPONENTE,
      CON_RENDA_menor,
      CAT_SOC_tem_cnpj,
      CAT_TELPROP_qtd_whatsapp,
      CAT_RISCO_LOJA_MOTOR_N,
      CAT_GRAUINSTRv2,
      amostra,
      prob,
      xbeta,
      aux,
      score_final,
      idade,
      PERF_RESTRITIVO,
      VEIC_qtd_renav_dist,
      VEIC_ano_mais_novo,
      VEIC_ano_mais_antigo,
      TELPROP_qtd_dist,
      TELPROP_qtd_oper_dist,
      TELPROP_qtd_whatsapp,
      TELPROP_qtd_DDD_dist,
      TELPROP_qtd_UF_dist,
      RENDA_estimada,
      RENDA_maior,
      RENDA_qtd_dist,
      RENDA_menor,
      RENDA_media,
      qtd_EmprPag_tot,
      qtd_EmprPag_dist,
      qtd_SA,
      qtd_LTDA,
      qtd_EIRELI,
      qtd_SA_recJud,
      qtd_LTDA_recJud,
      qtd_SA_falida,
      qtd_LTDA_falida,
      qtd_EIRELI_falida,
      qtd_falida,
      qtd_MUNICIPIO_ESTADO,
      PARENTE_qtd_dist,
      VIZINHOS_qtd_dist,
      PARENTE_qtd_dist
    ) 
  
  write.csv(base_escorada_report_tmp,
            file = "D:\\OneDrive - StepWise\\Projetos\\TriBanco\\Exercício Modelagem\\base_escorada_report_tmp.csv", row.names = FALSE)
}
