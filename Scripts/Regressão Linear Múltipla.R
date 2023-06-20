################################################################################
########################## REGRESSÃO MULTIPLA ##################################
################################################################################


# Chamando bibliotecas necess?rias
library(ggplot2) 
library(ggthemes)
library(GGally)
library(readr)
library(readxl)
library(openxlsx)
library(stringi)
library(stringr)
library(nortest)
library(PMCMRplus) 
library(car)
library(caTools)
library(corrplot)
library(tidyverse)
library(dplyr)
library(lazyeval)
library(qqplotr)
library(gridExtra)
library(olsrr)
library(stats)



pairs(dados, main = "Gráfico de Pares da Matriz de Correlação")

corrplot(cor(dados), method = "circle", type = "upper", tl.col = "black",
         tl.srt = 45, tl.cex = 0.8, cl.cex = 0.8, addCoef.col = "black")


################################################################################
################################################################################
# ORGANIZANDO O BANCO DE DADOS

# Importando os dados
url = "http://www.statsci.org/data/general/sleep.txt"
dados = read.table(url, sep = "\t", header = TRUE)


# Renomenado as variáveis
attach(dados)
names(dados) = c("Especies", "Peso_Corporal", "Peso_Cerebral", "Nao_Sonhando", 
                 "Sonhando", "Sono_Total", "Tempo_Vida_Util", "Tempo_Gestacao",
                 "Indice_Predacao", "Indice_Exposicao", "Indice_Perigo")


# Arrumando a classe das variáveis
sapply(dados, class)
dados = dados[ , c(-1)]
dados[ , c(8,9,10)] = lapply(dados[ , c(8, 9, 10)], as.factor)
summary(dados)



# Substituindo NA's pela média
dadosMedia = dados %>% 
  mutate(
    Sono_Total = coalesce(Sono_Total, mean(Sono_Total, na.rm = TRUE)),
    Tempo_Vida_Util = coalesce(Tempo_Vida_Util,mean(Tempo_Vida_Util,na.rm = TRUE)),
    Tempo_Gestacao = coalesce(Tempo_Gestacao, mean(Tempo_Gestacao, na.rm = TRUE)),
    Nao_Sonhando = coalesce(Nao_Sonhando, mean(Nao_Sonhando, na.rm = TRUE)),
    Sonhando = coalesce(Sonhando, mean(Sonhando, na.rm = TRUE)))

summary(dadosMedia)
sum(is.na(dadosMedia))



################################################################################
################################################################################
# ANÁLISE DESCRITIVA E EXPLORATÓRIA DE DADOS

# Boxplot para Peso Corporal
ggplot(dadosMedia, aes(x = Peso_Corporal)) +
  geom_boxplot(fill = "lightpink", alpha = 0.6, lwd = 0.7) +
  ylim(-0.7, 0.7) +
  coord_flip() +
  labs(title = "Boxplot para peso corporal de 62 espécies de mamíferos", x = "Kg",
       y = "Peso Corporal") +
  # move the title text to the middle
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())


# Boxplot para Peso Cerebral
ggplot(dadosMedia, aes(x = Peso_Cerebral)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.6) +
  ylim(-0.7, 0.7) +
  coord_flip() +
  labs(title = "Boxplot para peso cerebral de 62 espécies de mamíferos", x = "kg",
       y = "Peso Cerebral") +
  # move the title text to the middle
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())


# Boxplot para Não Sonhando
ggplot(dadosMedia, aes(x = Nao_Sonhando)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +
  ylim(-0.7, 0.7) +
  coord_flip() +
  labs(title = "Boxplot para período de tempo que ocorre o sono de ondas lentas (sem sonho)", x = "Horas/Dia",
       y = "Sem Sonho") +
  # move the title text to the middle
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())


# Boxplot para Sonhando
ggplot(dadosMedia, aes(x = Sonhando)) +
  geom_boxplot(fill = "lightSalmon", alpha = 0.6) +
  ylim(-0.7, 0.7) +
  coord_flip() +
  labs(title = "Boxplot para período de tempo que ocorre o sono paradoxal (com sonho)", x = "Horas/Dia",
       y = "Sonhando") +
  # move the title text to the middle
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())


# Boxplot para Sono Total
ggplot(dadosMedia, aes(x = Sono_Total)) +
  geom_boxplot(fill = "LightGoldenrodYellow", alpha = 0.6, width = 0.1) +
  ylim(-0.1, 0.1) +
  coord_flip() +
  labs(title = "Boxplot para a soma do período de tempo que ocorre o sono de ondas lentas e o sono paradoxal", x = "Horas/Dia",
       y = "Sono Total") +
  # move the title text to the middle
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())


# Boxplot para Tempo de Vida Útil
ggplot(dadosMedia, aes(x = Tempo_Vida_Util)) +
  geom_boxplot(fill = "lightpink", alpha = 0.6, lwd = 0.7) +
  ylim(-0.7, 0.7) +
  coord_flip() +
  labs(title = "Boxplot para tempo de vida útil máxima para 62 espécies de mamíferos", x = "Anos",
       y = "Tempo Vida Útil") +
  # move the title text to the middle
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())


# Boxplot para Gestação
ggplot(dadosMedia, aes(x = Tempo_Gestacao)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.6) +
  ylim(-0.7, 0.7) +
  coord_flip() +
  labs(title = "Boxplot para período de gravidez para 62 espécies de mamíferos", x = "Dias",
       y = "Tempo de Gestação") +
  # move the title text to the middle
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())



# Separando as variáveis em classes
classes = sapply(dadosMedia, class)

## Variáveis Contínuas
vars_continuas <- dadosMedia[,(classes %in% c('numeric', 'integer'))]
names(vars_continuas)
summary(vars_continuas)
sapply(vars_continuas, function(x) sum(is.na(x)))

#gráfico
#--------------
dados_long <- reshape2::melt(dados)

ggplot(dados_long, aes(x = reorder(variable, value, FUN = median), y = value, fill = variable)) + 
  geom_boxplot(alpha = 0.6, outlier.shape = 21, outlier.fill = "red", outlier.colour = "black", outlier.size = 2.5) +
  labs(title = "Distribuição das Variáveis", x = "Variável", y = "Valor") +
  scale_fill_brewer(palette = "Set3") +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


pairs(cor(vars_continuas), main = "Gráfico de Pares da Matriz de Correlação")

corrplot(cor(vars_continuas), method = "circle", type = "upper", tl.col = "black",
         tl.srt = 45, tl.cex = 0.8, cl.cex = 0.8, addCoef.col = "black")

#--------------

#identificando outliers

#PESO_CORPORAL
#--------------
peso_corporal_out <- vars_continuas$Peso_Corporal
var = peso_corporal_out
Q1 = quantile(var, 0.25)
Q3 = quantile(var, 0.75)
IQR = Q3 = Q1
outliers_corporal = var[var < (Q1 - 1.5*IQR) | var > (Q3 + 1.5*IQR)];outliers_corporal
sort(outliers_corporal, decreasing = TRUE)
#--------------

#PESO_Cerebral
#--------------
peso_cerebral_out <- vars_continuas$Peso_Cerebral
var = peso_cerebral_out
Q1 = quantile(var, 0.25)
Q3 = quantile(var, 0.75)
IQR = Q3 = Q1
outliers_cerebral = var[var < (Q1 - 1.5*IQR) | var > (Q3 + 1.5*IQR)];outliers_cerebral
sort(outliers_cerebral, decreasing = TRUE)
#--------------


print(cor(dados$Peso_Corporal,dados$Peso_Cerebral))
plot(dados$Peso_Corporal,dados$Peso_Cerebral, main = "Peso Corporal em relação ao Peso Cerebral")


# Variáveis Categóricas
vars_categoricas = dados[,!(classes %in% c('numeric', 'integer'))]
table(vars_categoricas$Indice_Predacao)
table(vars_categoricas$Indice_Exposicao)
table(vars_categoricas$Indice_Perigo)

#grafico
#--------------
#Predação
predacao <- data.frame(
  Indice = as.numeric(c(1, 2, 3, 4, 5)),
  Quantidade = c(14, 15, 12, 7, 14),
  Proporcao = c(22.5, 24.19, 19.35, 11.29, 22.58)
)

#Exposição
exposicao <- data.frame(
  Indice = as.numeric(c(1, 2, 3, 4, 5)),
  Quantidade = c(27, 13, 4, 5, 13),
  Proporcao = c(43.54, 20.96, 6.45, 8.06, 20.96)
)

#Perigo
perigo <- data.frame(
  Indice = as.numeric(c(1, 2, 3, 4, 5)),
  Quantidade = c(19, 14, 10, 10, 9),
  Proporcao = c(30.64, 22.58, 16.12, 16.12, 14.51)
)

grafico <- function(dados, titulo){
  ggplot(data = dados, aes(x = factor(Indice), y = Proporcao, fill = Indice)) + 
    geom_bar(stat = "identity") +
    scale_fill_gradientn(colours = rainbow(2)) +
    geom_text(aes(label=Proporcao), vjust=1.6, color="black", size=3.5) +
    labs(title = titulo, x = "Índice", y = "Proporção (%)") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"))
}


g1 = grafico(predacao, "Predação")
g2 = grafico(exposicao, "Exposição")
g3 = grafico(perigo, "Perigo")
grid.arrange(g1, g2, g3, nrow = 3)

#--------------


# Tabelas Cruzadas entre as variáveis categóricas
table(vars_categoricas$Indice_Predacao, vars_categoricas$Indice_Exposicao)
table(vars_categoricas$Indice_Predacao, vars_categoricas$Indice_Perigo)
table(vars_categoricas$Indice_Exposicao, vars_categoricas$Indice_Perigo)




# Gráfico de dispers?o para Peso Corporal
ggplot(dadosMedia, aes(y = Sono_Total, x = Peso_Corporal)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Peso corporal em kg") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Peso Cerebral
ggplot(dadosMedia, aes(y = Sono_Total, x = Peso_Cerebral)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Peso cerebral em kg") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Não Sonhando
ggplot(dadosMedia, aes(y = Sono_Total, x = Nao_Sonhando)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Tempo de sono de ondas lentas em horas/dia") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia")  #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Sonhando 
ggplot(dadosMedia, aes(y = Sono_Total, x = Sonhando)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Tempo de sono paradoxal em horas/dia") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Tempo de Vida útil
ggplot(dadosMedia, aes(y = Sono_Total, x = Tempo_Vida_Util)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Tempo de vida útil máxima em anos") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Tempo dre Gestação
ggplot(dadosMedia, aes(y = Sono_Total, x = Tempo_Gestacao)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Tempo de gestação em dias") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia")  #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


'OBS: OS GRÁFICOS DE DISPERSÃO PARA OS INDICES FIZ POR FAZER, MAS NÃO ACHO QUE 
COLOCAMOS NO TRABALHO PQ É UMA VARIÁVEL CATEGÓRICA'
# Gr?fico de dispers?o para Indice de Predação
ggplot(dadosMedia, aes(y = Sono_Total, x = Indice_Predacao)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Indice de predação") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Indice de Exposição
ggplot(dadosMedia, aes(y = Sono_Total, x = Indice_Exposicao)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Indice de exposição") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Indice de Perigo
ggplot(dadosMedia, aes(y = Sono_Total, x = Indice_Perigo)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Indice de perigo") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o



#Gráfico de dispersão com as respectivas correlações:
ggpairs(dadosMedia,
        upper = list(continuous = wrap(ggally_cor, digits = 4, size = 11)))



# Avaliando Multicolinearidade entre as variáveis 
n = 62
Peso_Corporal_p = (1/sqrt(n-1))*(dadosMedia$Peso_Corporal- mean(dadosMedia$Peso_Corporal))/sqrt(var(dadosMedia$Peso_Corporal))
Peso_Cerebral_p = (1/sqrt(n-1))*(dadosMedia$Peso_Cerebral - mean(dadosMedia$Peso_Cerebral))/sqrt(var(dadosMedia$Peso_Cerebral))
Nao_Sonhando_p = (1/sqrt(n-1))*(dadosMedia$Nao_Sonhando - mean(dadosMedia$Nao_Sonhando))/sqrt(var(dadosMedia$Nao_Sonhando))
Sonhando_p = (1/sqrt(n-1))*(dadosMedia$Sonhando - mean(dadosMedia$Sonhando))/sqrt(var(dadosMedia$Sonhando))
Sono_Total_p = (1/sqrt(n-1))*(dadosMedia$Sono_Total - mean(dadosMedia$Sono_Total))/sqrt(var(dadosMedia$Sono_Total))
Tempo_Vida_Util_p = (1/sqrt(n-1))*(dadosMedia$Tempo_Vida_Util - mean(dadosMedia$Tempo_Vida_Util))/sqrt(var(dadosMedia$Tempo_Vida_Util))
Tempo_Gestacao_p = (1/sqrt(n-1))*(dadosMedia$Tempo_Gestacao - mean(dadosMedia$Tempo_Gestacao))/sqrt(var(dadosMedia$Tempo_Gestacao))

# Dados utilizados na matriz de correlações 
#Não adicionei os Indices, pois não é possivel fazer correlação de variável categórica
dados_p = cbind(Peso_Corporal_p, Peso_Cerebral_p, Nao_Sonhando_p, Sonhando_p, 
                  Sono_Total_p, Tempo_Vida_Util_p, Tempo_Gestacao_p)
dados_p = as.data.frame(dados_p)

# Dados utilizados na matriz de VIF 
#Não adicionei os Indices, pois não é possivel fazer correlação de variável categórica
X_p = cbind(Peso_Corporal_p, Peso_Cerebral_p, Nao_Sonhando_p, Sonhando_p, 
            Tempo_Vida_Util_p, Tempo_Gestacao_p)
X_p = as.data.frame(X_p)
  
# Matriz de correlações entre as todas as variáveis padronizadas 
corxy = cor(dados_p); corxy
  
# Matriz de VIF
rxx = cor(X_p)
rxx_1 = solve(rxx); rxx_1


# Retirando as variáveis Não sonhando e Sonhando pq Sono total = SOnhando + Não sonhando
# Transformando as variáveis peso corpora e peso cerebral em uma proporção
Corpo_Cerebro = Peso_Corporal_p/Peso_Cerebral_p

dados_s_multi = cbind(Corpo_Cerebro, Sono_Total_p, Tempo_Vida_Util_p, Tempo_Gestacao_p)
dados_s_multi = as.data.frame(dados_s_multi)
  
X_s_multi = cbind(Corpo_Cerebro, Tempo_Vida_Util_p, Tempo_Gestacao_p)
X_s_multi = as.data.frame(X_s_multi)

# Matriz de correlações entre as todas as variáveis padronizadas 
corxy_2 = cor(dados_s_multi); corxy_2

# Matriz de VIFF
rxx_2 = cor(X_s_multi)
rxx_1_2 = solve(rxx_2); rxx_1_2



# Analisando multicolineariedade pelo R^2

# Arruamndo o banco de dados com as novas variáveis
dados_final = cbind(Corpo_Cerebro, Sono_Total_p, Tempo_Vida_Util_p, Tempo_Gestacao_p, 
                    dadosMedia$Indice_Predacao, dadosMedia$Indice_Exposicao, 
                    dadosMedia$Indice_Perigo)
dados_final = as.data.frame(dados_final)

attach(dados_final)
names(dados_final) = c("Corpo_Cerebro", "Sono_Total_p", "Tempo_Vida_Util_p", "Tempo_Gestacao_p",
                       "Indice_Predacao", "Indice_Exposicao", "Indice_Perigo")

# Arrumando a classe das variáveis
sapply(dados, class)
dados_final$Indice_Predacao = as.factor(dados_final$Indice_Predacao)
dados_final$Indice_Exposicao = as.factor(dados_final$Indice_Exposicao)
dados_final$Indice_Perigo = as.factor(dados_final$Indice_Perigo)
sapply(dados, class)


modelo1 = lm(Corpo_Cerebro ~ Tempo_Vida_Util_p + Tempo_Gestacao_p + Indice_Perigo + 
             Indice_Exposicao + Indice_Predacao, data = dados_final)

modelo2 = lm(Tempo_Vida_Util_p ~ Corpo_Cerebro + Tempo_Gestacao_p + Indice_Perigo + 
             Indice_Exposicao + Indice_Predacao, data = dados_final)

modelo3 = lm(Tempo_Gestacao_p ~ Corpo_Cerebro + Tempo_Vida_Util_p + Indice_Perigo + 
              Indice_Exposicao + Indice_Predacao, data = dados_final)

modelo4 = lm(Indice_Perigo ~ Corpo_Cerebro + Tempo_Vida_Util_p + Tempo_Gestacao_p +
             Indice_Exposicao + Indice_Predacao, data = dados_final)

modelo5 = lm(Indice_Exposicao ~ Corpo_Cerebro + Tempo_Vida_Util_p + Tempo_Gestacao_p +
             Indice_Perigo + Indice_Predacao, data = dados_final)

modelo6 = lm(Indice_Predacao ~ Corpo_Cerebro + Tempo_Vida_Util_p + Tempo_Gestacao_p +
               Indice_Perigo + Indice_Exposicao, data = dados_final)

summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)
summary(modelo5)
summary(modelo6)



################################################################################
################################################################################
# Ajustando o modelo
modelo_completo = lm(Sono_Total_p ~ Corpo_Cerebro + Tempo_Vida_Util_p + Tempo_Gestacao_p +
                       Indice_Perigo + Indice_Exposicao + Indice_Predacao, data = dados_final)
modelo_completo
summary(modelo_completo)
anova(modelo_completo)



################################################################################
################################################################################
# ANALISE DE DIAGNÓSTICO
modelo_completo = lm(Sono_Total_p ~ Corpo_Cerebro + Tempo_Vida_Util_p + Tempo_Gestacao_p +
                       Indice_Perigo + Indice_Exposicao + Indice_Predacao, data = dados_final)
summary(modelo_completo)


residuos = modelo_final$residuals/(sqrt(0.09914 ))


g1 = ggplot(dados_final, aes(y =..density..,residuos)) +
  geom_histogram(bins = 18, fill = 'light blue',
                 color ='black', aes(y = ..density..)) +
  geom_density(alpha = .1)+
  xlab(label = "Resíduos")+
  ylab(label = "Densidade")


g2 = ggplot(data = dados_final, mapping = aes(sample = residuos)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Quantis Teóricos", y = "Quantis Amostrais")

grid.arrange(g1,g2,ncol=2)


lillie.test(residuos)
shapiro.test(residuos)
xb = mean(residuos)
sx = sd(residuos)
ks.test(residuos, "pnorm", xb, sx, alternative = 'two.sided')
ad.test(residuos)


ggplot(dados_final, aes(y = residuos, x = modelo_completo$fitted.values)) + 
  geom_point() +
  xlab(label = "Valores Preditos")+
  ylab(label = "Resíduos")


################################################################################
################################################################################
# Selecão de modelos
k = ols_step_all_possible(modelo_completo)
plot(k)

stepAIC(modelo_completo, direction = "both")
stepAIC(modelo_completo, direction = "backward")
stepAIC(modelo_completo, direction = "forward")



################################################################################
################################################################################
# ANALISE DE DIAGNÓSTICO
modelo_final = lm(Sono_Total_p ~ Tempo_Vida_Util_p + Tempo_Gestacao_p +
                    Indice_Perigo, data = dados_final)
summary(modelo_final)


residuos = modelo_final$residuals/(sqrt(0.4909))


g1 = ggplot(dados_final, aes(y =..density..,residuos)) +
  geom_histogram(bins = 18, fill = 'light blue',
                 color ='black', aes(y = ..density..)) +
  geom_density(alpha = .1)+
  xlab(label = "Resíduos")+
  ylab(label = "Densidade")


g2 = ggplot(data = dados_final, mapping = aes(sample = residuos)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Quantis Teóricos", y = "Quantis Amostrais")

grid.arrange(g1,g2,ncol=2)


lillie.test(residuos)
shapiro.test(residuos)
xb = mean(residuos)
sx = sd(residuos)
ks.test(residuos, "pnorm", xb, sx, alternative = 'two.sided')
ad.test(residuos)


ggplot(dados_final, aes(y = residuos, x = modelo_final$fitted.values)) + 
  geom_point() +
  xlab(label = "Valores Preditos")+
  ylab(label = "Resíduos")


ggplot(dados_final, aes(y = residuos, x = Tempo_Vida_Util_p)) + 
  geom_point() +
  xlab(label = "Tempo de vida útil máxima em anos")+
  ylab(label = "Resíduos")


ggplot(dados_final, aes(y = residuos, x = Tempo_Gestacao_p)) + 
  geom_point() +
  xlab(label = "Tempo de gestação em dias")+
  ylab(label = "Resíduos")

ggplot(dados_final, aes(y = residuos, x = Indice_Perigo)) + 
  geom_point() +
  xlab(label = "Indice de perigo")+
  ylab(label = "Resíduos")

# Gr?fico de dispers?o para Tempo de Vida útil
ggplot(dados_final, aes(y = Sono_Total_p, x = Tempo_Vida_Util_p)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Tempo de vida útil máxima em anos") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Tempo de Vida útil
ggplot(dados_final, aes(y = Sono_Total_p, x = Tempo_Gestacao_p)) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Tempo de gestação em dias") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Tempo de Vida útil
ggplot(dados_final, aes(y = Sono_Total_p, x = exp(-Tempo_Vida_Util_p))) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Tempo de vida útil máxima em anos") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


# Gr?fico de dispers?o para Tempo de Vida útil
ggplot(dados_final, aes(y = Sono_Total_p, x = exp(-Tempo_Gestacao_p))) + #plota o plano cartesiano
  geom_point(color = "red", fill = "red") + #plota os pontos no plano cartesiano (fill = preenchimento)
  xlab("Tempo de gestação em dias") + #atribui um r?tulo para o eixo x
  ylab("Tempo de sono total em horas/dia") #atribui um r?tulo para o eixo y
#geom_smooth(method = 'lm', se = TRUE) #plota a reta de regress?o


################################################################################
################################################################################
# DETECÇÃO DE OUTLIERS EM X

X = cbind(Tempo_Vida_Util_p, Tempo_Gestacao_p, as.factor(dados_final$Indice_Perigo))
X = as.matrix(X)
H = X%%solve(t(X)%%X)%*%t(X)
