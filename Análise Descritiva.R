#PACOTES 
#-------------
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
#-------------

#IMPORTAÇÃO DE BASES
#-------------
SPC =    read.csv("D:\\OneDrive - StepWise\\Projetos\\Backtest\\Ponto de corte\\Estudo 05062023\\CDCM\\Bases\\SPC.csv", sep = ";")
SERASA = read.csv("D:\\OneDrive - StepWise\\Projetos\\Backtest\\Ponto de corte\\Estudo 05062023\\CDCM\\Bases\\SERASA.csv", sep = ";")
BVS =    read.csv("D:\\OneDrive - StepWise\\Projetos\\Backtest\\Ponto de corte\\Estudo 05062023\\CDCM\\Bases\\BVS.csv", sep = ";")
CDCM_CRIVO = read.csv("D:\\OneDrive - StepWise\\Projetos\\Backtest\\Ponto de corte\\Estudo 05062023\\CDCM\\Bases\\CDCM - ClienteNovo - crivo_credrisk_inad_classABC_012023a042023.csv"
, sep = ";")
#-------------

#FILTROS
#-------------
names(CDCM_CRIVO)
unique(CDCM_CRIVO_SPC$ClassABC.SPC.)
unique(CDCM_CRIVO_Serasa$ScoreFinal)

length(CDCM_CRIVO_SPC$ScoreFinal[CDCM_CRIVO_SPC$ScoreFinal == 0])

CDCM_CRIVO_SPC <- subset(CDCM_CRIVO_SPC, ClassABC.SPC. != "")
CDCM_CRIVO_Serasa <- subset(CDCM_CRIVO_Serasa, ClassABC.Serasa. != "")

CDCM_CRIVO_SPC <- CDCM_CRIVO %>% filter(ModeloFinal == "SPC")
CDCM_CRIVO_Serasa <- CDCM_CRIVO %>% filter(ModeloFinal == "Serasa")
#-------------


#GRÁFICOS
#------------------------------

  #BOXPLOT
  #-------------
p1 <- ggplot(SERASA, aes(x = SERASA$classABCDE, y = SERASA$SERASA_SCORE_HVAR)) + 
  geom_boxplot() +
  ggtitle("Score Serasa HVAR(Base backtest)") +
  theme_minimal()

p2 <- ggplot(SPC, aes(x = SPC$classABCDE, y = SPC$SPC_Score_customizado)) + 
  geom_boxplot() +
  ggtitle("Score SPC Customizado(Base backtest)") +
  theme_minimal()


p3 <- ggplot(CDCM_CRIVO_SPC, aes(x = CDCM_CRIVO_SPC$ClassABC.SPC., y = CDCM_CRIVO_SPC$ScoreFinal)) + 
  geom_boxplot() +
  ggtitle("Score SPC Customizado(Base atual)") +
  theme_minimal()

p4 <- ggplot(CDCM_CRIVO_Serasa, aes(x = CDCM_CRIVO_Serasa$ClassABC.Serasa., y = CDCM_CRIVO_Serasa$ScoreFinal)) + 
  geom_boxplot() +
  ggtitle("Score Serasa HVAR(Base atual)") +
  theme_minimal()

grid.arrange(p2, p3, ncol = 1)

  #-------------
  
  #HISTOGRAMA
  #-------------

x = ggplot(SERASA, aes(x=SERASA$SERASA_SCORE_HVAR)) +
  geom_histogram(binwidth=30, fill='blue', color='black') +
  ggtitle("Score HVAR(Base backtest)") + theme_minimal()

y = ggplot(CDCM_CRIVO_Serasa, aes(x=CDCM_CRIVO_Serasa$ScoreFinal)) +
  geom_histogram(binwidth=30, fill='blue', color='black') +
  ggtitle("Score HVAR(Base atual)") + theme_minimal()


z = ggplot(SPC, aes(x=SPC$SPC_Score_customizado)) +
  geom_histogram(binwidth=30, fill='blue', color='black') +
  ggtitle("Score SPC Customizado(Base backtest)") + theme_minimal()


w = ggplot(CDCM_CRIVO_SPC, aes(x=CDCM_CRIVO_SPC$ScoreFinal)) +
  geom_histogram(binwidth=30, fill='blue', color='black') +
  ggtitle("Score SPC Customizado(Base atual)") + theme_minimal()


grid.arrange(z,w, ncol = 1)
  #-------------
  
  #PIZZA
  #---------------
SPCnew_counts <- as.data.frame(table(CDCM_CRIVO_SPC$ClassABC.SPC.))
colnames(SPCnew_counts) <- c("classe", "count")
SPCnew_counts$prop <- SPCnew_counts$count / sum(SPCnew_counts$count)
SPCnew_counts$percentage <- SPCnew_counts$prop * 100


SPC_counts <- as.data.frame(table(SPC$classABCDE))
SPC_counts$prop <- SPC_counts$count / sum(SPC_counts$count)
SPC_counts$percentage <- SPC_counts$prop * 100
colnames(SPC_counts) <- c("classe", "count")


ggplot(SPC_counts, aes(x="", y=prop, fill=classe)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette = "Blues", guide = "legend", aesthetics = "fill", name = "Classe") +
  theme_void() +
  ggtitle("Proporção de Perfil de Risco Score SPC Customizado (Base Backtest)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "black") +
  labs(fill="Classe")
  #---------------


  #BARRA EMPILHADA
  #------------
# Contando a frequência de cada região
data_freq <- table(CDCM_CRIVO$Região[CDCM_CRIVO$Região != "Sem classificação"])
data_freq <- as.data.frame(data_freq)
names(data_freq) <- c("Região", "Frequência")

# Convertendo frequência para proporção
data_freq$Proporção <- data_freq$Frequência / sum(data_freq$Frequência)

# Adicionando uma coluna de grupo para permitir uma única barra no gráfico
data_freq$Grupo = "Brasil"

# Plotando o gráfico de barras empilhadas
ggplot(data = data_freq, aes(x = Grupo, y = Proporção, fill = Região)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("Representatividade") +
  xlab("Região") +
  ggtitle("Representatividade por amostra") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Região"))
  #------------

#------------------------------

#ANALISE DESCRITVA
#-------------
summary(SERASA$SERASA_SCORE_HVAR)
length(SPC$SEMEAR_Proposta)

plot(SPC$SPC_Score_customizado,SPC$classABCDE)


hist(SPC$BVS_SCRCRDPNM06MPFLGBCLFALLV5, main="Histograma Score BVS", xlab="Score", col="darkblue", border="black")

boxplot(SEMEAR$BVS_SCRCRDPNM06MPFLGBCLFALLV5)


ggplot(BVS, aes(x = "", y = BVS$BVS_SCRCRDPNM06MPFLGBCLFALLV5)) +
  geom_boxplot(fill = "lightblue", colour = "black", notch = TRUE, outlier.shape = 16, outlier.colour = "red") +
  labs(title = "Boxplot sobre Score BVS", y = "Score") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank()) + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "blue") +
  stat_summary(fun.data = function(y) data.frame(y = quantile(y, probs = 0.5), label = "Median"), 
               geom = "text", vjust = -1, size = 3, color = "darkblue") +
  theme_minimal()

length(SEMEAR$BVS_SCRCRDPNM06MPFLGBCLFALLV5[SEMEAR$BVS_SCRCRDPNM06MPFLGBCLFALLV5<100])

par(mfrow=c(3,1))
hist(SERASA$SERASA_SCORE_HVAR, main="Score SERASA", xlab="Score", col="darkblue", border="black")
hist(SPC$SPC_Score_customizado, main=" Score SPC", xlab="Score", col="darkblue", border="black")
hist(BVS$BVS_SCRCRDPNM06MPFLGBCLFALLV5, main=" Score BVS", xlab="Score", col="darkblue", border="black")
par(mfrow=c(1,1))


SERASA_score = SERASA$SERASA_SCORE_HVAR
SPC_score = SPC$SPC_Score_customizado
BVS_score = BVS$BVS_SCRCRDPNM06MPFLGBCLFALLV5

df <- data.frame(
  Score = c(SERASA_score, SPC_score, BVS_score),
  Origem = c(rep("SERASA", length(SERASA_score)), 
             rep("SPC", length(SPC_score)), 
             rep("BVS", length(BVS_score)))
)

# Plota o boxplot
ggplot(df, aes(x = Origem, y = Score)) + 
  geom_boxplot() +
  ggtitle("Boxplot Comparativo") +
  xlab("Origem") +
  ylab("Score")

#-------------


#EXPORTATION DE BASES
#------------
write.csv(CDCM_CRIVO_SPC, file = "D:\\OneDrive - StepWise\\Projetos\\Backtest\\Ponto de corte\\Estudo 05062023\\CDCM\\Bases\\CDCM_CRIVO_SPC.csv")
write.csv(CDCM_CRIVO_Serasa, file = "D:\\OneDrive - StepWise\\Projetos\\Backtest\\Ponto de corte\\Estudo 05062023\\CDCM\\Bases\\CDCM_CRIVO_Serasa.csv")

#------------


