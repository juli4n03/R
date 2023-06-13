base = read.csv("C:/Users/JulianoCesardaSilva/Desktop/Backtest/CDCM_Semear_SPC_BVS_Serasa_QUOD.csv", sep = ";")


# Defina a base de dados
df = base_empilhada_filtrada
names(df)
# Defina o tamanho da amostra total
sample_size <- 16382

# Defina as proporções de cada região
prop_CentroOeste <- 0.1133
prop_Nordeste <- 0.4682
prop_Norte <- 0.2605
prop_Sudeste <- 0.1455
prop_Sul <- 0.0125

# Calcule o tamanho da amostra para cada região
size_CentroOeste <- round(sample_size * prop_CentroOeste)
size_Nordeste <- round(sample_size * prop_Nordeste)
size_Norte <- round(sample_size * prop_Norte)
size_Sudeste <- round(sample_size * prop_Sudeste)
size_Sul <- round(sample_size * prop_Sul)

# Coleta amostra de cada região
sample_CentroOeste <- df[df$Região == 'Centro-Oeste', ] %>% sample_n(size = size_CentroOeste)
sample_Nordeste <- df[df$Região == 'Nordeste', ] %>% sample_n(size = size_Nordeste)
sample_Norte <- df[df$Região == 'Norte', ] %>% sample_n(size = size_Norte)
sample_Sudeste <- df[df$Região == 'Sudeste', ] %>% sample_n(size = size_Sudeste)
sample_Sul <- df[df$Região == 'Sul', ] %>% sample_n(size = size_Sul)

# Combina todas as amostras em uma única base de dados
amostra_aleatoria <- rbind(sample_CentroOeste, sample_Nordeste, sample_Norte, sample_Sudeste, sample_Sul)
amostra_aleatoria$Proposta
