if(!require(data.table)){install.packages("data.table")}
if(!require(dplyr)){install.packages("dplyr")}
require(ggplot2)

################################################
##################### Dados ####################
################################################

options(scipen = 999)
set.seed(1)
dados <- fread("http://www.cnachtsheim-text.csom.umn.edu/Kutner/Chapter%20%208%20Data%20Sets/CH08PR06.txt",
               col.names = c("y", "x"))

################################################
################## Descritiva ##################
################################################

plot(dados$x, dados$y, main = 'Relação entre Nível de esteroide e Idade', xlab = "Idade", ylab = "Nível de esteroide")

x12 <- dados$x*dados$x
cor(dados$x,x12)
pairs(dados_x, main = "Matriz de gráficos de dispersão")

dados_x <- data.frame(dados$x,x12)
cor(dados_x)

################################################
################# Padronização #################
################################################
x_c <- dados$x - mean(dados$x)
x12_c <- x_c*x_c

dados_c = data.frame(dados$y, x_c, x12_c); dados_c
pairs(dados_c, main = "Matriz de gráficos de dispersão com dados padronizados")
cor(dados_c)


################################################
## Modelo 1 - Termos quadráticos s/ interação ##
################################################

modelo1 <- lm(dados.y ~ poly(x_c, 2, raw = F), data=dados_c)
summary(modelo1)

#Função de regressão
ggplot(dados, aes(x, y) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, col = "red") +
  labs(
    #title = "Regressão Linear com Banda de Confiança",
    x = "Idade",
    y = "Nível de esteroide"
  )


################################################
######## Estimativa Intervalar Conjunta ########
################################################
x0 <- data.frame(x_c = c(10, 15, 20))
x0_c = x0 - mean(dados$x)#padronizando
if(!require(data.table)){install.packages("data.table")}
if(!require(dplyr)){install.packages("dplyr")}
require(ggplot2)

################################################
##################### Dados ####################
################################################

options(scipen = 999)
set.seed(1)
dados <- fread("http://www.cnachtsheim-text.csom.umn.edu/Kutner/Chapter%20%208%20Data%20Sets/CH08PR06.txt",
               col.names = c("y", "x"))

################################################
################## Descritiva ##################
################################################

plot(dados$x, dados$y, main = 'Relação entre Nível de esteroide e Idade', xlab = "Idade", ylab = "Nível de esteroide")

x12 <- dados$x*dados$x
cor(dados$x,x12)
pairs(dados_x, main = "Matriz de gráficos de dispersão")

dados_x <- data.frame(dados$x,x12)
cor(dados_x)

################################################
################# Padronização #################
################################################
x_c <- dados$x - mean(dados$x)
x12_c <- x_c*x_c

dados_c = data.frame(dados$y, x_c, x12_c); dados_c
pairs(dados_c, main = "Matriz de gráficos de dispersão com dados padronizados")
cor(dados_c)


################################################
## Modelo 1 - Termos quadráticos s/ interação ##
################################################

modelo1 <- lm(dados.y ~ poly(x_c, 2, raw = F), data=dados_c)
summary(modelo1)

#Função de regressão
ggplot(dados, aes(x, y) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, col = "red") +
  labs(
    #title = "Regressão Linear com Banda de Confiança",
    x = "Idade",
    y = "Nível de esteroide"
  )


################################################
######## Estimativa Intervalar Conjunta ########
################################################
x0 <- data.frame(x_c = c(10, 15, 20))
x0_c = x0 - mean(dados$x)#padronizando

#Banda de confiança de Working-Hoteling
ci.wh <- function(model, newdata, alpha = 0.01)
{
  df <- nrow(model.frame(model)) - length(coef(model))
  W <- sqrt(2 * qf(1 - alpha, 2, df) ) 
  ci <- predict(model, newdata, se.fit = TRUE)
  x <- cbind(
    'x' = newdata,
    's' = ci$se.fit,
    'fit' = ci$fit,
    'lwr' = ci$fit - W * ci$se.fit,
    'upr' = ci$fit + W * ci$se.fit)
  return(x)
}
ci.wh(modelo1, x0_c)



################################################
############ Intervalo de Predição #############
################################################
x0 <- data.frame(x_c = c(15))
predict(modelo1, newdata = x0, interval = 'prediction', level = 0.99)


################################################
############ Análise de Diagnóstico ############
################################################
residuos <- residuals(modelo1)
plot(predict(modelo1), residuos, xlab = 'Preditos', ylab = 'Resíduos')
abline(h=0, col='red', lty=2)

## Verificando a normalidade
qqnorm(residuos)
qqline(residuos, col='red', lty=2)

## Testes de Normalidade
lillie.test(residuos)
shapiro.test(residuos)

## Teste de Homocedasticidade
bptest(modelo1, studentize = TRUE)
gqtest(modelo1)


predict(modelo1, newdata = x0_c, interval = 'confidence', level = 0.99)


################################################
############ Intervalo de Predição #############
################################################
x0 <- data.frame(x_c = c(15))
predict(modelo1, newdata = x0, interval = 'prediction', level = 0.99)


################################################
############ Análise de Diagnóstico ############
################################################
residuos <- residuals(modelo1)
plot(predict(modelo1), residuos, xlab = 'Preditos', ylab = 'Resíduos')
abline(h=0, col='red', lty=2)

## Verificando a normalidade
qqnorm(residuos)
qqline(residuos, col='red', lty=2)

## Testes de Normalidade
lillie.test(residuos)
shapiro.test(residuos)

## Teste de Homocedasticidade
bptest(modelo1, studentize = TRUE)
gqtest(modelo1)


