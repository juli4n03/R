########################################
## --- AULAS 12 e 13 - EXEMPLOS 01 E 02 --- ##
########################################

#Carregando a base de dados no R
attach(CH01PR19)
X <- X2 #covariável X (nota obtida no vestibular)
Y <- X1 #variável resposta Y (nota média ao final do primeiro ano)

#Ajuste do modelo de regressão linear simples
ajuste <- lm(Y~X)
summary(ajuste)

#Intervalo de confiança para os parâmetros do modelo de regressão linear simples
confint(ajuste, level = 0.99)

#Intervalo de confiança para o valor esperada da variável resposta para um valor específico da
covariável
x0 <- data.frame(X = c(28))
predict(ajuste, newdata = x0, interval = 'confidence', level = 0.95)

#Intervalo de predição para uma nova observação da variável resposta quando a covariável
assume um valor específicox0 <- data.frame(X = c(28))
predict(ajuste, newdata = x0, interval = 'prediction', level = 0.95)

#Banda de confiança de Working-Hoteling
ci.wh <- function(model, newdata, alpha = 0.1)
{
  df <- nrow(model.frame(model)) - length(coef(model)) # 23
  W <- sqrt(2 * qf(1 - alpha, 2, df) ) # 2.2580
  ci <- predict(model, newdata, se.fit = TRUE)
  x <- cbind(
    'x' = newdata,
    's' = ci$se.fit,
    'fit' = ci$fit,
    'lwr' = ci$fit - W * ci$se.fit,
    'upr' = ci$fit + W * ci$se.fit)
  return(x)
}
new <- data.frame(X = c(28))
ci.wh(ajuste, new)

#Soma dos quadrados total
SQTotal <- sum((Y - mean(Y))^2)
SQTotal

#Soma dos quadrados dos resíduos
SQRes <- sum((Y - ajuste$fitted.values)^2)
SQRes

#Soma dos quadrados da regressão
SQReg <- SQTotal - SQRes

SQReg

#Tabela ANOVA
anova(ajuste)
summary(ajuste)

#Análise de resíduos
plot(ajuste)
shapiro.test(ajuste$residuals)
bptest(ajuste)