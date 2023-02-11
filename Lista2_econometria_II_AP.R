##LISTA 2 DE ECONOMETRIA II - AMANDA PENA##
##SCRIPT REFERENTE À QUESTÃO 1 DA LISTA DE EXERCÍCIOS##
##SÉRIES DESIGNADAS - 2 E 7##

##Instalando os pacote necessários##
#install.packages("fGarch")
#install.packages("robustbase")
#install.packages("stats")
#install.packages("MTS")
#install.packages("tseries")
#install.packages("lmtest")
#install.packages("forecast")
#install.packages("moments")
#install.packages("aTSA")
#install.packages("urca")
#install.packages("PerformanceAnalytics")
#install.packages("anomalize")
#install.packages("stargazer")


##Requerendo pacotes a serem utilizados##

require(fGarch)
require(robustbase)
require(stats)
require(MTS)
require(mvtnorm)
require(Matrix)
require(vars)
require(mgarchBEKK)
require(urca)
require(PerformanceAnalytics)
require(tidyverse) 
require(anomalize)
require(stargazer)

##Limpando o ambiente##
rm()
gc()

##Definindo o diretório de trabalho##
getwd()
setwd()

##Importando o conjunto de dados##


##Inspecionando o conjunto de dados##
head(conjunto1) # 6 primeiras observa??es da base
class(conjunto1) # classe do objeto
dim(conjunto1) # dimens?o da base
summary(conjunto1) # estat?sticas descritivas das vari?veis
describe(conjunto1) # descri??o das vari?veis

################################################################################

## Série 2 ##


##Verificando qual o tipo da série designada##
dados1 <- ts(conjunto1, start = c(2:201), frequency = 1)
plot(dados1, main="Série 2", xlab="")
plot.ts(dados1,plot.type = c("multiple"),main="",xlab="")
conjunto1 [,2]-> dados2
plot.ts(dados2,plot.type = c("multiple"), main="Série 2")

##FAC E FAC-P##
par(mfrow=c(2,1))
stats::acf(dados2,lag.max=50, main="FAC Série 2") ##Senóide, amortecida, parece um ARMA
stats::pacf(dados2,lag.max=50, main="FAC-P Série 2")

##Fazendo teste de Raiz Unitária
ur_dados2_1<- ur.df(y=dados2,lags=5,type="none",selectlags="BIC")
summary(ur_dados2_1) 

ur_dados2_2<- ur.df(y=dados2,lags=5,type="trend",selectlags="BIC")
summary(ur_dados2_2)

ur_dados2_3<- ur.df(y=dados2,lags=5,type="drift",selectlags="BIC")
summary(ur_dados2_3)


##Fazendo o ajuste do modelo
arima(dados2, order=c(1,0,1))
modelo_final <- arima(dados2, order=c(1,0,1))

arima(dados2, order=c(2,0,1))
modelo_final2 <- arima(dados2, order=c(2,0,1))
stargazer(modelo_final, modelo_final2,title="Arima", align=T, results='asis', type = "html", out="modelofinal.html")


##Fazendo análise dos resíduos
par(mfrow=c(2,2))
resíduos<-residuals(modelo_final2)
#plot(residuals(modelo_final))
qqnorm(residuals(modelo_final2))
qqline(residuals(modelo_final2))
hist(residuals(modelo_final2))

kurtosis(residuals(modelo_final2))
jarque.bera.test(residuals(modelo_final)) #não rejeita H0, então os resíduos seguem distribuição normal

## FAC E FACP dos resíduos

stats::acf(residuals(modelo_final2),lag.max=50, main="FAC dos Resíduos - Série 2") 
stats::pacf(residuals(modelo_final2),lag.max=50, main="FAC-P dos Resíduos - Série 2")
par(mfrow=c(1,1))
stats::acf(resíduos^2,lag.max=50, main="Variância dos Resíduos - Série 2")

#Teste de Ljung Box - checando autocorrelação dos resíduos
Box.test(resíduos, lag = 20, type = "Ljung") ##Não rejeita a hipótese nula e os resíduos são independentemente distribuídos

################################################################################

## Série 7 ##
par(mfrow=c(2,2))
conjunto1<- matrix(data1, nrow = 2)
conjunto1 [,7]-> dados3
plot.ts(dados3,plot.type = c("multiple"), main="Série 7")


##FAC E FAC-P##
par(mfrow=c(2,1))
stats::acf(dados3,lag.max=50, main="FAC - Série 7") ##Decai lentamente
stats::pacf(dados3,lag.max=50, main="FAC-P - Série 7")


###Fazendo teste de Raiz Unitária
ur_dados3.1<- ur.df(y=dados3,lags=5,type="none",selectlags="BIC")
summary(ur_dados3.1)

ur_dados3.2<- ur.df(y=dados3,lags=5,type="trend",selectlags="BIC")
summary(ur_dados3.2)

ur_dados3.3<- ur.df(y=dados3,lags=5,type="drift",selectlags="BIC")
summary(ur_dados3.3)

##Diferenciando a série para estacionareizar##
dif1<- diff(dados3)
plot.ts(dif1, main="Série 7 - com uma diferenciação")

##FAC e FAC-P da série em primeira diferença##
stats::acf(dif1,lag.max=50, main="FAC - Série 7 com Diferenciação") ##Decai lentamente
stats::pacf(dif1,lag.max=50, main="FAC-P - Série 7 com Diferenciação")


##Realizando novamente o teste de Raiz Unitária
ur_dif1 <- ur.df(y=dif2,lags=5,type="none",selectlags="BIC")
summary(ur_dif1)

ur_dif1 <- ur.df(y=dif1,lags=5,type="drift",selectlags="BIC")
summary(ur_dif1)

ur_dif1 <- ur.df(y=dif1,lags=5,type="trend",selectlags="BIC")
summary(ur_dif1) 

##Fazendo o ajuste##
modelo.ar <- arima(x=dados3, order = c(1,1,1))
modelo.ar
modelo.ar2 <- arima(x=dados3, order = c(2,1,1)) ## Esse parece ser melhor pelo AIC
show(modelo.ar2)
stargazer(modelo.ar, modelo.ar2,title="Arima", align=T, results='asis', type = "html", out="modelofinal2.html")

###Fazendo a análise dos resíduos###
resíduo <- modelo.ar2$residuals/sd(modelo.ar2$residuals)
par(mfrow=c(2,1))
#plot(residuals(modelo.ar2))
qqnorm(residuals(modelo.ar2))
qqline(residuals(modelo.ar2))
hist(residuals(modelo.ar2))

kurtosis(resíduo)
jarque.bera.test(resíduo) #Rejeita H0, então os resíduos não seguem distribuição normal

## FAC E FACP dos resíduos

stats::acf(residuals(modelo.ar2),lag.max=50, main="FAC dos Resíduos - Série 7") 
stats::pacf(residuals(modelo.ar2),lag.max=50, main="FAC-P dos Resíduos - Série 7")
par(mfrow=c(1,1))
stats::acf(resíduo^2,lag.max=50, main="Variância dos Resíduos - Série 7")

#Ljung Box - autocorrelação dos resíduos
Box.test(resíduo, lag = 12, type = "Ljung") #p-valor menor que 0,05, rejeita H0

kurtosis(resíduo) 
# 2 - teste formal - H0: homocedasticidade; p-valor muito pequeno, então rejeita H0 e não é homocedastico
aTSA::arch.test(modelo.ar2)

# Conclusão: A série é heterocedastica.

#### Estimando a parte GARCH
## garchFit -

fit_model <- garchFit( formula =  ~ garch(1,1), data = resíduo,
                 cond.dist = "norm", #norm
                 include.mean = FALSE,
                 trace = FALSE) 

fit_model2 <- garchFit( formula =  ~ garch(2,1), data = resíduo,
                       cond.dist = "norm", #norm
                       include.mean = FALSE,
                       trace = FALSE) 

fit_model3 <- garchFit( formula =  ~ garch(1,2), data = resíduo,
                        cond.dist = "norm", #norm
                        include.mean = FALSE,
                        trace = FALSE)

fit_model4 <- garchFit( formula =  ~ garch(2,2), data = resíduo,
                        cond.dist = "norm", #norm
                        include.mean = FALSE,
                        trace = FALSE)

stargazer(fit_model, fit_model2,fit_model3, fit_model4, title="GARCH", align=T, results='asis', type = "html", out="modelogarch.html")


## Análise do modelo estimado
stats::acf(fit_model2@residuals^2,lag.max=50, main="Variância do GARCH - Série 7") ##NÃO QUIS PLOTAR A ACF, INVESTIGAR O PQ

## Previsão
predict(modelo.final,9 ,plot=TRUE,conf=0.5)



















