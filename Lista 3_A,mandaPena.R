##LISTA 3 DE ECONOMETRIA II##
##AMANDA PENA##

##DEFININDO DIRETÓRIO DE TRABALHO##


##QUESTÃO 1##

##Carregando pacotes##
library(ggplot2)
library(forecast)
library(mFilter)
library(BETS)
library(vars)
library(scales)
library(gridExtra)
library(stargazer)
library(urca)

##Verificando a base de dados##
par(mfrow=c(3,1))
plot.ts(inf, main="Inflação")
stats::acf(inf,lag.max=50 , main="ACF Inflação")
stats::pacf(inf,lag.max=50 , main="PACF Inflação")
plot.ts(juros, main= "Juros")
acf(juros,lag.max=50, main= "ACF Juros")
stats::pacf(juros,lag.max=50 , main="PACF Juros")
plot.ts(y, main= "Produto")
stats::acf(y,lag.max=50, main="ACF do Produto") ##parece que tem sazonalidade, como identificar sazonalidade?
stats::pacf(y,lag.max=50 , main="PACF Produto")

##LETRA A##

##Verificando Estacionareidade das séries## dúvida*

##Transformações das séries que precisam estacionareizar##
par(mfrow=c(3,1))
dinf = diff(inf)
plot.ts(dinf, main="Inflação com Primeira Diferença")
stats::acf(dinf,lag.max=50 , main="ACF Inflação com Primeira Diferença")
stats::pacf(dinf,lag.max=50 , main="PACF Inflação com Primeira Diferença")
djuros = diff(juros)
plot.ts(djuros, main="Juros com Primeira Diferença")
stats::acf(djuros,lag.max=50 , main="ACF Juros com Primeira Diferença")
stats::pacf(djuros,lag.max=50 , main="PACF Juros com Primeira Diferença")

##Juntar séries
data <- window(ts.intersect(y, dinf, djuros),
               start=c(2000,01))
colnames(data) <- c('produto', 'inflacao', 'juros')


##Estimando o VAR##
lag <- VARselect(data, lag.max=12, type='trend')
var <- VAR(data, min(lag$selection), type='both')
summary(var)
print(var) ##estimou um VAR(4)
plot(var)
res<-residuals(var)
plot.ts(res)

##LETRA B##



################################################################################

##QUESTÃO 2##

options(OutDec=".") # trocando ponto por v?rgula

dados=read.table("clipboard",head=T) ##até 12/2012
class(dados)
dados
plot.ts(log(dados))
x <- ts(dados/1000, start = c(1974,1), frequency = 12)
plot.ts (x)
