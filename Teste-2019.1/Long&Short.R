install.packages("dplyr", dependencies = T)
install.packages("plyr", dependencies = T)
install.packages("quantmod", dependencies = T)
install.packages("data.table", dependencies = T)
install.packages("ggplot2", dependencies = T)
install.packages("tseries", dependencies = T)
install.packages("lubridate", dependencies = T)
install.packages("BatchGetSymbols", dependencies = T)

#Bibliotecas
library(quantmod)
library(data.table)
library(ggplot2)
library(tseries)
library(lubridate)
library(BatchGetSymbols)
library(dplyr)
library(plyr)


#Ativos que compõe o IBOVESPA
ativos = GetIbovStocks()$tickers
nomes = paste(ativos, "SA", sep = ".")


#Baixando os ativos do ibovespa - Tratando os dados
banco = BatchGetSymbols(tickers =  nomes,
                        bench.ticker = "^BVSP", 
                        first.date = as.Date("2018-04-09"),
                        last.date = as.Date("2019-04-09"))


#Criando um banco somente com informações dos ativos
novo_banco = banco$df.tickers

lista = dlply(novo_banco, .(ticker), 
              function(x) {rownames(x) = x$row;x$row = NULL;x})

#Criando um banco com todos os fechamentos e Removendo NA´s
fechamentos = as.data.table(matrix(NA,
                                   nrow = length(lista[[1]]$price.close),
                                   ncol = length(lista)
                                   ))

names(fechamentos) = names(lista)

for (i in 1:length(lista)) {
  fechamentos[,i] = lista[[i]][["price.close"]]
}

#IBOV = round(fechamentos,2) #Banco de dados com todos os ativos da bovespa
IBOV = fechamentos

#### Algumas funções interessantes 

#-- Estacionaridade
estacionario = function(serie){
  resul = adf.test(serie)
  
  if (resul$p.value <= 0.05){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#-- I(1)
int_1 = function(serie){
  s2 = diff(serie, d = 1)
  resul = adf.test(s2)
  
  if (resul$p.value <= 0.05){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#Fazendo os testes das funções criadas acima
teste = data.frame(matrix(NA,
                          nrow = 2,
                          ncol = length(lista)))
names(teste) = names(IBOV)
row.names(teste) = c("Est","I(1)")

teste[1,] = apply(IBOV, 2, estacionario)

teste[2,] = apply(IBOV, 2, int_1)

######################################
#Removendo as linhas que:
#         - Não são estacionárias de ordem 1: (False, True nos testes)
for (i in 1:length(teste)) {
    if ((teste[1,i] == FALSE) != (teste[2,i] == TRUE)) {
      IBOV[,i] = NULL
    }
}
######################################
#-- Utilizando laço para verificar a Cointegração entre ativos
new_lista = list()
list_res = list()
t = 1
for (i in 1:length(IBOV)) {
    for (j in 1:length(IBOV)) {
        if( i != j) {
          
            regres = lm(IBOV[[i]] ~ IBOV[[j]])
            resumo = summary(regres)
            
            residuos = resumo$residuals
            test_res = adf.test(residuos)
  
            if(test_res$p.value <= 0.05) {
              
              #dataframe com as informações em relação aos pares cointegrados
                new_lista[[t]] = data.frame(ativo1 = names(IBOV)[[i]],
                                            ativo2 = names(IBOV)[[j]],
                                            coef_a = resumo[["coefficients"]][[2]],
                                            coef_l = resumo[["coefficients"]][[1]],
                                            mean_r = mean(resumo[["residuals"]]),
                                            meia_v = NA,
                                            ban_inf = (-2)*sd(resumo[["residuals"]]),
                                            ban_sup = 2*sd(resumo[["residuals"]]))
                
              #lista com os residuos dos pares cointegrados
                list_res[[t]] = resumo[["residuals"]]
                
                t = (t + 1)
            }
        }
    }
}

resultado = NULL
for (i in 1:length(new_lista)) {
  resultado = rbind(resultado,new_lista[[i]])
}
row.names(resultado) = index(resultado)
View(resultado)

#######################################################################################

ggplot(data=IBOV) +
  geom_line(aes(index(IBOV), IBOV$BTOW3.SA), colour ="red") +
  geom_line(aes(index(IBOV), IBOV$CYRE3.SA), colour ="blue") +
  labs(title="Dados das Séries no ano de 2018") 
