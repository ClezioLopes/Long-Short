library(ggplot2)
install.packages("scales")
library(scales)
install.packages("BatchGetSymbols")
library(BatchGetSymbols)
library(urca)
install.packages("data.table")
library(data.table)


verficaCoin = function(papel1,papel2){
  #Faz download dos papéis informados diretamente do Yahoo!
  acao1 = BatchGetSymbols(papel1, first.date=Sys.Date() - 500, last.date=Sys.Date())
  acao2 = BatchGetSymbols(papel2, first.date=Sys.Date() - 500, last.date=Sys.Date())
  
  #Utilizando apenas os valores de fechamento diário
  acao1_f = acao1$df.tickers$price.close
  acao2_f = acao2$df.tickers$price.close
  
  #Removendo os NA's
  f_acao1 = acao1_f[!is.na(acao1_f)]
  f_acao2 = acao2_f[!is.na(acao2_f)]
  
  #Juntando os valores de fechamento
  dados_AC1xAC2 = cbind(f_acao1,f_acao2)
  
  #Realizando o teste de Phillips-Ouliaris para medir Cointegração
  cointest = ca.po(dados_AC1xAC2)
  
  #Valor do TESTE
  teste = cointest@teststat
  
  #Valor de cada nível de significância
  nivel = cointest@cval

  x=NA
  if(teste <= nivel){
    x="Os papéis não são cointegrados"
  } else {
    x="Os papéis são cointegrados"
  }
  return(x)
}

verficaCoin("PETR4.SA","PETR3.SA")



  

