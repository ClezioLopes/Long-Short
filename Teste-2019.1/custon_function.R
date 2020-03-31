#-------#  Operação de Long & Short

#Bibliotecas
library(quantmod)
library(data.table)
library(ggplot2)
library(tseries)
library(lubridate)
library(BatchGetSymbols)

######################################################
##      COMO FUNCIONA A TEORIA DE COINTEGRAÇÃO      ##
######################################################

#fazendo download dos ativos selecionados
  getSymbols("PETR3.SA", src="yahoo", from=Sys.Date()-150, to=Sys.Date())
  getSymbols("ITUB4.SA", src="yahoo", from=Sys.Date()-150, to=Sys.Date())
  
  PETR3 = as.data.frame(PETR3.SA)
  ITUB4 = as.data.frame(ITUB4.SA)
  
#Separando para trabalhar somente com os valores de fechamentos
  PETR3F = PETR3$PETR3.SA.Close
  ITUB4F = ITUB4$ITUB4.SA.Close

#Verficando se as séries são não-estacionárias
  tseries::adf.test(PETR3F)
  tseries::adf.test(ITUB4F)
    
#Ajustando uma regressão com os fechamentos
  regressao = lm(PETR3F ~ ITUB4F)
  summary(regressao)

#Verificando se os resíduos são estacionários
  residuos = residuals(regressao)
  tseries::adf.test(residuos)

##################################################################
##   CRIANDO UMA FUNÇÃO ROBUSTA PARA VERIFICAR A COINTEGRAÇÃO   ##
##################################################################
  
#Vamos criar uma função que verifica a cointegração entre os ativos, tornando assim o nosso trabalho muito mais fácil
cointegra = function(ativo1, ativo2) {
  saida = "Vazio"
  
  #Baixando os ativos
  acao1 = BatchGetSymbols("PETR3.SA", first.date=Sys.Date() - 150, last.date=Sys.Date())
  acao2 = BatchGetSymbols("PETR4.SA", first.date=Sys.Date() - 150, last.date=Sys.Date())
  
  situacao1 = acao1$df.control$download.status
  situacao2 = acao1$df.control$download.status
  
  #Verificando se os ativos foram baixados corretamente
  if ((situacao1 == "OK") & (situacao2 == "OK")){
    
    #Trabalhando somente com valores referentes aos ativos
    acao1c = data.frame(acao1$df.tickers) 
    acao2c = data.frame(acao2$df.tickers)
    
    #Removendo possíveis NA's
    acao1limpo = acao1c[!is.na(acao1c$price.close),]
    acao2limpo = acao2c[!is.na(acao2c$price.close),]
    
    names(acao1limpo) = c("Open","High","Low","Close","Volume","Ajus",
                          "Date","Nome","Rajus","Rclose")
    names(acao2limpo) = c("Open","High","Low","Close","Volume","Ajus",
                          "Date","Nome","Rajus","Rclose")
    
    banco1 = xts(acao1limpo,order.by = acao1limpo$Date)
    banco2 = xts(acao2limpo,order.by = acao2limpo$Date)
    
    acao1t = banco1[index(banco1) %in% index(banco2)]  
    acao2t = banco2[index(banco2) %in% index(banco1)] 
    
    #trabalhando apenas com os valores de fechamento
    acao1f = banco1$Close
    acao2f = banco2$Close
    
    #Aqui está o coração da cointegração
    # 1- Verificar se as séries são não-estacionárias
    # 2- Ajustar uma regressão entre as duas séries
    # 3- Verificar se os resíduos da regressão são estacionários
    test1 = tseries::adf.test(acao1f)
    test2 = tseries::adf.test(acao2f)
    
    if ((test1$p.value > 0.05) & (test2$p.value > 0.05)) {
      

      # regressao = lm(acao1f ~ acao2f)
      # residuos = regressao$residuals
      # test_res = tseries::adf.test(residuos)

      if (test_res$p.value < 0.05) {
      
        saida = "As séries são Cointegradas"
      } else {
        
        saida = "As séries não são Cointegradas"
      }
    } else {
      saida = "As séries são estacionárias"
    }
    
  } else {
    saida = "Erro ao fazer download dos ativos!"
  }
  
  return(saida)
}
  
#################################################
cointegra("PETR4.SA","ITUB4.SA")  

#################################################
  