install.packages('GetHFData')
install.packages("stringr")

library(stringr)
library(GetHFData)
library(quantmod)
library(tseries)
library(BatchGetSymbols)


ativos = GetIbovStocks()
acoes = ativos$tickers[-c(37,40,54,58)]
ativos$tickers = paste(ativos$tickers,".SA", sep = "")
nomes = ativos$tickers[-c(37,40,54,58)]

length(nomes)

getSymbols(nomes[1:30], src="yahoo", from=Sys.Date()-200, to=Sys.Date())
getSymbols(nomes[31:61], src="yahoo", from=Sys.Date()-200, to=Sys.Date())

X = data.frame(nomes)

banco = data.frame(ABEV3.SA$ABEV3.SA.Close,
                   B3SA3.SA$B3SA3.SA.Close,
                   BBAS3.SA$BBAS3.SA.Close,
                   BBDC3.SA$BBDC3.SA.Close,
                   BBDC4.SA$BBDC4.SA.Close,
                   BBSE3.SA$BBSE3.SA.Close,
                   BRAP4.SA$BRAP4.SA.Close,
                   BRDT3.SA$BRDT3.SA.Close,
                   BRFS3.SA$BRFS3.SA.Close,
                   BRKM5.SA$BRKM5.SA.Close,
                   BRML3.SA$BRML3.SA.Close,
                   BTOW3.SA$BTOW3.SA.Close,
                   CCRO3.SA$CCRO3.SA.Close,
                   CIEL3.SA$CIEL3.SA.Close,
                   CMIG4.SA$CMIG4.SA.Close,
                   CSAN3.SA$CSAN3.SA.Close,
                   CSNA3.SA$CSNA3.SA.Close,
                   CVCB3.SA$CVCB3.SA.Close,
                   CYRE3.SA$CYRE3.SA.Close,
                   ECOR3.SA$ECOR3.SA.Close,
                   EGIE3.SA$EGIE3.SA.Close,
                   ELET3.SA$ELET3.SA.Close,
                   ELET6.SA$ELET6.SA.Close,
                   EMBR3.SA$EMBR3.SA.Close,
                   ENBR3.SA$ENBR3.SA.Close,
                   EQTL3.SA$EQTL3.SA.Close,
                   ESTC3.SA$ESTC3.SA.Close,
                   FLRY3.SA$FLRY3.SA.Close,
                   GGBR4.SA$GGBR4.SA.Close,
                   GOAU4.SA$GOAU4.SA.Close,
                   GOLL4.SA$GOLL4.SA.Close,
                   HYPE3.SA$HYPE3.SA.Close,
                   IGTA3.SA$IGTA3.SA.Close,
                   ITSA4.SA$ITSA4.SA.Close,
                   ITUB4.SA$ITUB4.SA.Close,
                   JBSS3.SA$JBSS3.SA.Close,
                   KROT3.SA$KROT3.SA.Close,
                   LAME4.SA$LAME4.SA.Close,
                   LREN3.SA$LREN3.SA.Close,
                   MGLU3.SA$MGLU3.SA.Close,
                   MRFG3.SA$MRFG3.SA.Close,
                   MRVE3.SA$MRVE3.SA.Close,
                   MULT3.SA$MULT3.SA.Close,
                   NATU3.SA$NATU3.SA.Close,
                   PCAR4.SA$PCAR4.SA.Close,
                   PETR3.SA$PETR3.SA.Close,
                   PETR4.SA$PETR4.SA.Close,
                   QUAL3.SA$QUAL3.SA.Close,
                   RADL3.SA$RADL3.SA.Close,
                   RAIL3.SA$RAIL3.SA.Close,
                   RENT3.SA$RENT3.SA.Close,
                   SBSP3.SA$SBSP3.SA.Close,
                   SMLS3.SA$SMLS3.SA.Close,
                   SUZB3.SA$SUZB3.SA.Close,
                   TIMP3.SA$TIMP3.SA.Close,
                   UGPA3.SA$UGPA3.SA.Close,
                   USIM5.SA$USIM5.SA.Close,
                   VALE3.SA$VALE3.SA.Close,
                   VIVT4.SA$VIVT4.SA.Close,
                   VVAR3.SA$VVAR3.SA.Close,
                   WEGE3.SA$WEGE3.SA.Close
                   )

names(banco) = acoes

estacionario = function(ativo){
  a = tseries::adf.test(ativo)
  
  if(a$p.value > 0.05){
    saida = "A série não é estacionária"
  } else {
    saida = "A série é estacionária"
  }
  
  return(saida)
}

estacionario(banco$BBDC3)
