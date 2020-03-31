library(quantmod)

getSymbols("^BVSP", src="yahoo", from=Sys.Date()-160, to=Sys.Date())
BVSP <- data.frame(BVSP)

chartSeries(BVSP, up.col="DodgerBlue2", dn.col="IndianRed2",
            theme=chartTheme('white'), TA=NULL, show.grid = F)

###########################################################################
###########################################################################

getSymbols("PETR4.SA", src="yahoo", from=Sys.Date()-160, to=Sys.Date())
PETR4.SA <- data.frame(PETR4.SA)

chartSeries(PETR4.SA, up.col="DodgerBlue2", dn.col="IndianRed2",
            theme=chartTheme('white'), TA=NULL, show.grid = F)


###########################################################################
###########################################################################
vetor = c("PETR3.SA","BBDC3.SA","BBDC4.SA")
getSymbols(vetor, src="yahoo", from=Sys.Date()-160, to=Sys.Date())

BBDC3.SA <- data.frame(BBDC3.SA)
BBDC4.SA <- data.frame(BBDC4.SA)
PETR3.SA <- data.frame(PETR3.SA)
PETR4.SA <- data.frame(PETR4.SA)

chartSeries(BBDC3.SA, up.col="DodgerBlue2", dn.col="Gold1",
            theme=chartTheme('white'), TA=NULL, show.grid = F)
