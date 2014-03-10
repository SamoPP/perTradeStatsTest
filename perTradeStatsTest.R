# I spotted "poor" performance of perTradeStats compard to lighning fast addTxns.
# Below is an example that shows this.
# I test a "stupid" strategy that goes long on close of each day/bar and then
# the next day it closes the previous long position from previous day/bar and 
# immediately goes long again on that same close of day/bar.
# 
###############################################################################

options(width=250)

TZ <- "UTC"

Sys.setenv(TZ=TZ)

require(blotter)

symbol <- "SPY"

getSymbols(symbol, from="1990-01-01", auto.assign=TRUE)
SPY <- adjustOHLC(SPY)
SPY <- na.omit(SPY)

maxPosValue <- 100000 # Maximum number of $ bet on each trade
txnFees <- 0 # txnFees need to be positive since I take negative value when constructing transaction

entryTransactions <- Cl(SPY)
colnames(entryTransactions) <- "TxnPrice"
entryTransactions$TxnQty <- floor(maxPosValue/entryTransactions$TxnPrice)
entryTransactions$TxnFees <- (-1)*txnFees
# Remove last one in order not to have open trades
entryTransactions <- head(entryTransactions, -1)
exitTransactions <- Cl(SPY)
colnames(exitTransactions) <- "TxnPrice"
exitTransactions$TxnQty <- (-1)*lag.xts(entryTransactions$TxnQty)
exitTransactions$TxnFees <- (-1)*txnFees
# Remove first one since there is no entry yet (from previous close)
exitTransactions <- exitTransactions[-1, ]

transactions <- rbind(exitTransactions, entryTransactions)

init.eq <- maxPosValue

portf.test.pts <- "portf.test.pts"
account.test.pts <- "acct.test.pts"

init.date <- as.Date(first(index(get(symbol))))

try(rm(list=ls(pos=.blotter), pos=.blotter), silent = TRUE)

currency("USD")
stock(symbol, currency="USD", multiplier=1)

initPortf(portf.test.pts, symbols=symbol, initDate=init.date)

initAcct(account.test.pts, 
		 portfolios=portf.test.pts, 
		 initDate=init.date, 
		 initEq=init.eq)

addTxns(Portfolio=portf.test.pts, Symbol=symbol, TxnData=transactions, verbose=FALSE)
 
updatePortf(portf.test.pts)
updateAcct(account.test.pts)
updateEndEq(account.test.pts)

# Very slow compared to everything else
per.trade.stats.result <- perTradeStats(portf.test.pts, symbol)#, tradeDef="flat.to.reduced")
 
