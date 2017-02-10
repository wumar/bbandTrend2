# This is a simple Bolinger Band trend following strategy. The system calculates a central
# moving average band from the daily close. The number of days is a variable. The entry point is
# defined as the close price crossing a  number of standard deviations away from the central
# moving average. THe position is closed when the close price crosses back below another number
# of standard deviations from the central moving average. Negative number means the other way.
# Here we incorporate optimization via paramsets for the bband breakout and the moving average
# I have been unable to get paramsets working for the close due to the need to i) set multiple 
# variables with one function called from the indicator and ii) optimize a parameter in the 
# sigCrossover function

library(quantstrat)       # Required package for strategy back testing
library(doParallel)       # For parrallel optimization
library(rgl)              # Library to load 3D trade graphs
library(reshape2)         # Library to load 3D trade graphs
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

csvDir       <- "C:/Users/RJK/Documents/SpiderOak Hive/Financial/commodities_data" # Directory containing csv files
strat        <- "BB1"       # Give the stratgey a name variable
portfolio.st <- "BB1"       # Portfolio name
account.st   <- "BB1"       # Account name
maPeriod     <- seq(20, 150, by = 10)       # moving average period
bbBreakout   <- seq(1, 3, by = 0.25)           # multiple of SD for breakout 
bbClose      <- 1                          # multiple of SD for close

# This function sets the standard devation parameter to pass to the 
# Bolinger Band indicator function

closeSD_final <-function(user_SD){
  if(user_SD == 0){
    returnSD <- 1
  } else{
    returnSD <- abs(user_SD)
  }
  return(returnSD)
}

# The following two functions set, based on the bbClose variable, which band the close must
# cross in order for the strategy to exit. If number os +ve it is on the same side of the 
# moving average as the initial move, 0 is the MA and -ve is on the other side of the MA.

longExitBand <- function(user_SD){
  if(user_SD == 0){
    longBand <- "mavg.BBands_close"
  } else if(user_SD > 0){
    longBand <- "up.BBands_close" 
  } else {
    longBand <- "dn.BBands_close"
  }
  return(longBand)
}

shortExitBand <- function(user_SD){
  if(user_SD == 0){
    shortBand <- "mavg.BBands_close"
  } else if(user_SD > 0){
    shortBand <- "dn.BBands_close" 
  } else {
    shortBand <- "up.BBands_close"
  }
}

currency('USD')                         # set USD as a base currency
symbol <- c("LSU","RR","CO","NG","OJ")  # Universe selection

# if run previously, run this code
delete.paramset(portfolio.st,"BB_OPT")
rm.strat(portfolio.st)


# set the instument as a future and get the data from the csv file
for (sym in symbol){
  
  future(sym, currency = "USD", multiplier = 1)
}

getSymbols(Symbols = symbol, verbose = TRUE, warnings = TRUE, 
           src = 'csv', dir= csvDir, extension='csv', header = TRUE, 
           stingsAsFactors = FALSE)

for (sym in symbol){
  no_dup <- to.daily(get(sym), indexAt='days',drop.time = TRUE) # this is required to remove duplicate data
  assign(sym, no_dup)
}

# initialize the portfolio, account and orders. Starting equity $10K and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol, initDate = "1995-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = 10000, initDate = "1995-01-01")
initOrders(portfolio = portfolio.st, initDate = "1995-01-01")

# define the strategy with a position limit to prevent multiple trades in a direction
strategy(strat, store = TRUE)
for (sym in symbol){
  addPosLimit(strat, sym, timestamp="2000-01-01", maxpos=100, 
              longlevels = 1, minpos=-100, shortlevels = 1)
}

# Add the indicators - One bband for the breakout another for the stop
add.indicator(strat, name = "BBands", 
              arguments = list(HLC = quote(Cl(mktdata)), 
                               n = maPeriod, maType = 'SMA',sd = bbBreakout
              ),
              label = "BBands_breakout"
)

add.indicator(strat, name = "BBands", 
              arguments = list(HLC = quote(Cl(mktdata)), 
                               n = maPeriod, maType = 'SMA',sd = closeSD_final(bbClose)
              ),
              label = "BBands_close"
)

# Add the signals -  Go long on a cross of the close greater than the breakout band and close on a cross 
# less than the close band. Signals reversed for a short.
add.signal(strat, name = "sigCrossover", 
           arguments = list(columns=c(quote(Cl(mktdata)),"up.BBands_breakout"),
                            relationship = "gt"
           ), 
           label = "long_entry"
)

add.signal(strat, name = "sigCrossover", 
           arguments = list(columns=c(quote(Cl(mktdata)),longExitBand(bbClose)), 
                            relationship = "lt"
           ), 
           label = "long_exit"
)

add.signal(strat, name = "sigCrossover", 
           arguments = list(columns=c(quote(Cl(mktdata)),"dn.BBands_breakout"), 
                            relationship = "lt"
           ), 
           label = "short_entry"
)

add.signal(strat, name = "sigCrossover", 
           arguments = list(columns=c(quote(Cl(mktdata)),shortExitBand(bbClose)), 
                            relationship = "gt"
           ), 
           label = "short_exit"
)

# Add the rules - what trades to make on the signals giving using osMaxPos to limit positions.
add.rule(strat, name = 'ruleSignal', 
         arguments = list(sigcol = "long_entry", 
                          sigval = TRUE, orderqty = 100, ordertype = 'market', 
                          orderside ='long', osFUN='osMaxPos', orderset = 'ocolong'
         ), 
         type ='enter', label = "LE"
)

add.rule(strat, name = 'ruleSignal', 
         arguments = list(sigcol = "long_exit", 
                          sigval = TRUE, orderqty = 'all', ordertype = 'market', 
                          orderside ='long', orderset = "ocolong"
         ), 
         type ='exit', label = "LX"
)

add.rule(strat, name = 'ruleSignal', 
         arguments = list(sigcol = "short_entry", 
                          sigval = TRUE, orderqty = -100, ordertype ='market', 
                          orderside = 'short', osFUN='osMaxPos', orderset = 'ocoshort'
         ), 
         type ='enter', label = 'SE'
)

add.rule(strat, name = 'ruleSignal', 
         arguments = list(sigcol = "short_exit", 
                          sigval = TRUE, orderqty = 'all', ordertype ='market', 
                          orderside ='short', orderset = 'ocoshort'
         ), 
         type ='exit', label = "SX"
)

#add paramset distributions
add.distribution(portfolio.st,
                 paramset.label = "BB_OPT",
                 component.type = "indicator",
                 component.label = "BBands_breakout",
                 variable = list( sd = bbBreakout ),
                 label = "bb_break"
)

add.distribution(portfolio.st,
                 paramset.label = "BB_OPT",
                 component.type = "indicator",
                 component.label = "BBands_breakout",
                 variable = list( n = maPeriod ),
                 label = "ma_b"
)

add.distribution(portfolio.st,
                 paramset.label = "BB_OPT",
                 component.type = "indicator",
                 component.label = "BBands_close",
                 variable = list( n = maPeriod ),
                 label = "ma_c"
)

add.distribution.constraint(portfolio.st,
                            paramset.label = "BB_OPT",
                            distribution.label.1 = "ma_b",
                            distribution.label.2 = "ma_c",
                            operator = "==",
                            label = "mabeqmac")

registerDoParallel(cores=detectCores())

out <- apply.paramset(strat, paramset.label = "BB_OPT",
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE)

stats <- out$tradeStats
wd <- getwd()
csv_file <- paste(wd,"closesd",bbClose,".csv", sep="")
out <- write.csv(stats,             # write to file
                 file = csv_file,
                 quote = FALSE, row.names = TRUE)

# A loop to investigate the parameters via a 3D graph
for (sym in symbol){
  dfName <- paste(sym,"stats", sep = "")
  statSubsetDf <- subset(stats, Symbol == sym)
  assign(dfName, statSubsetDf)
  tradeGraphs(stats = statSubsetDf, 
              free.params=c("bb_break","ma_b"),
              statistics = c("Ann.Sharpe","Profit.To.Max.Draw","Min.Equity"), 
              title = sym)
}

# Or use a heatmap to look at one parameter at a time
for (sym in symbol){
  dfName <- paste(sym,"stats", sep = "")
  statSubsetDf <- subset(stats, Symbol == sym)
  assign(dfName, statSubsetDf)
  z <- tapply(X=statSubsetDf$Ann.Sharpe, 
              INDEX = list(statSubsetDf$bb_break,statSubsetDf$ma_b), 
              FUN = median)
  x <- as.numeric(rownames(z))
  y <- as.numeric(colnames(z))
  filled.contour(x=x,y=y,z=z,color=heat.colors,xlab="bbreak",ylab="MA")
  title(sym)
}


Sys.setenv(TZ=ttz)                                             # Return to original time zone
