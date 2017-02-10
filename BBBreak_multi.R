# This is a simple Bolinger Band trend following strategy. The system calculates a central
# moving average band from the daily close. The number of days is a variable. The entry point is
# defined as the close price crossing a  number of standard deviations away from the central
# moving average. THe position is closed when the close price crosses back below another number
# of standard deviations from the central moving average. Negative number means the other way.
# This code assumes that you have a directory with csv files of OHLC daily data for testing

library(quantstrat)       # Required package for strategy back testing
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

strat        <- "BB1"       # Give the stratgey a name variable
portfolio.st <- "BB1"       # Portfolio name
account.st   <- "BB1"       # Account name
csvDir       <- "C:/Users/RJK/Documents/SpiderOak Hive/Financial/commodities_data" # Directory containing csv files
maPeriod     <- 200          # moving average period
bbBreakout   <- 2           # multiple of SD for breakout 
bbClose      <- 0           # multiple of SD for close

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

currency('USD')             # set USD as a base currency

# Universe selection
symbol <- c("LSU","RR","CO","NG","OJ")

# if run previously
rm.strat(portfolio.st)
delete.paramset(strat,"STOP_OPT")

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

# initialize the portfolio, account and orders. Starting equity $10K and assuming data post 1998.

initPortf(portfolio.st, symbols = symbol, initDate = "2000-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = 1000000, initDate = "2000-01-01")
initOrders(portfolio = portfolio.st, initDate = "2000-01-01")

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

out <- applyStrategy(strategy=strat , portfolios=portfolio.st) # Attempt the strategy
updatePortf(Portfolio = portfolio.st)                          # Update the portfolio
updateAcct(name = account.st)
updateEndEq(account.st)

# Plot the charts fo each symbol
for (sym in symbol){
  chart.Posn(Portfolio = portfolio.st, Symbol = sym, TA="add_BBands(n=20,sd=2)")          # Chart the position
}
stats <- tradeStats(portfolio.st)

eq1 <- getAccount(account.st)$summary$End
rt1 <- Return.calculate(eq1,"log")

getSymbols("^GSPC", from = '2000-01-01')
chartSeries(GSPC, type="candlesticks", theme = chartTheme("white"))
rt2 <- periodReturn(GSPC, period = "daily")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("BB","SP500")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
                 main="BBand to Benchmark Comparison",ylab="cum return",xlab="",
                 minor.ticks=FALSE)
Sys.setenv(TZ=ttz)                                             # Return to original time zone
