#__________________________________________________________________________________________________________________________________

# This monster function that takes some rows of sharesDF (e.g. a particular fold of data), uses pre-trained model to predict prob of stockmarket going up next day, makes a decision about whether to buy vs sell based on strategy params, then returns a df that tracks financial outcome of model-predicted vs baseline strategy for each day's trade----
#__________________________________________________________________________________________________________________________________

tradingOutcomeFun <- function(inputDF, startingMoney, propMoneySpentOnShares, buyThreshold, sellThreshold, 
                              numSharesToBuyOrSell, transactionCost){ # haven't added in lag yet - do I need to?
  
  # # Testing function - *** HASH OUT, AND RE-READ IN ACTUAL PARAM VALUES AT TOP OF CODE ***:
  # inputDF <- sharesDF
  # startingMoney <- 2000 # money used to buy shares in first place, with some held back to buy more (see param below)
  # propMoneySpentOnShares <- 0.75 # need to hold some money back to buy more shares if model says so.
  # buyThreshold <- 0.6
  # sellThreshold <- 0.4 # what does model-predicted prob of stock going up have to be before we decide to buy/sell?
  # numSharesToBuyOrSell <- 1 # if above threshold, buy x shares; if below then sell x. NOTE WOULD BE BETTER TO MAKE NUMBER A FUNCTION OF PROB THAT MARKET WILL GO UP OR DOWN; IE IF VERY CONFIDENT THEN SELL MORE
  # transactionCost <- 0 # how many dollars for each day's buying or selling of shares? [should this be a % of transaction value instead?]
  # cat('WARNING - Hash out test parameters in tradingOutcomeFunction!\n')
  
  # Predict whether market will go up or down
  inputDF$probUp <- predict(trainedModelOutputs$trainedModel, newdata=inputDF, type='response')

  # Decide whether or not to buy or sell
  inputDF$decision <- 'noAction'
  inputDF$decision[inputDF$probUp > buyThreshold] <- 'buy'
  inputDF$decision[inputDF$probUp < sellThreshold] <- 'sell'
  table(inputDF$decision)
  
  #++++++++++++++
  # Calculate money spent or earnt buying / selling shares, and number of shares resulting from each day's trade. Tricky part is working in rules whereby if there isn't enough $ or shares for the trade, dont go ahead with it. That's what ifelses below are doing.
  #++++++++++++++
  
  # Calculate initial no. of shares bought & money left over, based on starting share price
  startingShareNum <- floor((startingMoney*propMoneySpentOnShares)/inputDF$closePrice[1])
  startingShareValue <- round(startingShareNum*inputDF$closePrice[1], 2)
  startingMoneyValue <- round(startingMoney-startingShareValue, 2)
  
  # cat(paste0('Buying shares for test data. Shares bought initially: ', startingShareNum, ' // Starting value of shares: $', startingShareValue, ' // Money left over: $', startingMoneyValue, '\n'))
  
  ### SET UP FIRST ROW MANUALLY, THEN DO REST WITH A LOOP THAT REFERENCES FIRST ROW
  
  inputDF$numSharesBeforeTrading <- c(startingShareNum, rep(NA, nrow(inputDF)-1))
  inputDF$moneyBeforeTrading <- c(startingMoneyValue, rep(NA, nrow(inputDF)-1))
  inputDF$moneyInOrOut <- NA
  inputDF$numSharesAfterTrading <- NA
  inputDF$valueOfMoneyBeforeNextTrade <- NA
  inputDF$valueOfSharesBeforeNextTrade <- NA
  inputDF$profit <- NA
  
  row1 <- inputDF[1, ]
  
  warningsVec <- vector() # record info about how many times ran out of $ or stocks
  
  # If elses to apply different buying rules depending on whether there's enough money to buy or enough stocks to sell:
  if(row1$decision=='buy'){ # if buying, test whether enough money to buy stocks; if not dont buy and issue warning.
    
    if(row1$closePrice>row1$moneyBeforeTrading){
      warningsVec[length(warningsVec)+1] <- 'insufficient funds to make trade at row 1'
      row1$moneyInOrOut <- 0
      row1$numSharesAfterTrading <- row1$numSharesBeforeTrading
    } else {
      row1$moneyInOrOut <- -1*(row1$closePrice*numSharesToBuyOrSell)-transactionCost*numSharesToBuyOrSell
      row1$numSharesAfterTrading <- row1$numSharesBeforeTrading+numSharesToBuyOrSell
    }
  } else if(row1$decision=='sell'){ # if selling, test enough stocks availalbe; if not dont sell and issue warning.
    
    if(row1$numSharesBeforeTrading<numSharesToBuyOrSell){
      warningsVec[length(warningsVec)+1] <- 'insufficient shares to make trade at row 1'
      row1$moneyInOrOut <- 0
      row1$numSharesAfterTrading <- row1$numSharesBeforeTrading
    } else {
      row1$moneyInOrOut <- row1$closePrice*numSharesToBuyOrSell-transactionCost*numSharesToBuyOrSell
      row1$numSharesAfterTrading <- row1$numSharesBeforeTrading-numSharesToBuyOrSell
    }
  } else { # and last option is if decision is 'noAction'.
    
    row1$moneyInOrOut <- 0
    row1$numSharesAfterTrading <- row1$numSharesBeforeTrading
  }
  
  # Add in values that depend on ifelse results
  row1$valueOfMoneyBeforeNextTrade <- with(row1, moneyBeforeTrading + moneyInOrOut)
  row1$valueOfSharesBeforeNextTrade <- with(row1, closePriceAfterLag*numSharesAfterTrading)
  row1$profit <- with(row1, valueOfMoneyBeforeNextTrade + valueOfSharesBeforeNextTrade - startingMoney)
  
  inputDF[1, ] <- row1 # replacing inputDF row 1 with updated version from above
  
  ### NOW USE LOOP TO FILL IN REST OF ROWS
  
  for(i in 1:(nrow(inputDF)-1)){
    
    row_iPlus1 <- inputDF[i+1,]
    
    row_iPlus1$numSharesBeforeTrading <- inputDF$numSharesAfterTrading[i]
    row_iPlus1$moneyBeforeTrading <- inputDF$valueOfMoneyBeforeNextTrade[i]
    
    # If elses to apply different buying rules depending on whether there's enough money to buy or enough stocks to sell:
    if(row_iPlus1$decision=='buy'){ # if buying, test whether enough money to buy stocks; if not dont buy and issue warning.
      
      if(row_iPlus1$closePrice>row_iPlus1$moneyBeforeTrading){
        warningsVec[length(warningsVec)+1] <- paste0('insufficient funds to make trade at row ', i+1, '; cant buy until after next time decision=="sell" & funds get bumped up')
        row_iPlus1$moneyInOrOut <- 0
        row_iPlus1$numSharesAfterTrading <- row_iPlus1$numSharesBeforeTrading
      } else {
        row_iPlus1$moneyInOrOut <- -1*(row_iPlus1$closePrice*numSharesToBuyOrSell)-transactionCost*numSharesToBuyOrSell
        row_iPlus1$numSharesAfterTrading <- row_iPlus1$numSharesBeforeTrading+numSharesToBuyOrSell
      }
    } else if(row_iPlus1$decision=='sell'){ # if selling, test enough stocks availalbe; if not dont sell and issue warning.
      
      if(row_iPlus1$numSharesBeforeTrading<numSharesToBuyOrSell){
        warningsVec[length(warningsVec)+1] <- paste0('insufficient shares to make trade at row ', i+1, '; cant sell until after next time decision=="buy"')
        row_iPlus1$moneyInOrOut <- 0
        row_iPlus1$numSharesAfterTrading <- row_iPlus1$numSharesBeforeTrading
      } else {
        row_iPlus1$moneyInOrOut <- row_iPlus1$closePrice*numSharesToBuyOrSell-transactionCost*numSharesToBuyOrSell
        row_iPlus1$numSharesAfterTrading <- row_iPlus1$numSharesBeforeTrading-numSharesToBuyOrSell
      }
    } else { # and last option is if decision is 'noAction'.
      
      row_iPlus1$moneyInOrOut <- 0
      row_iPlus1$numSharesAfterTrading <- row_iPlus1$numSharesBeforeTrading
    }
    
    # Add in values that depend on ifelse results
    row_iPlus1$valueOfMoneyBeforeNextTrade <- with(row_iPlus1, moneyBeforeTrading + moneyInOrOut)
    row_iPlus1$valueOfSharesBeforeNextTrade <- with(row_iPlus1, closePriceAfterLag*numSharesAfterTrading)
    row_iPlus1$profit <- with(row_iPlus1, valueOfMoneyBeforeNextTrade + valueOfSharesBeforeNextTrade - startingMoney)
    
    inputDF[i+1,] <- row_iPlus1
  }
  
  inputDF$totalAssetsBeforeNextTrade <- with(inputDF, valueOfMoneyBeforeNextTrade + valueOfSharesBeforeNextTrade)
  tempProfit <- round(inputDF$profit, 2); inputDF <- inputDF %>% select(-profit) %>% mutate(profit=tempProfit) # reordering
  
  # # Check it worked
  # inputDF[1:4,]
  # inputDF[5:9,]
  # inputDF[183:186,]
  # inputDF[280:284,] # include row numbers where warning message above said 'not enough $/shares to complete trade', to check that's working
  
  #++++++++++++++
  # Calculate profit of baseline strategy - hold onto shares; no buying or selling
  #++++++++++++++
  
  inputDF$numSharesAfterTrading_baseline <- startingShareNum
  inputDF$valueOfSharesBeforeNextTrade_baseline <- with(inputDF, numSharesAfterTrading_baseline*closePriceAfterLag)
  inputDF$valueOfMoneyBeforeNextTrade_baseline <- startingMoneyValue
  inputDF$totalAssetsBeforeNextTrade_baseline <- with(inputDF, valueOfMoneyBeforeNextTrade_baseline+valueOfSharesBeforeNextTrade_baseline)
  inputDF$profit_baseline <- round(inputDF$totalAssetsBeforeNextTrade_baseline-startingMoney, 2)
  
  # head(inputDF, 2)
  cat("   ", length(warningsVec[grepl('insufficient funds', warningsVec)]), "trades ran out of money and", length(warningsVec[grepl('insufficient shares', warningsVec)]), "trades ran out of stock during simulation\n")
  return(inputDF)
}

# #++++++++++++++
# # Test function
# #++++++++++++++
# 
# outputdf <- tradingOutcomeFun(inputDF=sharesDF, startingMoney=10000, propMoneySpentOnShares=0.75, buyThreshold=0.51, sellThreshold=0.50,
#                               numSharesToBuyOrSell=50, transactionCost=0)
# 
# #++++++++++++++
# # Plot profit from this strategy vs just holding on to stock (also plotting numShares at same time)
# #++++++++++++++
# 
# profitsDF <- bind_rows(outputdf %>%
#                       select(date, profit) %>%
#                       mutate(strategy='Model-based buy & sell'),
#                     outputdf %>%
#                       select(date=date, profit=profit_baseline) %>%
#                       mutate(strategy='Baseline (Hold shares & money)'))
# numSharesDF <- bind_rows(outputdf %>%
#                        select(date, numSharesBeforeTrading) %>%
#                        mutate(strategy='Model-based buy & sell'),
#                      outputdf %>%
#                        select(date=date) %>%
#                        mutate(numSharesBeforeTrading=outputdf$numSharesBeforeTrading[1], strategy='Baseline (Hold shares & money)'))
# 
# profitsPlot <- ggplot(profitsDF, aes(date, profit, colour=strategy)) + geom_line() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
#   scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 week")
# numSharesPlot <- ggplot(numSharesDF, aes(date, numSharesBeforeTrading, colour=strategy)) + geom_line() +
#   ylim(0, max(numSharesDF$numSharesBeforeTrading*1.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
#   scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 week")
# 
# windows()
# grid.newpage()
# grid.draw(rbind(ggplotGrob(numSharesPlot), ggplotGrob(profitsPlot), size = "last")) # plot both together
# graphics.off()

# investigate any anomalies seen in plot
# filter(outputdf, date>'2016-07-07')
