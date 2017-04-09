
# NEED TO DO BETTER NESTED RESAMPLING TO ESTIMATE FIRST STRATEGY PARAMS, AND SECOND MODEL PERFORMANCE, WITHOUT OVERFITTING.
# - currently use createTimeSlices to train model, then once model is fitted try different params on a single year's worth of data and then measure performance using those params on the same set of data. 
# - But this is poo! Should be doing timeslice version of nested resampling: (1) fit model on inner slice, (2) find best strategy params on middle slice, (3) test strategy params on outer slice (or something). Aim would be to repeat each step on multiple slices, so that the strategy param that performed best across multiple slices could be chosen, and so that the performance *and variance in performance* using the best strategy params could be measured on independent data.

# SHOULD I BE USING ADJUSTED PRICE FUNCTION Ad() OR adjustOHLC, AS PER HERE https://www.rdocumentation.org/packages/quantmod/versions/0.4-7/topics/adjustOHLC  SEE ALSO THIS ANSWER ABOUT POSSIBLE DATA ERRORS AND FIXES: http://quant.stackexchange.com/questions/25521/yahoo-data-meaning-of-close-and-adjusted-close & http://quant.stackexchange.com/questions/7216/daily-returns-using-adjusted-close


#++++++++++++++
# Temp playing with defining a function that makes numShares bought a function of confidence in pUp
#++++++++++++++

myBuyThreshold <- 0.6
sharesAvailableToBuy <- 70
myProbUp <- seq(myBuyThreshold, 1, 0.1)

# Function that buys 1 share when prob of market going up is at buyThreshold, and that buys all available shares (defined by available money I guess) when prob of market going up is 1:
mySharesToBuy <- 1 + ((sharesAvailableToBuy-1)/(1-myBuyThreshold))*(myProbUp-myBuyThreshold) # figured this out by plotting linear relationship I wanted - i.e. y=1 when probUp=buyThreshold, and y=availableShares when probUp=1. Then working out intercept ('1 + '), slope ('(sharesAvailableToBuy-1)/(1-myBuyThreshold)'), and scaling factor ('-myBuyThreshold') required to make this a linear equation.
# NB if I want to have a parameter than strenghtens rate at which shares bought increases with increasing p, then modify slope like this:
cbind(myProbUp, mySharesToBuy)

rateParam <- 1
slope <- rateParam*(sharesAvailableToBuy-1)/(1-myBuyThreshold) # where rateParam of 1 just keeps eqn as is. >1 sell at faster rate, <1 sell at slower rate. Will also need an if statement that says that if result of linear function > sharesAvailableToBuy, buy sharesAvailableToBuy, otherwise buy no. specified by function.

#++++++++++++++
# and as above, but for defining shares to sell
#++++++++++++++

sharesAvailableToSell <- 10
mySellThreshold <- 0.4
myProbUp <- seq(0, mySellThreshold, 0.1)

mySharesToSell <- sharesAvailableToSell + (-1*((sharesAvailableToSell-1)/mySellThreshold))*myProbUp
cbind(myProbUp, mySharesToSell)

# ==> as above, eqn sells 1 share if pUp=mySellThreshold, increasing linearly to sell all available shares if pUp=0. Just plotted what I wanted here and worked out linear eqn required to get this relationship based on intercept ('sharesAvailableToSell') & slope ('(-1*((sharesAvailableToSell-1)/mySellThreshold))'). As above, can use rate param combined with if statement to sell at faster / slower rate.



#__________________________________________________________________________________________________________________________________

# Set parameters and load packages ----
#__________________________________________________________________________________________________________________________________

rm(list=ls())
initialWindow <- 730 # how many contiguous days in each splits' training set
horizon <- 365 # how many contiguous days in each splits' test set. * Also controls size of final test dataset *. NB these are the params from caret's createTimeSlices function; see notes below for details.
lag <- 1 # how many days ahead am I predicting? e.g. does numTweets today predict stock price tomorrow, or in two days time? 

startingMoney <- 2000 # money used to buy shares in first place, with some held back to buy more (see param below). In USD I assume (or whatever currency share data is in).
propMoneySpentOnShares <- 0.75 # need to hold some money back to buy more shares if model says so. 
transactionCost <- 0.03 # how many dollars does it cost to buy or sell each share? NB Vanguard says their ETFs have a bid-ask spread of 'a couple of pennies per share', and other cost ('expense ratio') is minimal - perhaps a few hundred dollars for 10K over 10yr. 

# Params to loop thru to find optimal buy/sell strategy
buyThresholdVec <- seq(0.5, 1, 0.25)
sellThresholdVec <- seq(0, 0.5, 0.25) # what does model-predicted prob of stock going up have to be before we decide to buy/sell?
numSharesToBuyOrSellVec <- c(1, 5, 15) # if above threshold, buy x shares; if below then sell x. NOTE WOULD BE BETTER TO MAKE NUMBER A FUNCTION OF PROB THAT MARKET WILL GO UP OR DOWN; IE IF VERY CONFIDENT THEN SELL MORE 

warning('Havent sorted lag yet for lag>1. Stock price is correctly lagged, but Im still trading each day rather than every [lag] days, which is what I think I should be doing. Need to investigate\n')

set.seed(0803)
suppressMessages(library('quantmod'))
suppressMessages(library('dplyr'))
suppressMessages(library('ggplot2'))
suppressMessages(library('grid'))
suppressMessages(library('caret'))

#__________________________________________________________________________________________________________________________________

# Get financial data using quantmod package, and define respose variable - price movement (up vs down) of target stock ----
#__________________________________________________________________________________________________________________________________

#++++++++++++++
# Extract data
#++++++++++++++

# getSymbols(c("ORCL","AAPL")) # Get apple and oracle data
# head(AAPL['2017-01-01::2017-02-25']) # See stock data
# oaXTS <- as.xts(merge(AAPL, ORCL)) # see data for multiple shares at once -  in case I want to look at multiple shares in future
# head(oaDF)

getSymbols('VTI') # get VTI data. This stock is an 'ETF', so tracks whole market. Forbes says it's one of the best (https://www.forbes.com/pictures/hefk45ij/10-best-etfs-for-2016/#321329d67806). 
cat("Currently trading with VTI - this stock is an ETF, so tracks whole market. It's one of the best. Things to note:\n - Cost of holding 10K of VTI stock for 10yr is $46. This is trivial in scheme of things, so ignoring\n - Yield of this stock is 2%; i.e. if trading at $50 you get a $1 dividend at the end of the year. Believe this means that if stock is increasing at 5% per year it's actual increase is more like 7% (cos you could use that dividend to buy 2% more stocks). I'm also ignoring this [what about if I'm using adjusted price tho?], but should factor in later\n - Source: https://www.forbes.com/pictures/hefk45ij/10-best-etfs-for-2016/#321329d67806\n\n")

# windows()
# chartSeries(VTI) # plot stock performance over time
# graphics.off()

warning('Apple shares had massive crash a few years ago - is model going to be able to account for these, or is it too simple? It doesnt account for magnitude of gains or losses at present. E.g. if model tells me direction of shares will be down next day, but I only sell 1 share, and then theres a crash, the model will have predicted correctly but Im still going to lose all my winnings. Do I need to sell/buy much more aggressively?\n')

dates <- index(VTI) # quantmod uses xts format, which requires index() to extract dates

sharesDF <- data.frame(date=dates,  
                       unadjustedClosePriceForPlot=Cl(VTI),
                       closePrice=Ad(VTI),
                       closePriceAfterLag=Next(Ad(VTI), k=lag)) # Cl() & NExt() are helper functions in quantmod... could just calc directly from data (call "AAPL") but this is easier. k is lag size, i.e k=2 would give price 2days thence.

closePricePlot <- ggplot(sharesDF, aes(date, VTI.Close)) + geom_line(colour='light green') + # df names defined above dont stick - fix below
  ggtitle('Unadjusted close price across full date range')
adjClosePricePlot <- ggplot(sharesDF, aes(date, VTI.Adjusted)) + geom_line(colour='light green') +
  ggtitle('Adjusted close price across full date range')

windows()
grid.newpage() 
grid.draw(rbind(ggplotGrob(closePricePlot), ggplotGrob(adjClosePricePlot), size = "last")) # plot both together
# graphics.off()

sharesDF <- select(sharesDF, -VTI.Close)

# Data cleaning - xts does funny things with colnames and row labels, so fixing
rownames(sharesDF) <- NULL
colnames(sharesDF)[grepl('Adjusted', colnames(sharesDF))] <- 'closePrice'
colnames(sharesDF)[grepl('Next', colnames(sharesDF))] <- 'closePriceAfterLag'

# Print info on data Im looking at
cat(paste0('Extracting data across full available date range: ', min(sharesDF$date), ' to ',  max(sharesDF$date), '\n stock start price: $', round(sharesDF$closePrice[1], 0), ' // starting money: $', startingMoney, ' // propn spent on shares: ', propMoneySpentOnShares, ' // approx num shares bought (based on start price in training cf testing data): ', floor((startingMoney*propMoneySpentOnShares)/sharesDF$closePrice[1]), '\n'))

warning('Looking at adjusted close price for VTI for now, but theres volume etc in the quantmod data too, which could be useful?\n')

#++++++++++++++
# Record whether shares went up or down during lag period
#++++++++++++++

sharesDF$priceChange <- with(sharesDF, ifelse(closePriceAfterLag>closePrice, 'up', 'down'))
head(sharesDF)


#__________________________________________________________________________________________________________________________________

# Get predictor variables - no. of bull & bear tweets ----
#__________________________________________________________________________________________________________________________________

#++++++++++++++
# For now, simulate. Scrape from twitter in future
#++++++++++++++

# Create tweet data, such that numBulls & numBears on a given day will predict whether price goes up or down after lag period
tweetsDF <- data.frame(date=sharesDF$date,
                       numBulls=floor(ifelse(sharesDF$priceChange=='up',
                                       rpois(n=nrow(sharesDF), lambda=500) + runif(n=nrow(sharesDF), min=0, max=7000),
                                       rpois(n=nrow(sharesDF), lambda=100) + runif(n=nrow(sharesDF), min=0, max=7000))),
                       numBears=floor(ifelse(sharesDF$priceChange=='down',
                                       rpois(n=nrow(sharesDF), lambda=500) + runif(n=nrow(sharesDF), min=0, max=7000),
                                       rpois(n=nrow(sharesDF), lambda=100) + runif(n=nrow(sharesDF), min=0, max=7000)))) # runif is hack way to add some extra variation and reduce strength of signal

# tweetsDF <- data.frame(date=as.Date(c(NA, sharesDF$date[1:length(sharesDF$date)-1])), # offsetting date by 1d
#                        numBulls=floor(ifelse(sharesDF$priceChange=='up', 
#                                        rpois(n=nrow(sharesDF), lambda=500) + runif(n=nrow(sharesDF), min=0, max=10000), 
#                                        rpois(n=nrow(sharesDF), lambda=100) + runif(n=nrow(sharesDF), min=0, max=10000))),
#                        numBears=floor(ifelse(sharesDF$priceChange=='down', 
#                                        rpois(n=nrow(sharesDF), lambda=500) + runif(n=nrow(sharesDF), min=0, max=10000), 
#                                        rpois(n=nrow(sharesDF), lambda=100) + runif(n=nrow(sharesDF), min=0, max=10000)))) # runif is adding some extra variation. Will be some negative numbers, but meh

sharesDF <- left_join(sharesDF, tweetsDF, by='date')
head(sharesDF)

# Create ratio of bears to bulls - may be better predictor than including both numBulls and numBears in model
sharesDF$bullToBearRatio <- sharesDF$numBulls/sharesDF$numBears

head(sharesDF)

# Plot to see how simulation looks
plotDF <- sharesDF
plotDF$probPriceChangeUp <- with(plotDF, ifelse(priceChange=='down', 0, 1))

windows()
ggplot(plotDF, aes(bullToBearRatio, probPriceChangeUp)) + geom_jitter(width=0, alpha=0.5) + geom_smooth()
dev.off()

#__________________________________________________________________________________________________________________________________

# Model price change as function of bullToBear ratio, then predict price up prob on holdout set ----
#__________________________________________________________________________________________________________________________________

#++++++++++++++
# Some final data manipulation before modelling
#++++++++++++++

sharesDF$priceChange <- as.factor(sharesDF$priceChange)
sharesDF <- sharesDF[complete.cases(sharesDF),] # NAs introduced by calculating closePriceAfterLag; last [lag] days will be NA

# #++++++++++++++
# # Using simple holdout set for now, switch to cross val (or backtesting? see wikipedia) later
# #++++++++++++++
# 
# # Create test and train datasets
# traindf <- sharesDF[1:floor(nrow(sharesDF)*.67),]
# testdf <- sharesDF[ceiling(nrow(sharesDF)*.67):nrow(sharesDF),]
# 
# cat(paste0(' training data date range: ', min(traindf$date), ' to ', max(traindf$date), '; \n test data date range: ', min(testdf$date), ' to ', max(testdf$date), '\n'))
# 
# # Train model
# m1 <- glm(priceChange ~ bullToBearPrevDay, data=traindf, family='binomial') # obviuosly test better algorithms in future
# 
# # Make predictions on test set
# testdf$probUp <- predict(m1, newdata=testdf, type='response')

#++++++++++++++
# Use caret's createTimeSlices cross validation method
#++++++++++++++

# See here for implementing in model building:  http://stackoverflow.com/questions/24758218/time-series-data-spliting-and-model-evaluation
# See here for what the createTimeSlices params are:  https://r-norberg.blogspot.co.nz/2016/08/data-splitting-time-slices-with.html

# Basically in this method of cross validation all training data is time-contiguous, as is all test data. The parameters are as follows:
# - horizon: how many rows of data are used in the test set NOTE THIS ASSUMES 1 ROW = 1 TIME PERIOD
# - initial window: how many rows of data are used in the training set to start with
# - fixed window: if True, size of training set in each resample stays same, if false, size of training set grows and grows.
# - Example # 1: if horizon = 5, initial window = 10, and fixed = T, then 
#   .. first training set would be rows 1:10, first test set would be rows 11:15
#   .. second training set would be rows 2:11, second test set would be rows 12:16
#   .. all through the data
# - Example #2: fixed = F:
#  .. first training set would be rows 1:10, first test set would be rows 11:15
#  .. second training set would be rows 1:11, second test set would be rows 12:16
#  .. last training set would be rows 1:xx, second test set would be rows xx:xx
# See first link above for more details

timeControl <- trainControl(# classProbs=TRUE, summaryFunction=twoClassSummary, # want accuracy instead for now
                            method = "timeslice",  # this is better way of calling createTimeSlices function
                            initialWindow = initialWindow, 
                            horizon = horizon,      
                            fixedWindow = TRUE) 

mLR <- train(priceChange ~ bullToBearRatio, data=sharesDF, method="glm", family='binomial', trControl = timeControl)
myFinalModel <- mLR

finalModelAcc <- mean(myFinalModel$resample$Accuracy)
finalModelAcc_stdev <- sd(myFinalModel$resample$Accuracy)

#++++++++++++++
# Predict from model on new data
#++++++++++++++

sharesDF$probUp <- predict(mLR$finalModel, type='response')

testdf <- sharesDF[(nrow(sharesDF)-horizon):nrow(sharesDF), ] # I *think* the final [horizon] rows never get used in model training (see graphs here https://r-norberg.blogspot.co.nz/2016/08/data-splitting-time-slices-with.html), so can use to test on. Alternatively I could hold back another test set to test on... or just do simple training/testing split (like I did above, then hashed out).

plotdf <- mutate(testdf, priceChangeNumeric=ifelse(priceChange=='up', 1, 0))
windows()
ggplot(plotdf, aes(probUp, priceChangeNumeric)) + geom_jitter(alpha=0.1)
dev.off()

#__________________________________________________________________________________________________________________________________

# Write monster function that takes test data with model-predicted probabilities, makes a decision about whether to buy vs sell based on params, then returns df that tracks financial outcome of model-predicted vs baseline strategy for each day's trade----
#__________________________________________________________________________________________________________________________________

cat("\nRecording financial outcome of applying decision: '\n - Buy/sell at end of day (close of business or 'COB'); i.e. dollars spent/earned are based on that day's closePrice. \n - After buying/selling, shares then change value over next [lag] days, and share value is calculated as numShares at COB (after buying/selling) times their value right before COB after [lag] days, i.e. right before buying or selling again. \n - Money value is also calcuated right before COB after [lag] days, but is assumed to stay at same value between COB and COB after [lag] days (could change to match real interest rate - would be sitting in loan offset account so interest is equivalent to home loan interest rate I believe), whereas share price changes.\n - If run out of $ or shares then have to wait until model says sell or buy so until have enough $ / shares to complete next buy/sell:\n")
  
tradingOutcomeFun <- function(startingMoney, propMoneySpentOnShares, buyThreshold, sellThreshold, 
                              numSharesToBuyOrSell, transactionCost){ # currently uses testdf as input, could make this a param tho. Also haven't added in lag yet - do I need to?
  
  # # Testing function - *** HASH OUT, AND RE-READ IN ACTUAL PARAM VALUES AT TOP OF CODE ***:
  # startingMoney <- 2000 # money used to buy shares in first place, with some held back to buy more (see param below)
  # propMoneySpentOnShares <- 0.75 # need to hold some money back to buy more shares if model says so.
  # buyThreshold <- 0.6
  # sellThreshold <- 0.4 # what does model-predicted prob of stock going up have to be before we decide to buy/sell?
  # numSharesToBuyOrSell <- 1 # if above threshold, buy x shares; if below then sell x. NOTE WOULD BE BETTER TO MAKE NUMBER A FUNCTION OF PROB THAT MARKET WILL GO UP OR DOWN; IE IF VERY CONFIDENT THEN SELL MORE
  # transactionCost <- 0 # how many dollars for each day's buying or selling of shares? [should this be a % of transaction value instead?]
  # cat('Hash out test parameters in function!\n')
  
  # Decide whether or not to buy or sell
  testdf$decision <- 'noAction'
  testdf$decision[testdf$probUp > buyThreshold] <- 'buy'
  testdf$decision[testdf$probUp < sellThreshold] <- 'sell'
  table(testdf$decision)
  
  #++++++++++++++
  # Calculate money spent or earnt buying / selling shares, and number of shares resulting from each day's trade. Tricky part is working in rules whereby if there isn't enough $ or shares for the trade, dont go ahead with it. That's what ifelses below are doing.
  #++++++++++++++
  
  # Calculate initial no. of shares bought & money left over, based on starting share price
  startingShareNum <- floor((startingMoney*propMoneySpentOnShares)/testdf$closePrice[1])
  startingShareValue <- round(startingShareNum*testdf$closePrice[1], 2)
  startingMoneyValue <- round(startingMoney-startingShareValue, 2)
  
  # cat(paste0('Buying shares for test data. Shares bought initially: ', startingShareNum, ' // Starting value of shares: $', startingShareValue, ' // Money left over: $', startingMoneyValue, '\n'))
  
  ### SET UP FIRST ROW MANUALLY, THEN DO REST WITH A LOOP THAT REFERENCES FIRST ROW
  
  testdf$numSharesBeforeTrading <- c(startingShareNum, rep(NA, nrow(testdf)-1))
  testdf$moneyBeforeTrading <- c(startingMoneyValue, rep(NA, nrow(testdf)-1))
  testdf$moneyInOrOut <- NA
  testdf$numSharesAfterTrading <- NA
  testdf$valueOfMoneyBeforeNextTrade <- NA
  testdf$valueOfSharesBeforeNextTrade <- NA
  testdf$profit <- NA
  
  row1 <- testdf[1, ]
  
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
  
  testdf[1, ] <- row1 # replacing testdf row 1 with updated version from above
    
  ### NOW USE LOOP TO FILL IN REST OF ROWS
  
  for(i in 1:(nrow(testdf)-1)){
    
    row_iPlus1 <- testdf[i+1,]
    
    row_iPlus1$numSharesBeforeTrading <- testdf$numSharesAfterTrading[i]
    row_iPlus1$moneyBeforeTrading <- testdf$valueOfMoneyBeforeNextTrade[i]
    
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
    
    testdf[i+1,] <- row_iPlus1
  }
  
  testdf$totalAssetsBeforeNextTrade <- with(testdf, valueOfMoneyBeforeNextTrade + valueOfSharesBeforeNextTrade)
  tempProfit <- round(testdf$profit, 2); testdf <- testdf %>% select(-profit) %>% mutate(profit=tempProfit) # reordering
  
  # # Check it worked
  # testdf[1:4,]
  # testdf[5:9,]
  # testdf[183:186,]
  # testdf[280:284,] # include row numbers where warning message above said 'not enough $/shares to complete trade', to check that's working
  
  #++++++++++++++
  # Calculate profit of baseline strategy - hold onto shares; no buying or selling
  #++++++++++++++
  
  testdf$numSharesAfterTrading_baseline <- startingShareNum
  testdf$valueOfSharesBeforeNextTrade_baseline <- with(testdf, numSharesAfterTrading_baseline*closePriceAfterLag)
  testdf$valueOfMoneyBeforeNextTrade_baseline <- startingMoneyValue
  testdf$totalAssetsBeforeNextTrade_baseline <- with(testdf, valueOfMoneyBeforeNextTrade_baseline+valueOfSharesBeforeNextTrade_baseline)
  testdf$profit_baseline <- round(testdf$totalAssetsBeforeNextTrade_baseline-startingMoney, 2)
  
  # head(testdf, 2)
  cat("   ", length(warningsVec[grepl('insufficient funds', warningsVec)]), "trades ran out of money and", length(warningsVec[grepl('insufficient shares', warningsVec)]), "trades ran out of stock during simulation\n")
  return(testdf)
}

# #++++++++++++++
# # Test function
# #++++++++++++++
#   
# outputdf <- tradingOutcomeFun(startingMoney=10000, propMoneySpentOnShares=0.75, buyThreshold=0.6, sellThreshold=0.4,
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
# # graphics.off()
# 
# # investigate any anomalies seen in plot
# # filter(outputdf, date>'2016-07-07')

#__________________________________________________________________________________________________________________________________

# Use function above to try different combinations of decision rules (prob cutoffs, amount to trade etc) & see lift of each method ----
#__________________________________________________________________________________________________________________________________

# Right now just focusing on buyThreshold, sellThreshold, and numSharesToBuyOrSell... look at others later

#++++++++++++++
# Loop through params to test performance of diff decision rules
#++++++++++++++
  
finalDifferenceInProfitDF <- data.frame(buyThreshold=NA, sellThreshold=NA, numSharesToBuyOrSell=NA, finalProfitDiff=NA)
finalAbsoluteProfitDF <- data.frame(buyThreshold=NA, sellThreshold=NA, numSharesToBuyOrSell=NA, finalProfit=NA)

for(j in 1:length(buyThresholdVec)){
  for(k in 1:length(sellThresholdVec)){
    for(l in 1:length(numSharesToBuyOrSellVec)){
      
      buyThreshold_j <- buyThresholdVec[j]
      sellThreshold_k <- sellThresholdVec[k]
      numSharesVec_l <- numSharesToBuyOrSellVec[l]
      
      outputDF <- tradingOutcomeFun(startingMoney=startingMoney, propMoneySpentOnShares=propMoneySpentOnShares, 
                                    transactionCost=transactionCost,
                                     buyThreshold=buyThreshold_j, sellThreshold=sellThreshold_k, 
                                     numSharesToBuyOrSell=numSharesVec_l)

      # Calc absolute difference in profit at end of period between model strategy and baseline strategy
      finalProfitDiff <- data.frame(buyThreshold=buyThreshold_j,
                                    sellThreshold=sellThreshold_k,
                                    numSharesToBuyOrSell=numSharesVec_l,
                                    finalProfitDiff=outputDF$profit[nrow(outputDF)]-
                                      outputDF$profit_baseline[nrow(outputDF)]) 
      
      # Calc absolute profit at end of period between model strategy and baseline strategy
      finalProfit <- data.frame(buyThreshold=buyThreshold_j,
                                    sellThreshold=sellThreshold_k,
                                    numSharesToBuyOrSell=numSharesVec_l,
                                    finalProfit=outputDF$profit[nrow(outputDF)])

      finalDifferenceInProfitDF <- bind_rows(finalDifferenceInProfitDF, finalProfitDiff)
      finalAbsoluteProfitDF <- bind_rows(finalAbsoluteProfitDF, finalProfit)
    }}}

finalDifferenceInProfitDF <- finalDifferenceInProfitDF[2:nrow(finalDifferenceInProfitDF), ]
finalAbsoluteProfitDF <- finalAbsoluteProfitDF[2:nrow(finalAbsoluteProfitDF), ] # getting rid of NA row I created to make df schema
# filter(finalDifferenceInProfitDF, buyThreshold==1 & sellThreshold==0) # Good sanity check - profit diff should be zero, because with these params you never buy or sell (same strategy as baseline).

#++++++++++++++
# Plot outputs - final profit, and final difference in profit vs baseline, for diff param values
#++++++++++++++

### PLOT FINAL DIFFERENCE IN PROFIT BETWEEN MODEL & BASELINE STRATEGIES

# Change some plotting vars to factors, for better ggplotting
finalDifferenceInProfitDF <- mutate(finalDifferenceInProfitDF, 
                                    sellThreshold=as.factor(sellThreshold),
                                    numSharesToBuyOrSell=as.factor(paste0(numSharesToBuyOrSell, 'sharesTradedPerTrade')))

# Plot
windows()
print(ggplot(finalDifferenceInProfitDF, aes(buyThreshold, finalProfitDiff, colour=sellThreshold)) + geom_line() + 
  facet_wrap(~numSharesToBuyOrSell))
# graphics.off()

### PLOT FINAL DIFFERENCE IN PROFIT BETWEEN MODEL & BASELINE STRATEGIES
# Change some plotting vars to factors, for better ggplotting
finalAbsoluteProfitDF <- mutate(finalAbsoluteProfitDF, 
                                    sellThreshold=as.factor(sellThreshold),
                                    numSharesToBuyOrSell=as.factor(paste0(numSharesToBuyOrSell, 'sharesTradedPerTrade')))

# Plot
windows()
print(ggplot(finalAbsoluteProfitDF, aes(buyThreshold, finalProfit, colour=sellThreshold)) + geom_line() + 
  facet_wrap(~numSharesToBuyOrSell))
# graphics.off()

#++++++++++++++
# Print some final info about best performing model
#++++++++++++++

bestParams <- filter(finalAbsoluteProfitDF, finalProfit==max(finalProfit))
profitDiff_bestParams <- filter(finalDifferenceInProfitDF, sellThreshold==bestParams$sellThreshold, 
                                buyThreshold==bestParams$buyThreshold, 
                                numSharesToBuyOrSell==bestParams$numSharesToBuyOrSell) $finalProfitDiff
annualisedReturn_bestParams <- round(((bestParams$finalProfit+startingMoney)/startingMoney^(365/horizon)-1)*100, 0)
annualisedReturn_baseline <- round((((bestParams$finalProfit-profitDiff_bestParams)+startingMoney)/startingMoney^(365/horizon)-1)*100, 0)

cat(paste0('\nOutputs based on a model with Accuracy of ', round(finalModelAcc, 2), '+', round(finalModelAcc_stdev, 2), 'SD\nBest parameters: buyThreshold=', bestParams$buyThreshold, ' sellThreshold=', bestParams$sellThreshold, ' numSharesToSell=', bestParams$numSharesToBuyOrSell, '\n - this resulted in a total profit of $', round(bestParams$finalProfit, 0), ' after ', horizon, ' days, vs baseline strategy profit of $', round(bestParams$finalProfit-profitDiff_bestParams, 0), ' under same params\n - this is an annualised return of ', annualisedReturn_bestParams,'% and ', annualisedReturn_baseline, '% per year, respectively\n'))


#++++++++++++++
# Plot performance over time for optimal param values 
#++++++++++++++

optDF <- tradingOutcomeFun(startingMoney=startingMoney, propMoneySpentOnShares=propMoneySpentOnShares, 
                           transactionCost=transactionCost, 
                           buyThreshold=bestParams$buyThreshold, 
                           sellThreshold=as.numeric(as.character(bestParams$sellThreshold)), # factorised for plot above
                           numSharesToBuyOrSell=as.numeric(
                             as.character(gsub("sharesTradedPerTrade", "", bestParams$numSharesToBuyOrSell))))

#++++++++++++++
# Plot profit from this strategy vs just holding on to stock (also plotting numShares at same time)
#++++++++++++++

profitsDF <- bind_rows(optDF %>% 
                         select(date, profit) %>%
                         mutate(strategy='Model-based buy & sell'),
                       optDF %>%
                         select(date=date, profit=profit_baseline) %>%
                         mutate(strategy='Baseline (Hold shares & money)'))
numSharesDF <- bind_rows(optDF %>% 
                           select(date, numSharesBeforeTrading) %>%
                           mutate(strategy='Model-based buy & sell'),
                         optDF %>%
                           select(date=date) %>%
                           mutate(numSharesBeforeTrading=optDF$numSharesBeforeTrading[1], strategy='Baseline (Hold shares & money)'))

profitsPlot <- ggplot(profitsDF, aes(date, profit, colour=strategy)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
numSharesPlot <- ggplot(numSharesDF, aes(date, numSharesBeforeTrading, colour=strategy)) + geom_line() + 
  ylim(0, max(numSharesDF$numSharesBeforeTrading*1.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

windows()
grid.newpage() 
grid.draw(rbind(ggplotGrob(numSharesPlot), ggplotGrob(profitsPlot), size = "last")) # plot both together
# graphics.off()

# investigate any anomalies seen in plot
# filter(outputdf, date>'2016-07-07')

gc()

