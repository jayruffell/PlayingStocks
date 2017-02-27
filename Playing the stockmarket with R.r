# TEST
#__________________________________________________________________________________________________________________________________

# Set parameters and load packages ----
#__________________________________________________________________________________________________________________________________

rm(list=ls())

lag <- 1 # how many days ahead am I predicting? e.g. does numTweets today predict stock price tomorrow, or in two days time? 

# REMAINING PARAMS NOW   GET SPECIFIED IN FUNCTION BELOW, RATHER THAN UP HERE
# startingMoney <- 2000 # money used to buy shares in first place, with some held back to buy more (see param below). In USD I assume (or whatever currency share data is in).
# propMoneySpentOnShares <- 0.75 # need to hold some money back to buy more shares if model says so. 
# buyThreshold <- 0.6
# sellThreshold <- 0.4 # what does model-predicted prob of stock going up have to be before we decide to buy/sell?  
# numSharesToBuyOrSell <- 1 # if above threshold, buy x shares; if below then sell x. NOTE WOULD BE BETTER TO MAKE NUMBER A FUNCTION OF PROB THAT MARKET WILL GO UP OR DOWN; IE IF VERY CONFIDENT THEN SELL MORE 
# transactionCost <- 0 # how many dollars for each day's buying or selling of shares? [should this be a % of transaction value instead?]

warning('Havent sorted lag yet for lag>1. Stock price is correctly lagged, but Im still trading each day rather than every [lag] days, which is what I think I should be doing. Need to investigate\n')

set.seed(0803)
library('quantmod')
suppressMessages(library('dplyr'))
suppressMessages(library('ggplot2'))
library('caret')

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
cat("Currently trading with VTI - this stock is an ETF, so tracks whole market. It's one of the best. Things to note:\n - Cost of holding 10K of VTI stock for 10yr is $46. This is trivial in scheme of things, so ignoring\n - Yield of this stock is 2%; i.e. if trading at $50 you get a $1 dividend at the end of the year. Believe this means that if stock is increasing at 5% per year it's actual increase is more like 7% (cos you could use that dividend to buy 2% more stocks). I'm also ignoring this, but should factor in later\n - Source: https://www.forbes.com/pictures/hefk45ij/10-best-etfs-for-2016/#321329d67806\n")

windows()
chartSeries(VTI) # plot stock performance over time
graphics.off()

warning('Apple shares had massive crash a few years ago - is model going to be able to account for these, or is it too simple? It doesnt account for magnitude of gains or losses at present. E.g. if model tells me direction of shares will be down next day, but I only sell 1 share, and then theres a crash, the model will have predicted correctly but Im still going to lose all my winnings. Do I need to sell/buy much more aggressively?\n')

dates <- index(VTI) # quantmod uses xts format, which requires index() to extract dates

sharesDF <- data.frame(date=dates,  
                       closePrice=Cl(VTI),
                       closePriceAfterLag=Next(Cl(VTI), k=lag)) # Cl() & NExt() are helper functions in quantmod... could just calc directly from data (call "AAPL") but this is easier. k is lag size, i.e k=2 would give price 2days thence.

# Data cleaning - xts does funny things with colnames and row labels, so fixing
rownames(sharesDF) <- NULL
colnames(sharesDF)[grepl('Close', colnames(sharesDF))] <- 'closePrice'
colnames(sharesDF)[grepl('Next', colnames(sharesDF))] <- 'closePriceAfterLag'

# Print info on data Im looking at
cat(paste0(' Modelling shares across full available date range: ', min(sharesDF$date), ' to ',  max(sharesDF$date), '\n stock start price: $', round(sharesDF$closePrice[1], 0), ' // starting money: $', startingMoney, ' // propn spent on shares: ', propMoneySpentOnShares, ' // approx num shares bought (based on start price in training cf testing data): ', floor((startingMoney*propMoneySpentOnShares)/sharesDF$closePrice[1]), '\n'))

warning('Just looking at close price for Apple for now, but theres adjusted price, volume etc in the quantmod data too, which could be useful. Could also add in other shares to make my own composite stock price, or just find a single stock that is a composite\n')

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
                                       rpois(n=nrow(sharesDF), lambda=500) + runif(n=nrow(sharesDF), min=0, max=1000),
                                       rpois(n=nrow(sharesDF), lambda=100) + runif(n=nrow(sharesDF), min=0, max=1000))),
                       numBears=floor(ifelse(sharesDF$priceChange=='down',
                                       rpois(n=nrow(sharesDF), lambda=500) + runif(n=nrow(sharesDF), min=0, max=1000),
                                       rpois(n=nrow(sharesDF), lambda=100) + runif(n=nrow(sharesDF), min=0, max=1000)))) # runif is hack way to add some extra variation and reduce strength of signal

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
graphics.off()

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

timeControl <- trainControl(classProbs=TRUE, summaryFunction=twoClassSummary,
                            method = "timeslice",  # this is better way of calling createTimeSlices function
                            initialWindow = 235, 
                            horizon = 130,      
                            fixedWindow = TRUE) # no idea what best params should be here.. I'm going with train/test split of 60%, and analysing 1yr of data in each resample. NOTE THAT THIS APPROACH WONT CAPTURE LAST ~129 DAYS, COS WINDOW WONT BE BIG ENOUGH. So may be better to do horizon=1 (this is default anyway)

mLR <- train(priceChange ~ bullToBearRatio, data=sharesDF, method="glm", family='binomial', trControl = timeControl)
mLR

#++++++++++++++
# Predict from model on new data
#++++++++++++++

sharesDF$probUp <- predict(mLR$finalModel, type='response')

testdf <- sharesDF[(nrow(sharesDF)-130):nrow(sharesDF), ] # I *think* the final [horizon] rows never get used in model training (see graphs here https://r-norberg.blogspot.co.nz/2016/08/data-splitting-time-slices-with.html), so can use to test on. Alternatively I could hold back another test set to test on... or just do simple training/testing split (like I did above, then hashed out).

plotdf <- mutate(testdf, priceChangeNumeric=ifelse(priceChange=='up', 1, 0))
windows()
ggplot(plotdf, aes(probUp, priceChangeNumeric)) + geom_jitter(alpha=0.1)
graphics.off()

#__________________________________________________________________________________________________________________________________

# Write monster function that takes test data with model-predicted probabilities, makes a decision about whether to buy vs sell based on params, then returns df that tracks financial outcome of model-predicted vs baseline strategy for each day's trade----
#__________________________________________________________________________________________________________________________________

cat("Recording financial outcome of applying decision. Buy/sell at end of day (close of business or 'COB'); i.e. dollars spent/earned are based on that day's closePrice. After buying/selling, shares then change value over next [lag] days, and share value is calculated as numShares at COB (after buying/selling) times their value right before COB after [lag] days, i.e. right before buying or selling again. Money value is also calcuated right before COB after [lag] days, but is assumed to stay at same value between COB and COB after [lag] days (could change to match real interest rate - would be sitting in loan offset account so interest is equivalent to home loan interest rate I believe), whereas share price changes.\n")
  
tradingOutcomeFun <- function(startingMoney, propMoneySpentOnShares, buyThreshold, sellThreshold, 
                              numSharesToBuyOrSell, transactionCost){ # currently uses testdf as input, could make this a param tho. Also haven't added in lag yet - do I need to?
  
  # Testing function - *** HASH OUT ***:
  startingMoney <- 2000 # money used to buy shares in first place, with some held back to buy more (see param below)
  propMoneySpentOnShares <- 0.75 # need to hold some money back to buy more shares if model says so.
  buyThreshold <- 0.6
  sellThreshold <- 0.4 # what does model-predicted prob of stock going up have to be before we decide to buy/sell?
  numSharesToBuyOrSell <- 1 # if above threshold, buy x shares; if below then sell x. NOTE WOULD BE BETTER TO MAKE NUMBER A FUNCTION OF PROB THAT MARKET WILL GO UP OR DOWN; IE IF VERY CONFIDENT THEN SELL MORE
  transactionCost <- 0 # how many dollars for each day's buying or selling of shares? [should this be a % of transaction value instead?]
  cat('Hash out test parameters in function!\n')
  
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
  
  cat(paste0('Buying shares for test data. Shares bought initially: ', startingShareNum, ' // Starting value of shares: $', startingShareValue, ' // Money left over: $', startingMoneyValue, '\n'))
  
  ### SET UP FIRST ROW MANUALLY, THEN DO REST WITH A LOOP THAT REFERENCES FIRST ROW
  
  testdf$numSharesBeforeTrading <- c(startingShareNum, rep(NA, nrow(testdf)-1))
  testdf$moneyBeforeTrading <- c(startingMoneyValue, rep(NA, nrow(testdf)-1))
  testdf$moneyInOrOutAtCOB <- NA
  testdf$numSharesAtCOB <- NA
  testdf$valueOfMoneyAtCOBNextDay <- NA
  testdf$valueOfSharesAtCOBNextDay <- NA
  testdf$profit <- NA
  
  row1 <- testdf[1, ]
  
  # If elses to apply different buying rules depending on whether there's enough money to buy or enough stocks to sell:
  if(row1$decision=='buy'){ # if buying, test whether enough money to buy stocks; if not dont buy and issue warning.
    
    if(row1$closePrice>row1$moneyBeforeTrading){
      cat('insufficient funds to make trade at row 1\n')
      row1$moneyInOrOutAtCOB <- 0
      row1$numSharesAtCOB <- row1$numSharesBeforeTrading
    } else {
      row1$moneyInOrOutAtCOB <- -1*row1$closePrice-transactionCost
      row1$numSharesAtCOB <- row1$numSharesBeforeTrading+numSharesToBuyOrSell
    }
  } else if(row1$decision=='sell'){ # if selling, test enough stocks availalbe; if not dont sell and issue warning.
    
    if(row1$numSharesBeforeTrading<numSharesToBuyOrSell){
      cat('insufficient shares to make trade at row 1\n')
      row1$moneyInOrOutAtCOB <- 0
      row1$numSharesAtCOB <- row1$numSharesBeforeTrading
    } else {
      row1$moneyInOrOutAtCOB <- row1$closePrice-transactionCost
      row1$numSharesAtCOB <- row1$numSharesBeforeTrading-numSharesToBuyOrSell
    }
  } else { # and last option is if decision is 'noAction'.
    
    row1$moneyInOrOutAtCOB <- 0
    row1$numSharesAtCOB <- row1$numSharesBeforeTrading
  }
  
  # Add in values that depend on ifelse results
  row1$valueOfMoneyAtCOBNextDay <- with(row1, moneyBeforeTrading + moneyInOrOutAtCOB)
  row1$valueOfSharesAtCOBNextDay <- with(row1, closePriceAfterLag*numSharesAtCOB)
  row1$profit <- with(row1, valueOfMoneyAtCOBNextDay + valueOfSharesAtCOBNextDay - startingMoney)
  
  testdf[1, ] <- row1 # replacing testdf row 1 with updated version from above
  
  ### NOW USE LOOP TO FILL IN REST OF ROWS
  
  for(i in 1:(nrow(testdf)-1)){
    
    row_iPlus1 <- testdf[i+1,]
    
    row_iPlus1$numSharesBeforeTrading <- testdf$numSharesAtCOB[i]
    row_iPlus1$moneyBeforeTrading <- testdf$valueOfMoneyAtCOBNextDay[i]
    
    # If elses to apply different buying rules depending on whether there's enough money to buy or enough stocks to sell:
    if(row_iPlus1$decision=='buy'){ # if buying, test whether enough money to buy stocks; if not dont buy and issue warning.
      
      if(row_iPlus1$closePrice>row_iPlus1$moneyBeforeTrading){
        cat(paste0('insufficient funds to make trade at row ', i+1, '; cant buy until after next time decision=="sell" & funds get bumped up\n'))
        row_iPlus1$moneyInOrOutAtCOB <- 0
        row_iPlus1$numSharesAtCOB <- row_iPlus1$numSharesBeforeTrading
      } else {
        row_iPlus1$moneyInOrOutAtCOB <- -1*row_iPlus1$closePrice-transactionCost
        row_iPlus1$numSharesAtCOB <- row_iPlus1$numSharesBeforeTrading+numSharesToBuyOrSell
      }
    } else if(row_iPlus1$decision=='sell'){ # if selling, test enough stocks availalbe; if not dont sell and issue warning.
      
      if(row_iPlus1$numSharesBeforeTrading<numSharesToBuyOrSell){
        cat(paste0('insufficient shares to make trade at row ', i+1, '; cant sell until after next time decision=="buy"\n'))
        row_iPlus1$moneyInOrOutAtCOB <- 0
        row_iPlus1$numSharesAtCOB <- row_iPlus1$numSharesBeforeTrading
      } else {
        row_iPlus1$moneyInOrOutAtCOB <- row_iPlus1$closePrice-transactionCost
        row_iPlus1$numSharesAtCOB <- row_iPlus1$numSharesBeforeTrading-numSharesToBuyOrSell
      }
    } else { # and last option is if decision is 'noAction'.
      
      row_iPlus1$moneyInOrOutAtCOB <- 0
      row_iPlus1$numSharesAtCOB <- row_iPlus1$numSharesBeforeTrading
    }
    
    # Add in values that depend on ifelse results
    row_iPlus1$valueOfMoneyAtCOBNextDay <- with(row_iPlus1, moneyBeforeTrading + moneyInOrOutAtCOB)
    row_iPlus1$valueOfSharesAtCOBNextDay <- with(row_iPlus1, closePriceAfterLag*numSharesAtCOB)
    row_iPlus1$profit <- with(row_iPlus1, valueOfMoneyAtCOBNextDay + valueOfSharesAtCOBNextDay - startingMoney)
    
    testdf[i+1,] <- row_iPlus1
  }
  
  testdf$totalAssetsAtCOBNextDay <- with(testdf, valueOfMoneyAtCOBNextDay + valueOfSharesAtCOBNextDay)
  tempProfit <- round(testdf$profit, 2); testdf <- testdf %>% select(-profit) %>% mutate(profit=tempProfit) # reordering
  
  # Check it worked
  # testdf[1:4,]
  # testdf[5:9,]
  # testdf[38:42,] # include row numbers where warning message above said 'not enough $/shares to complete trade', to check that's working
  .
  
  #++++++++++++++
  # Calculate profit of baseline strategy - hold onto shares; no buying or selling
  #++++++++++++++
  
  testdf$numSharesAtCOB_baseline <- startingShareNum
  testdf$valueOfSharesAtCOBNextDay_baseline <- with(testdf, numSharesAtCOB_baseline*closePriceAfterLag)
  testdf$valueOfMoneyAtCOBNextDay_baseline <- startingMoneyValue
  testdf$totalAssetsAtCOBNextDay_baseline <- with(testdf, valueOfMoneyAtCOBNextDay_baseline+valueOfSharesAtCOBNextDay_baseline)
  testdf$profit_baseline <- round(testdf$totalAssetsAtCOBNextDay_baseline-startingMoney, 2)
  
  # head(testdf, 2)
  return(testdf)
}

  
#++++++++++++++
# Test function
#++++++++++++++

outputdf <- tradingOutcomeFun(startingMoney=2000, propMoneySpentOnShares=0.75, buyThreshold=0.6, sellThreshold=0.4, 
                  numSharesToBuyOrSell=1, transactionCost=0)

#++++++++++++++
# Plot profit from this strategy vs just holding on to stock
#++++++++++++++

plotDF <- bind_rows(outputdf %>% 
                      select(date, profit) %>%
                      mutate(strategy='Model-based buy & sell'),
                    outputdf %>%
                      select(date=date, profit=profit_baseline) %>%
                      mutate(strategy='Baseline (Hold shares & money)'))

windows()
ggplot(plotDF, aes(date, profit, colour=strategy)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 week")
graphics.off()


#__________________________________________________________________________________________________________________________________

# Use function above to try different combinations of decision rules (prob cutoffs, amount to trade etc) and look at lift of each method
#__________________________________________________________________________________________________________________________________

# Right now just focusing on buyThreshold, sellThreshold, and numSharesToBuyOrSell... look at others later

#++++++++++++++
# Loop through params to test performance of diff decision rules
#++++++++++++++

buyThresholdVec <- seq(0, 1, 0.25)
sellThresholdVec <- seq(0, 1, 0.25)
numSharesToBuyOrSellVec <- 1:5

resultsDF <- data.frame(buyThreshold=NA, sellThreshold=NA, numSharesToBuyOrSell=NA, profitDiff=NA)

for(j in 1:length(buyThresholdVec)){
  for(k in 1:length(sellThresholdVec)){
    for(l in 1:length(numSharesToBuyOrSellVec)){
      
      buyThreshold_j <- buyThresholdVec[j]
      sellThreshold_k <- sellThresholdVec[k]
      numSharesVec_l <- numSharesToBuyOrSellVec[l]
      
      outputDF <- tradingOutcomeFun(startingMoney=2000, propMoneySpentOnShares=0.75, transactionCost=0,
                                     buyThreshold=buyThreshold_j, sellThreshold=sellThreshold_k, 
                                     numSharesToBuyOrSell=numSharesVec_l)
      finalProfitDiff <- data.frame(buyThreshold=buyThreshold_j,
                                    sellThreshold=sellThreshold_k,
                                    numSharesToBuyOrSell=numSharesVec_l,
                                    profitDiff=outputDF$profit[nrow(outputDF)]-
                                      outputDF$profit_baseline[nrow(outputDF)]) # performance metric is absolute difference in profit at end of period between model strategy and baseline strategy
      resultsDF <- bind_rows(resultsDF, finalProfitDiff)
    }}}

#++++++++++++++
# Plot outputs
#++++++++++++++

resultsDF <- resultsDF[2:nrow(resultsDF), ] # getting rid of NA row I created schema with

windows()
ggplot(resultsDF, aes(buyThreshold, profitDiff, colour=as.factor(sellThreshold))) + geom_line() + facet_wrap(~numSharesToBuyOrSell)
graphics.off()

warning('why isnt profit diff zero when buythreshold=1 and sellthreshold=0? This should mean shares should never be traded, and strategies should be identical')