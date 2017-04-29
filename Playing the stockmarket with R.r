
# NB google trends data starts 01012004 - start finance data here too?

# IMPORTANT THING TO LOOK INTO - HOW DOES YAHOO'S MISSING DATES AFFECT PREDICTIONS FOR 'NEXT DAY'???


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
pathModel <- 'C:/Users/new user/Documents/Playing the stockmarket with R/' # where are the models that predict stock movements

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

# Add in any predictor variables needed for modelling & subsequent prediction----
#__________________________________________________________________________________________________________________________________

# #++++++++++++++
# # NUMBER OF BEAR GOOGLE SEARCHES (CHANGE TO RATIO?)
# #++++++++++++++
# 
# # Some notes on google terms:
# # - [tennis shoes] finds tennis & shoes, order doesn't matter
# # - ["tennis shoes"] finds exact phrase
# # - [tennis + shoes] finds tennis or shoes
# # - [tennis - shoes] finds tennis not shoes
# # - do capitals matter?
# 
# #++++++++++++++
# # get google search data
# #++++++++++++++
# 
# # devtools::install_github('PMassicotte/gtrendsR') # has to be dev version for now
# library(gtrendsR)
# suppressMessages(library('dplyr'))
# gtBearMarket <- gtrends("bear market", time="now+4-H") # could do things like 'sharemarket crash', 'etf', or name of specific fund.
# str(gtBearMarket) # see related queries/keywords - may be useful stuff
# ?gtrends
# 
# gTrendsDF <- gtBearMarket$interest_over_time %>% select(date, hits) %>% arrange(date)
# tail(gTrendsDF)
# data("categories")
# gTrendsDF$date <- as.Date(gTrendsDF$date)           
# gTrendsDF$hitsPrevDay <- c(NA, gTrendsDF$hits[1:(length(gTrendsDF$hits)-1)])
# cat('warning - make hitsPrevDay reflect hits prev *trading* day?\n')
# 
# sharesDF <- left_join(sharesDF, gTrendsDF, by='date')
# 
# 
# 
# # Create ratio of bears to bulls - may be better predictor than including both numBulls and numBears in model
# sharesDF$bullToBearRatio <- sharesDF$numBulls/sharesDF$numBears
# 
# head(sharesDF)
# 
# # # Plot to see how simulation looks
# # plotDF <- sharesDF
# # plotDF$probPriceChangeUp <- with(plotDF, ifelse(priceChange=='down', 0, 1))
# # 
# # windows()
# # ggplot(plotDF, aes(bullToBearRatio, probPriceChangeUp)) + geom_jitter(width=0, alpha=0.5) + geom_smooth()
# # dev.off()

#++++++++++++++
# NUMBER OF BULL VS BEAR TWEETS
#++++++++++++++

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

sharesDF <- left_join(sharesDF, tweetsDF, by='date')
head(sharesDF)

# Create ratio of bears to bulls - may be better predictor than including both numBulls and numBears in model
sharesDF$bullToBearRatio <- sharesDF$numBulls/sharesDF$numBears

head(sharesDF)

# # Plot to see how simulation looks
# plotDF <- sharesDF
# plotDF$probPriceChangeUp <- with(plotDF, ifelse(priceChange=='down', 0, 1))
# 
# windows()
# ggplot(plotDF, aes(bullToBearRatio, probPriceChangeUp)) + geom_jitter(width=0, alpha=0.5) + geom_smooth()
# dev.off()

#__________________________________________________________________________________________________________________________________

# Train model to predict stock market movement next day----
#__________________________________________________________________________________________________________________________________

#++++++++++++++
# Some final data manipulation before modelling
#++++++++++++++

sharesDF$priceChange <- as.factor(sharesDF$priceChange)
sharesDF <- sharesDF[complete.cases(sharesDF),] # NAs introduced by calculating closePriceAfterLag; last [lag] days will be NA

#++++++++++++++
# Run model training function
#++++++++++++++

# Function below takes some rows of sharesDF as input (could be particular folds of data rather than all of sharesDF) and produces a model that predicts the prob of an increase in the sharemarket the next day, given an input dataframe with same schema as sharesDF. Returns a list containing the trained model ('$trainedModel'). Also returns performance metrics of final model - '$trainedModelOutputs$trainedModelAcc' and '$trainedModelOutputs$trainedModelAcc_stdev'
  
cat('Training model to predict prob of sharemarket going up next day\n')  
modelTrainingDF <- sharesDF[1:(nrow(sharesDF)/2),] # specify what data is used to then get split into CV folds within caret. E.g. this could be each fold of the final CV used to estimate overall strategy performance

source(paste0(pathModel, 'trainModelFun_bullVsBearTweets.r'))
trainedModelOutputs <- trainModelFun_bullVsBearTweets(inputDF=modelTrainingDF)

#__________________________________________________________________________________________________________________________________

# For each combination of strategy parameters, apply model to data to predict what shares will do next day, buy/sell according to params + model-predicted values, and track financial outcome at end of each day of this approach. Find strategy params that give best profit.
#__________________________________________________________________________________________________________________________________

# Right now just focusing on buyThreshold, sellThreshold, and numSharesToBuyOrSell... look at others later

cat("\nRecording financial outcome of applying decision: '\n - Buy/sell at end of day (close of business or 'COB'); i.e. dollars spent/earned are based on that day's closePrice. \n - After buying/selling, shares then change value over next [lag] days, and share value is calculated as numShares at COB (after buying/selling) times their value right before COB after [lag] days, i.e. right before buying or selling again. \n - Money value is also calcuated right before COB after [lag] days, but is assumed to stay at same value between COB and COB after [lag] days (could change to match real interest rate - would be sitting in loan offset account so interest is equivalent to home loan interest rate I believe), whereas share price changes.\n - If run out of $ or shares then have to wait until model says sell or buy so until have enough $ / shares to complete next buy/sell:\n")

cat("\nNB currently using same DF that was used to train model ('modelTrainingDF') to pick strategy with best performance. But splitting into 1-year blocks to look at performance each year, so I can find best average performance (+ look at variation in performance). Is this a good idea or should I be testing on independent data? Note that I still test on independent data once I've found the params with best average performance.\n")

# Read in function that takes some rows of sharesDF (e.g. a particular fold of data), uses pre-trained model to predict prob of stockmarket going up next day, makes a decision about whether to buy vs sell based on strategy params, then returns a df that tracks financial outcome of model-predicted vs baseline strategy for each day's trade. Look at actual output of function for details; produces many cols.
source(paste0(pathModel, 'tradingOutcomeFun.r'))

# # Test function:
# outputdf <- tradingOutcomeFun(inputDF=modelTrainingDF, startingMoney=10000, propMoneySpentOnShares=0.75, buyThreshold=0.51, sellThreshold=0.50,
#                               numSharesToBuyOrSell=50, transactionCost=0)

#++++++++++++++
# Divide inputDF into contiguous periods, so I can find params with best average performance in a year-long trial
#++++++++++++++

numPeriods <- 5 # how many 'folds' are we looking at
rowsPerPeriod <- floor(nrow(modelTrainingDF)/numPeriods)

modelTrainingDF_subsets <- list()
for(i in 1:numPeriods){
  modelTrainingDF_subsets[[i]] <- modelTrainingDF[(i*rowsPerPeriod-(rowsPerPeriod-1)):(i*rowsPerPeriod),]
}

#++++++++++++++
# For each data subset, grid search through strategy params to find set of decision rules that gives best performance
#++++++++++++++

bestParamsMat <- matrix(NA, ncol=numPeriods, 
                        nrow=length(buyThresholdVec)*length(sellThresholdVec)*
                          length(numSharesToBuyOrSellVec)) # store results so I can calc average +se profit per param set

for(y in 1:numPeriods){
  
  inputDF <- modelTrainingDF_subsets[[y]]
  finalDifferenceInProfitDF <- data.frame(buyThreshold=NA, sellThreshold=NA, numSharesToBuyOrSell=NA, finalProfitDiff=NA)
  finalAbsoluteProfitDF <- data.frame(buyThreshold=NA, sellThreshold=NA, numSharesToBuyOrSell=NA, finalProfit=NA)
  
  for(j in 1:length(buyThresholdVec)){
    for(k in 1:length(sellThresholdVec)){
      for(l in 1:length(numSharesToBuyOrSellVec)){
        
        buyThreshold_j <- buyThresholdVec[j]
        sellThreshold_k <- sellThresholdVec[k]
        numSharesVec_l <- numSharesToBuyOrSellVec[l]
        
        outputDF <- tradingOutcomeFun(inputDF=inputDF, startingMoney=startingMoney, propMoneySpentOnShares=propMoneySpentOnShares, 
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
  bestParamsMat[, y] <- finalAbsoluteProfitDF$finalProfit
  }

#++++++++++++++
# Summarise results - find param set that gives best average performance per period
#++++++++++++++

avProfitPerStrategyDF <- data.frame(buyThreshold=finalAbsoluteProfitDF$buyThreshold, 
                                    sellThreshold=finalAbsoluteProfitDF$sellThreshold,
                                    numSharesToBuyOrSell=finalAbsoluteProfitDF$numSharesToBuyOrSell, 
                                    meanProfit=rowSums(bestParamsMat),
                                    sdProfit=apply(bestParamsMat, 1, sd)) 

bestParams <- avProfitPerStrategyDF[which.max(avProfitPerStrategyDF$meanProfit),]
bestParams

# make box whisker plot of results, just to see if there are consistent relationships between strategy param values and profit 
library(reshape2)
plotDF <- cbind(avProfitPerStrategyDF, bestParamsMat)
plotDF <- plotDF %>%
  mutate(buyThresh_sellThresh_numShares=paste(buyThreshold, sellThreshold, numSharesToBuyOrSell, sep="_")) %>%
  select(-buyThreshold, -sellThreshold, -numSharesToBuyOrSell, -meanProfit, -sdProfit) %>%
  melt(id.vars='buyThresh_sellThresh_numShares', variable.name='Period', value.name='finalProfitInPeriod')
plotDF$numSharesToBuyAndSell <- as.factor(rep(finalAbsoluteProfitDF$numSharesToBuyOrSell, times=numPeriods))

windows()
print(ggplot(plotDF, aes(buyThresh_sellThresh_numShares, finalProfitInPeriod, colour=numSharesToBuyAndSell)) + geom_boxplot() +
        theme(axis.text.x=element_text(angle=90, hjust=1)) + 
        ggtitle(paste0('Profit per strategy across ', numPeriods, ' ', rowsPerPeriod, 'd periods of test data')))
# graphics.off()


===== TEMPORARY BOOKMARK =====
  



#__________________________________________________________________________________________________________________________________

# Now that I have best set of strategy params for a given model, estimate strategy performance (+ variation in performance) 'in the wild' using a new dataset
#__________________________________________________________________________________________________________________________________

myInputDF_final <- XXX

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

bestParams <- filter(finalAbsoluteProfitDF, finalProfit==max(finalProfit))[1,] # sometimes multiple strats have identical profit.. the [1,] just picks the first.
profitDiff_bestParams <- filter(finalDifferenceInProfitDF, sellThreshold==bestParams$sellThreshold, 
                                buyThreshold==bestParams$buyThreshold, 
                                numSharesToBuyOrSell==bestParams$numSharesToBuyOrSell) $finalProfitDiff
annualisedReturn_bestParams <- round(((bestParams$finalProfit+startingMoney)/startingMoney^(365/horizon)-1)*100, 0)
annualisedReturn_baseline <- round((((bestParams$finalProfit-profitDiff_bestParams)+startingMoney)/startingMoney^(365/horizon)-1)*100, 0)

cat(paste0('\nOutputs based on a model with Accuracy of ', round(trainedModelOutputs$trainedModelAcc, 2), '+', round(trainedModelOutputs$trainedModelAcc_stdev, 2), 'SD\nBest parameters: buyThreshold=', bestParams$buyThreshold, ' sellThreshold=', bestParams$sellThreshold, ' numSharesToSell=', bestParams$numSharesToBuyOrSell, '\n - this resulted in a total profit of $', round(bestParams$finalProfit, 0), ' after ', horizon, ' days, vs baseline strategy profit of $', round(bestParams$finalProfit-profitDiff_bestParams, 0), ' under same params\n - this is an annualised return of ', annualisedReturn_bestParams,'% and ', annualisedReturn_baseline, '% per year, respectively\n'))


#++++++++++++++
# Plot performance over time for optimal param values 
#++++++++++++++
  
optDF <- tradingOutcomeFun(inputDF=myInputDF, startingMoney=startingMoney, propMoneySpentOnShares=propMoneySpentOnShares, 
                           transactionCost=transactionCost, 
                           buyThreshold=bestParams$buyThreshold, 
                           sellThreshold=as.numeric(as.character(bestParams$sellThreshold)), # factorised for plot above
                           numSharesToBuyOrSell=as.numeric(
                             as.character(gsub("sharesTradedPerTrade", "", bestParams$numSharesToBuyOrSell))))
cat('Change inputDF to diff value when looking at optimal values? Right now Ive just chucked myInputDF in, but this will prolly change\n')

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
