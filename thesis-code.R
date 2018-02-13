# check for req'd packages, install if not present
list.of.packages <- c("tidyverse", "lubridate", "sas7bdat", "fpp2", "reshape2",
                      "stargazer", "knitcitations", "RefManageR", "xtable", 
                      "kableExtra", "zoo", "tictoc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.us.r-project.org")

# source file containing functions created for this analysis
#source("~/Documents/Grad School/Thesis/github/afit.thesis/custom-functions.R")
source("C:/Users/Jake Elliott/Desktop/afit.thesis/custom-functions.R")

#library loadout
library(sas7bdat)
library(fpp2)
library(zoo)
library(lubridate)
library(reshape2)
library(stargazer)
library(kableExtra)
library(knitr)
library(gridExtra)
library(tictoc)
library(tidyverse)

# set directory for lazy data referencing - allow switch between macOS and Windows
# Basically just set working directory to wherever local repo is held
# setwd("~/Documents/Grad School/Thesis/github/afit.thesis/")
setwd("C:/Users/Jake Elliott/Desktop/afit.thesis/")


###################
#   Import Data   #
###################

# Personnel data
# simple list of AFSCs and description
afsc_list <- read.sas7bdat("Data/lu_ao_afs.sas7bdat")
# monthly records of assigned levels, broken out by AFSC - currently in longform
assigned <- read.sas7bdat("Data/assigned_levels.sas7bdat") %>% 
  spread(AFS, Assigned)
# monthly separation counts, broken out by AFSC - currently in longform
attrition <- read.sas7bdat("Data/separations_count.sas7bdat") %>% 
  spread(AFS, Separation_Count) 

# Econ data
setwd("Data")
econ_data <- list.files(pattern = "*_natl.csv") %>% 
  # Import data sets
  lapply(read_csv) %>% 
  # Merge all sets 
  Reduce(function(x,y) merge(x, y, by = "DATE"), .) %>% 
  # Store data as tibble
  as.tibble() 

# Now, because gdp per cap is observed only on a quarterly basis, we add it separately
# in order to handle the missing values resulting from a merge. This merge is 
# from the previous in that it keeps all values from the existing data set, creating NAs
# where gdp_per_cap does not match with other observations. Merging the quarterly 
# gdp_per_cap data with the monthly indicators results in NAs in gdp_per_cap where 
# the dates do not match. We then handle NAs by extending the quarterly records throughout 
# their respective quarters (e.g. the GDP per cap for Q1 2014 is applied to Jan-Mar 2014).
# Simultaneously, we will rename the variables for interpretability.

# Read in GDP per capita
gdp_per_cap <- read_csv("real09_gdp_percap.csv")
# Combine gdp per cap with econ_data, using a left-join (all.x = TRUE) to preserve the main data set
econ_data <- merge(econ_data, gdp_per_cap, by = "DATE", all.x = TRUE) %>%
  as.tibble() %>%
  # Rename column headers to something more meaningful
  select(Unemployment.Rate.Adj = UNRATE, Unemployment.Rate.NonAdj = UNRATENSA,
         CPI.Adj = CPIAUCSL, Nonfarm.Jobs.Adj = JTSJOL, 
         Nonfarm.Jobs.NonAdj = JTUJOL, Labor.Force.Participation = LNS11327662, 
         Labor.Market.Momentum = FRBKCLMCIM, Real.GDP.Per.Capita = A939RX0Q048SBEA, 
         Nonfarm.Quits.Adj = JTSQUL, Date = DATE)

# The na.locf() command below carries a value forward through NAs until the next non-empty value is met; 
# this is how we choose to represent the gdp per capita through an entire quarter
econ_data$Real.GDP.Per.Capita <- as.numeric(econ_data$Real.GDP.Per.Capita) %>% 
  na.locf()


#######################################
#   Data Cleaning and Preparation     #
#######################################

# The dates in the personnel data are in total days since 1 Jan 1960 (SAS default)
# We'll need to reformat the date into something readable and mergeable with our 
# econ set. Also, we create a new column - the total number of separations across 
# all AFSCs. We'll also create totals for different categories of officers: rated, 
# non-rated, line, etc.
attrition <- mutate(attrition, Total = rowSums(attrition[-1], na.rm = TRUE)) %>% 
  mutate(temp = EOP_Date * 86400) %>% 
  mutate(Date = as.POSIXct(temp, origin = "1960-01-01")) %>% 
  within(rm(temp, EOP_Date))

# repeat prodcedure for assigned data
assigned <- mutate(assigned, Total = rowSums(assigned[-1], na.rm = TRUE)) %>% 
  mutate(temp = EOP_Date * 86400) %>% 
  mutate(Date = as.POSIXct(temp, origin = "1960-01-01")) %>% 
  within(rm(temp, EOP_Date))

# However, the date variables differ slightly between the econ and personnel sets:
# Though both represent monthly observations, the econ set default to the first 
# of each month, and the personnel data defaulted to the last 
# (e.g. October 2004 is represented as 01-10-2004 in the former, and 31-10-2004 in the latter)
# To handle this, we'll create new date variables in each set that have the days 
# trimmed off, then merge. Merging isn't strictly necessary, but it is a convenient
# way to only keep those observations common to both data sets.
econ_data <- mutate(econ_data, "Date1" = paste(year(Date), month(Date)))
attrition <- mutate(attrition, "Date1" = paste(year(Date), month(Date)))
# Merge data sets
df <- merge(econ_data, attrition, by = "Date1")

# Next, we see many NAs within the attrition data set. Given the data's nature, 
# our intuition was that these missing values aren't a result of encoding error 
# or similar, but rather an indication that no separations occurred during 
# that period (i.e. NAs indicate no separations were observed, instead of
# indicating some sort of error). This intuition was confirmed by the data's
# provider, HAF/A1FPX.
df[is.na(df)] <- 0

# Next we'll go ahead and drop all of our date variables. When we use df
# to create a time series object, the date variables become redundant.
df <- df[, !(names(df) %in% c("Date1", "Date.x", "Date.y"))]

# Now we'll initialize the time series object - start = Oct 2004, freq = 12 - 
# and create the validation and training sets. Since we're only really interested
# in the Total column for modeling purposes
df.ts.1 <- ts(df, start = c(2004, 10), frequency = 12)
train.ts.1 <- subset(df.ts.1, end = 127)
val.ts.1 <- subset(df.ts.1, start = 128)

#########################################
#   Initial Exploration and Modeling    #
#########################################

# Let's take an initial, unmodified look at our response - total separations across
# all officer AFSCs. We see some pretty substantial spikes; fortunately, we know
# from the sponsor that they are artificially high. Special incentive programs 
# for separation were implemented in the same years containing the spikes. So,
# we can do something about those observations - remove them, impute and replace, etc.
autoplot(df.ts.1[,'Total'], ylab = "Total Separations")

# We also will want to look for evidence of seasonality or for any one year that 
# stands out. Grouping the separations by year, we can see that the tail ends of 
# 2005, 2006, 2007 and 2014 were higher than other years (we saw this in the 
# previous plot as well). Aside from those periods, however, no individual year 
# stands out. We do notice, though, that separations appear to have a bowed shape
# as the years progress. That is, slightly higher levels of separation at the beginning
# and ends of the calenday year, with lower rates of separation during summer months.
# We may have to account for this seasonality in our modeling by transforming the data.
p <- ggseasonplot(df.ts.1[,'Total'], year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Total Separations") + 
  ggtitle("Seasonal Plot: Total Separations") + 
  theme(legend.position = "none")
p  

# However, we might want to try modeling before making any alterations to the data.
# It very well could be that we don't need to replace or impute values, that we're 
# able to forecast fairly accurately without adjustments. We'll start with 
# some naive forecasts against which we compare future forecasts. 

n.1 <- naive(train.ts.1[,"Total"], h = dim(val.ts.1)[1])
sn.1 <- snaive(train.ts.1[,"Total"], h = dim(val.ts.1)[1])

n.1.error <- accuracy(n.1, val.ts.1[,"Total"])
sn.1.error <- accuracy(sn.1, val.ts.1[,"Total"])

# There are two takeaways from results below:

# First, we see that the seasonal model performs worse on the validation set, 
# indicating that it is possibly overfit or overly affected by some outliers
kable(n.1.error, caption = "Naive Performance", digits = 3, align = 'c')
kable(sn.1.error, caption = "Seasonal Naive Performance", digits = 3, align = 'c')


# Plotting the forecasts against the validation data, we can see that outliers
# might be the source of the problem. The last spike, around 2014-2015, is carried
# through in the forecasts, resulting in high errors. We know from an in-depth 
# discussion of the actual data that spike is an aberration. From this, we infer
# that outliers are going to be a problem, and probably ought to be handled.
autoplot(n.1) +
  autolayer(val.ts.1[,"Total"]) +
  theme(legend.position = "none") +
  ylab("Total Attrition")

autoplot(sn.1) +
  autolayer(val.ts.1[,"Total"]) +
  theme(legend.position = "none") +
  ylab("Total Attrition")

# First, though, we need to identify which exact data points are outliers. We can 
# refer back to our season plot to help. On visual inspection, it appears that 
# roughly Oct-Dec of '05-'07 and '14 stand out (possibly Sep '07, as well).
# These observations are backed by insight provided by the data's sponsor - HAF/A1.
# From them, we've found out that in 2006, '07, and '14 special separation
# programs were instituted in order to incentivize attrition. So, these outlying 
# points probably reflect the effects of those special programs.
p

# To handle these, we'll calculate the average values of all other years during 
# months and replace the current values. First, let's create slices of our
# response containing the months we're concerned with - December and November.
# We also want to grab the correpsonding indices for updating our series later.

dec <- subset(df.ts.1[,'Total'], month = 12)
dec.ind <- which(cycle(df.ts.1[,'Total']) == 12)
nov <- subset(df.ts.1[,'Total'], month = 11)
nov.ind <- which(cycle(df.ts.1[,'Total']) == 11)

# oct <- subset(df.ts.1[,'Total'], month = 10)
# sep <- subset(df.ts.1[,'Total'], month = 9)

# Referring back to p, and combining the graphical insights with information
# from the data's sponsor, we assume that 2006, '07, and '14 are the years which
# saw the largest effects from the separation incentive programs - i.e. artificial
# attrition. Those correspond the the 3rd, 4th, and 11th indices. So now, we 
# replace those observations with the average of the non-aberrant years.

dec[c(3,4,11)] <- mean(dec[-c(3,4,11)])
nov[c(3,4,11)] <- mean(nov[-c(3,4,11)])

# And finally, we place these values back into the original series.
df.ts.1[dec.ind, 'Total'] <- dec
df.ts.1[nov.ind, 'Total'] <- nov

# Revisiting the response and seasonality plots, we can more easily see the 
# effects of seasonality, and a much more stationary data set without any 
# egregious outliers.

autoplot(df.ts.1[,'Total'], ylab = "Total Separations")

ggseasonplot(df.ts.1[,'Total'], year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Total Separations") + 
  ggtitle("Seasonal Plot: Total Separations") + 
  theme(legend.position = "none")

# Lastly, we might want to retrain and assess our naive models so see if removing
# those outliers effected much

# store split index
set.split <- 127
# New train and val sets
train.ts.2 <- subset(df.ts.1[,'Total'], end = set.split)
val.ts.2 <- subset(df.ts.1[,'Total'], start = set.split+1)

# Train models and generate errors
n.2 <- naive(train.ts.2, h = length(val.ts.2))
sn.2 <- snaive(train.ts.2, h = length(val.ts.2))

n.2.error <- accuracy(n.2, val.ts.2)
sn.2.error <- accuracy(sn.2, val.ts.2)

# Compare the errors
kable(n.1.error, caption = "Naive Performance")
kable(n.2.error, caption = "Naive Performance")

kable(sn.1.error, caption = "Seasonal Naive Performance")
kable(sn.2.error, caption = "Seasonal Naive Performance")

# Aaaaaaand compare plots, forecasts

# Nothing too special about these
autoplot(n.2) +
  autolayer(val.ts.2) +
  theme(legend.position = "none") +
  ylab("Total Attrition")

# Here we see that the variation in the validation set is more closely followed 
# by the forecasts. We infer that removing the outliers was beneficial.
autoplot(sn.2) +
  autolayer(val.ts.2) +
  theme(legend.position = "none") +
  ylab("Total Attrition")

# Though our naive models aren't particularly useful for providing forecasts 
# (or identifying key economic indicators), they're useful for providing a 
# baseline comparison for other models. The idea being naive models are our 
# simplest methods, and we'll compare the performance of more sophisticated 
# against them - models such as...

# A multivariate regression model with ARIMA errors. Why this? Because a 
# multivariate regression model allows us to include outside variables (economic 
# indicators) to help predict a response (attrition). The problem with just 
# regression, though, is that regression assumes independent errors, and we 
# often find autocorrelation with time-series data. Enter the 
# ARIMA: fitting an ARIMA model on our regression error, then, allows us to 
# handle the autocorrelative nature of the data, but does not allow room for 
# any exogeneous information (i.e. info other than the response).

# Separately, each of those methods provides roughly half of what we're looking
# to model. So, by our powers combined: We'll relax the assumption of independent
# errors in the regression model, and instead assume that they ARE autocorrelated.
# And since we have a model for predicting autocorrelated data, we now treat the
# 'error' term in the regression model as its own ARIMA model (technical 
# formulation is in the thesis; you can also just search Google for
# "Regression with ARIMA errors"). We're left with a model that, when correctly
# specified, should provide both forecasts for our response and insight as to 
# which variables contribute to those forecasts (response variance, etc).

# Now, before go fitting our data, we need to take some steps to ensure we're 
# fitting it properly - there are other assumptions involved here. First, we need
# independent regressors. Collinearity between our regressor variables will 
# inflate regression coefficients' variances; we won't have a good idea of how 
# influential our economic indicators are. To avoid these issues, we'll build a 
# heat map showing the correlation for every pairwise combination in our set of
# economic indicators.

# generate actual correlation heatmap
# Note: reorder.cormat(), get.upper.tri(), and %ni% are custom functions whose code
# can be found in the script custom-functions.R
df[which(names(econ_data) %ni% c("Date", "Date1"))] %>% 
  cor() %>%
  round(2) %>% 
  reorder.cormat() %>%
  get.upper.tri() %>% 
  melt(na.rm = TRUE) %>% 
  ggplot(aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Collinearity") +
  theme_minimal() + # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, 
                               title.position = "top", title.hjust = 0.5))

# The heatmap shows many isntances of collinearity, which is expected - many 
# economic indicators are variations or flavors of the same information. However,
# some non-correlated groups are shown. For our initial model, we select the 
# Labor Force Participation Rate, Market Momentum Index, and the Unemployment 
# Rate. Though the first two have a noticeable correlation, we suspect they will
# still provide information for our model - the correlation isn't too strong 
# anyway. The model can be adjusted and specified later, this is just a first 
# stab.

# grab index of econ vars: LFPR, Unem.Adj, LMM
econ.vars <- which(names(df) %in% c("Labor.Force.Participation",
                                            "Unemployment.Rate.Adj", 
                                            "Labor.Market.Momentum"))

# Now that we've selected our regressors, we need to check for stationarity. We're
# looking for evidence of non-zero trend, seasonality, etc.

autoplot(df.ts.1[,econ.vars], facets = TRUE) +
  ylab("")

# Yikes, okay not good. Definitely non-stationary. Let's try differencing, and 
# see if that improves the situation.

econ.vars.d <- diff(df.ts.1[,econ.vars])

autoplot(econ.vars.d, facets = TRUE) +
  ylab("")

# Oh, yea - way better. Differenced, these variables look useable. Now while 
# we're looking at differencing, we should look to see if our response also
# needs to be differenced. Remember, we have this:

autoplot(df.ts.1[,"Total"]) +
  ylab("Total Attrition")

# Hmm, nothing too crazy, actually - is there any statistical evidence
# for differencing?

# regular differencing
ndiffs(df.ts.1[,"Total"])
# seasonal differencing
nsdiffs(df.ts.1[,"Total"])

# Neat! However, before we can build the model, we have to account for the
# differencing performed on our regressors. With simple differencing, we lose
# the first observation, and so we remove the first observation from our 
# response.

head(df.ts.1[,"Total"])
head(econ.vars.d[,1])

response.ts <- ts(df.ts.1[-1,"Total"], start = c(2004, 11), frequency = 12)

head(response.ts)

# We also need to split up the econ vars into training and test sets

# store split index
set.split <- 126

# subset our response
train.ts.3 <- subset(response.ts, end = set.split)
val.ts.3 <- subset(response.ts, start = set.split+1)

# subset econ variables
econ.vars.d.train <- subset(econ.vars.d, end = set.split)
econ.vars.d.val <- subset(econ.vars.d, start = set.split+1)

# We'll utilize the auto arima function from fpp2
tic("dynamic regression")
dyn.reg.1 <- auto.arima(train.ts.3, xreg = econ.vars.d.train, trace = TRUE,
                        stepwise = FALSE, approximation = FALSE)
toc()
# The first thing we'll want to check, after the model summary, is how our 
# residuals behave. Do they appear to satisfy normality assumptions? Are there 
# any outliers? Evidence of leftover autocorrelation? Essentially, we're 
# checking to see if there's anything other than white noise in the error term.

# No - to all of the above. ACF plots look clean, raw residuals seem 
# noisy, and also have a roughly normal distribution. Furthermore, a Ljung-Box
# test shows no evidence that the data aren't normal (p: 0.8121).
checkresiduals(dyn.reg.1)

# Let's generate some forecasts then
dyn.reg.1.f <- forecast(dyn.reg.1, xreg = econ.vars.d.val, h = 20)

# We'll also take a look at some accuracy measures. According to RMSE and MASE,
# the dynamic regression model performs better than a seasonal naive estimation;
# that's good news - we're getting closer towards accurate forecasting.
accuracy(dyn.reg.1.f, val.ts.3)

# And then plot those forecasts over the actual data
autoplot(dyn.reg.1.f) + 
  autolayer(dyn.reg.1$fitted) +
  autolayer(val.ts.3) +
  theme(legend.position = "none") +
  ylab("Total Attrition")

# In short, our residuals are clean, error is improved from that of naive methods,
# and forecasts track the validation set fairly well. There's one glaring problem,
# however. Looking back at our model summary, it's clear that none of the estimated
# coefficients for the economic indicators are statistically significant. Boo.
# This could be for several reasons:
#
# 1) indicators are month-to-month changes, don't have large enough fluctuations
# to cause significant changes
# 
# 2) no lagged information - i.e. current economic info probably doesn't affect 
# current attrition rate
#
# 3) data might be too aggregated, contains too much noise to establish significant
# relationships

# In summary, dynreg gives better forecasts than naive models, but current 
# specification doesn't reveal much in the way of economic insight

#####################################
#   Specification - Lagged Effects  #
#####################################

# Initially, we'll test 6, 12, 18, and 24 months for each indicator. We'll 
# build a framework for any desired time-lag later.


# lazy, inefficient lagged variable build - if there's time later we'll build a
# function and generalize this process

Unemployment.Rate.lag <- cbind(
  lag0 = econ.vars.d[,"Unemployment.Rate.Adj"],
  lag6 = stats::lag(econ.vars.d[,"Unemployment.Rate.Adj"], -6),
  lag12 = stats::lag(econ.vars.d[,"Unemployment.Rate.Adj"], -12),
  lag18 = stats::lag(econ.vars.d[,"Unemployment.Rate.Adj"], -18), 
  lag24 = stats::lag(econ.vars.d[,"Unemployment.Rate.Adj"], -24)
)

Labor.Force.Participation.lag <- cbind(
  lag0 = econ.vars.d[,"Labor.Force.Participation"],
  lag6 = stats::lag(econ.vars.d[,"Labor.Force.Participation"],-6),
  lag12 = stats::lag(econ.vars.d[,"Labor.Force.Participation"], -12),
  lag18 = stats::lag(econ.vars.d[,"Labor.Force.Participation"], -18), 
  lag24 = stats::lag(econ.vars.d[,"Labor.Force.Participation"], -24)
)

Labor.Market.Momentum.lag <- cbind(
  lag0 = econ.vars.d[,"Labor.Market.Momentum"],
  lag6 = stats::lag(econ.vars.d[,"Labor.Market.Momentum"], -6),
  lag12 = stats::lag(econ.vars.d[,"Labor.Market.Momentum"], -12),
  lag18 = stats::lag(econ.vars.d[,"Labor.Market.Momentum"], -18), 
  lag24 = stats::lag(econ.vars.d[,"Labor.Market.Momentum"], -24)
)

# create train and val splits
UR.lag.train <- subset(Unemployment.Rate.lag, end = set.split)
UR.lag.val <- subset(Unemployment.Rate.lag, start = set.split+1, 
                     end = dim(econ.vars.d)[1])

LFPR.lag.train <- subset(Labor.Force.Participation.lag, end = set.split)
LFPR.lag.val <- subset(Labor.Force.Participation.lag, start = set.split+1, 
                     end = dim(econ.vars.d)[1])

LMM.lag.train <- subset(Labor.Market.Momentum.lag, end = set.split)
LMM.lag.val <- subset(Labor.Market.Momentum.lag, start = set.split+1, 
                     end = dim(econ.vars.d)[1])

# Initialize table to store results of loop. We are going to capture the 
# variable combination, AICc, training RMSE, and validation RMSE. Tibble 
# generated, saved to local .rds so we won't have to re-run this awful loop; 
# runtime is approximately 1 hr on 4-core machine, process run in parallel


# lag.results <- tibble("UR.lag" = rep(NA, 125),
#                       "LFPR.lag" = rep(NA, 125),
#                       "LMM.lag" = rep(NA, 125),
#                       "AICc" = rep(NA, 125),
#                       "Training.RMSE" = rep(NA, 125),
#                       "Validation.RMSE" = rep(NA, 125))
# 
# m <- 1
# for(i in c(1:5)){
#   for(j in c(1:5)){
#     for(k in c(1:5)){
#       
#       xreg.train <- cbind(UR.lag.train[,i],
#                           LFPR.lag.train[,j],
#                           LMM.lag.train[,k])
#       
#       xreg.val <- cbind(UR.lag.val[,i],
#                         LFPR.lag.val[,j],
#                         LMM.lag.val[,k])
#       
#       dyn.model <- auto.arima(train.ts.3,
#                               xreg = xreg.train, 
#                               stepwise = FALSE, 
#                               approximation = FALSE,
#                               parallel = TRUE)
#       
#       dyn.model.f <- forecast(dyn.model, xreg = xreg.val, h = 20)
#       
#       dyn.model.err <- accuracy(dyn.model.f, val.ts.3)
#       
#       lag.results[m, "UR.lag"] <- colnames(UR.lag.train)[i]
#       lag.results[m, "LFPR.lag"] <- colnames(LFPR.lag.train)[j]
#       lag.results[m, "LMM.lag"] <- colnames(LMM.lag.train)[k]
#       lag.results[m, "AICc"] <- dyn.model$aicc
#       lag.results[m, "Training.RMSE"] <- dyn.model.err[1,2]
#       lag.results[m, "Validation.RMSE"] <- dyn.model.err[2,2]
#       
#       m <- m + 1
#     }
#   }
# }
# 
# saveRDS(lag.results, "lagResults.rds")

# read in compiled lag.results
lag.results <- readRDS("lagResults.rds")

# summarize the selection criteria
summary(lag.results[,4:6])

# filter tibble to return rows where each metric is below respective first quartile
lag.results %>% 
  filter(lag.results[,"Validation.RMSE"] <= 156.4 & 
           lag.results[,"Training.RMSE"] <= 133.1 & 
           lag.results[,"AICc"] <= 1299)

# only one result, might want to go back and loosen filter criteria - for now
# let's investigate that one model

# best across three
xreg.train <- cbind(UR.lag.train[,"lag24"],
                    LFPR.lag.train[,"lag18"],
                    LMM.lag.train[,"lag24"])

xreg.val <- cbind(UR.lag.val[,"lag24"],
                    LFPR.lag.val[,"lag18"],
                    LMM.lag.val[,"lag24"])

dyn.reg.2 <- auto.arima(train.ts.3,
                        xreg = xreg.train,
                        stepwise = FALSE,
                        approximation = FALSE)

dyn.reg.2.f <- forecast(dyn.reg.2, xreg = xreg.val, h = 20)

autoplot(dyn.reg.2.f) + 
  autolayer(dyn.reg.2$fitted) +
  autolayer(val.ts.3) +
  theme(legend.position = "none") +
  ylab("Total Attrition")

# we can also identify the 'top' model by the minimum of each criteria
rbind(lag.results %>% 
        filter(AICc == min(AICc)),
      lag.results %>% 
        filter(Training.RMSE == min(Training.RMSE)),
      lag.results %>% 
        filter(Validation.RMSE == min(Validation.RMSE)))

# take best five models according to each criteria and see if there are any
# commonalities

best.by.AICc <- lag.results %>% 
                  arrange(AICc) %>% 
                  head(5) 

best.by.trainingRMSE <- lag.results %>% 
                          arrange(Training.RMSE) %>% 
                          head(5)

best.by.validationRMSE <- lag.results %>% 
                            arrange(Validation.RMSE) %>% 
                            head(5)

