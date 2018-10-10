#################################### WQD7004: ASSIGNMENT 2 #######################################

# Name : NG KANG WEI
# Student ID : WQD170068

path <- file.path("E:\\academic\\mds-sem2\\WQD7004-Rprogramming\\2008.csv")
myDF <- read.csv(path)
library(dplyr)

# 1) Sort in decreasing order the ten most popular airports according to the number of origins of flights
# # (hint : use decreasing=T and indexing )
flightOrigin <- table(myDF$Origin)
sort(flightOrigin, decreasing=TRUE)[1:10]

# dplyr
popularOrg <- myDF %>% group_by(Origin) %>% count(sort=TRUE)
popularOrg[1:10,]

# 2) Assign the names of the ten most popular airports according to the number of origins of flights to 
## variable called mostPopularOrg
# # (hint : use names() and indexing)
mostPopularOrg <- names(sort(flightOrigin, decreasing=TRUE))[1:10]
mostPopularOrg

# dplyr
namesOfPopOrg <- popularOrg[1:10,] %>% select(Origin)
namesOfPopOrg


# 3) Assign the names of the ten most popular airports according to the number of destinations of flights to 
## variable called mostPopularDes
flightDestination <- table(myDF$Dest)
mostPopularDes <- names(sort(flightDestination, decreasing=TRUE))[1:10]
mostPopularDes

# dplyr
mostPopularDest <- myDF %>% group_by(Dest) %>% count(sort=T) 
mostPopularDest <- mostPopularDest[1:10,] %>% select(Dest)
mostPopularDest

# 4) How many flights had their origin in one of these 10 most popular airports
## (hint : use %in%)
sum(myDF$Origin %in% mostPopularDes)

# dplyr
myDF %>% filter(Origin %in% pull(namesOfPopOrg)) %>% nrow

# 5)How many flights had their destinationn in one of these 10 most popular airports
## (hint : use %in%)
sum(myDF$Dest %in% mostPopularDes)

# dplyr
myDF %>% filter(Dest %in% pull(mostPopularDest)) %>% nrow

# 6) Find flights for which the origin and the destination
# were among the 10 most popular airports
## (hint : use %in%)
sum(myDF$Origin %in% mostPopularOrg & myDF$Dest %in% mostPopularDes)

# dplyr
myDF %>% filter(Origin %in% pull(namesOfPopOrg), Dest %in% pull(mostPopularDest)) %>% nrow

# 7) For the purposes of this question, treat the group 
# of the 200 least popular airports according to the 
# number of flights having these as the origins.
# How many flights had one of these 200 least popular 
# airports as their origin?
leastPopularOrg <- sort(flightOrigin)[1:200]
sum(leastPopularOrg)

# dplyr
leastPopularAirport <- myDF %>% group_by(Origin) %>% count(sort=T) %>% tail(n=200) %>% select(Origin)
leastPopularAirport
myDF %>% filter(Origin %in% pull(leastPopularAirport)) %>% nrow

# 8) Index a vector according to the names of the elements in the vector
##8a) How many flights departed from "IND" ?
flightOrigin['IND']

# dplyr
myDF %>% filter(Origin=='IND') %>% nrow

##8b) How many flights departed from "IND","ORD","JFK","EWR","IAD" ?
flightOrigin[c('IND', 'ORD', 'JFK', 'EWR', 'IAD')]

# dplyr
myDF %>% filter(Origin %in% c('IND', 'ORD', 'JFK', 'EWR', 'IAD')) %>% group_by(Origin) %>% tally()

##8c) How many flights departed from each of the 10 most popular airports ?
flightOrigin[mostPopularOrg]

# dplyr
myDF %>% filter(Origin %in% mostPopularOrg) %>% group_by(Origin) %>% tally

##8d) How many flights departed from each of the 200 least popular airports ?
flightOrigin[names(leastPopularOrg)]

# dplyr
flightsFromLeastPop <- myDF %>% filter(Origin %in% pull(leastPopularAirport)) %>% group_by(Origin) %>% tally(sort=T)
glimpse(flightsFromLeastPop)

# 8e) How many flights landed at Ronald Reagan Washington
## National ("DCA") or Washington Dulles Airport ("IAD") in 2008? 
flightDestination[c('DCA', 'IAD')]

# dplyr
myDF %>% filter(Dest %in% c('DCA', 'IAD')) %>% group_by(Dest) %>% tally

# 9)Check the first 20 flights and see which one departed on time or early
first20 <- head(myDF, 20)
first20DepOnTimeEarly <- subset(first20, first20$DepDelay <= 0); first20DepOnTimeEarly
nrow(first20DepOnTimeEarly)

# dplyr
myDF %>% slice(1:20) %>% filter(DepDelay <= 0) %>% nrow

##9a) Restrict attention to only the 10 most popular airports 
##and see which one departed on time or early
mostPopDepOnTimeEarly <- tapply(myDF$DepDelay <= 0, myDF$Origin, sum, na.rm=TRUE)[mostPopularOrg]
mostPopDepOnTimeEarly

# dplyr
depEarly <- myDF %>% filter(Origin %in% pull(namesOfPopOrg), DepDelay <= 0) %>% group_by(Origin) %>% tally()

##9b)Find the percentage of flights at each of the 10 most popular 
# airports that departed on time or early
mostPopDepOnTimeEarly / flightOrigin[mostPopularOrg]  * 100

# dplyr
flightFromMostPop <- myDF %>% filter(Origin %in% pull(namesOfPopOrg)) %>% group_by(Origin) %>% tally
flightFromMostPop %>% mutate(EarlyFlightPct = depEarly$n / n * 100) %>% select(Origin, EarlyFlightPct)

# 9c) What percentage of flights departed from IND on time or early?
sum(myDF$Origin=='IND' & myDF$DepDelay <= 0, na.rm=TRUE) / flightOrigin['IND'] * 100

# dplyr
earlyFlightInd <- myDF %>% filter(Origin=='IND', DepDelay<=0) %>% tally
allFlightInd <- myDF %>% filter(Origin=='IND') %>% tally
earlyFlightInd / allFlightInd * 100

#10) Analyze Flights by Origin Airport and Month of Departure
##10a) Break the data in the DepDelay vector according to which city of origin 
depOrg <- tapply(myDF$DepDelay, myDF$Origin, length)
head(depOrg)

# dplyr
myDF %>% select(DepDelay, Origin) %>% group_by(Origin) %>% tally

##10b) Break the data in the DepDelay vector according to month
depMonth <- tapply(myDF$DepDelay, myDF$Month, length)
head(depMonth)

# dplyr
myDF %>% select(DepDelay, Month) %>% group_by(Month) %>% tally

#11) How many flights delay occur from each airport in each month ?
tapply(myDF$DepDelay > 0, list(myDF$Origin, myDF$Month), sum, na.rm=T)

# dplyr
myDF %>% select(DepDelay, Month, Origin) %>% filter(DepDelay > 0) %>% group_by(Origin, Month) %>% tally

##11a) Extract the data from origin airport = "IND"
# and from the month of June
tapply(myDF$DepDelay > 0, list(myDF$Origin, myDF$Month), sum, na.rm=T)['IND', 6]

# dplyr
myDF %>% filter(DepDelay > 0, Origin=='IND', Month==6) %>% tally

##11b) Extract the data from origin airport = "ATL"
# and from the month of March
tapply(myDF$DepDelay > 0, list(myDF$Origin, myDF$Month), sum, na.rm=T)['ATL',3]

# dplyr
myDF %>% filter(DepDelay > 0, Origin=='ATL', Month==3) %>% tally

# 11c) The number of flights delay from 3 airports = "ATL","AUS","BDL"
# during the months of July through October
flightDelay3airport <- tapply(myDF$DepDelay > 0, list(myDF$Origin, myDF$Month), sum, na.rm=T)[c('ATL', 'AUS', 'BDL'), 7:10]
flightDelay3airport

# dplyr
delayedFlight3Airport <- myDF %>% filter(DepDelay > 0, Origin %in% c('ATL', 'AUS', 'BDL'), Month %in% c(7:10)) %>% group_by(Month, Origin) %>% tally
delayedFlight3Airport

# 11d) How many delayed departure flights altogether from ATL, AUS, and BDL during the months of 
#July 2008 through October 2008?
sum(flightDelay3airport)
colSums(flightDelay3airport)
rowSums(flightDelay3airport)

# dplyr
sum(delayedFlight3Airport$n)

# 11e) All the flight delays, month by month, frm IND airport
tapply(myDF$DepDelay > 0, list(myDF$Origin, myDF$Month), sum, na.rm=T)['IND', ]

# dplyr
myDF %>% filter(Origin=='IND', DepDelay > 0) %>% group_by(Month) %>% tally

# 11f) All the flight delays, month by month, frm both IND and ORD at once
flightDelayIndOrd <- tapply(myDF$DepDelay > 0, list(myDF$Origin, myDF$Month), sum, na.rm=T)[c('IND', 'ORD'),]
flightDelayIndOrd

# dplyr
delayedFlightIndOrd <- myDF %>% filter(Origin %in% c('IND', 'ORD'), DepDelay >0) %>% group_by(Month, Origin) %>% tally
delayedFlightIndOrd

# 12) Calculating Percentages of Flights with delayed more than 30 minutes when departing
moreThan30min <- subset(myDF, myDF$DepDelay > 30)
delayedFlight <- tapply(moreThan30min$DepDelay, list(moreThan30min$Origin, moreThan30min$Month), length)[c('IND', 'ORD'),]
pct <- delayedFlight / flightDelayIndOrd * 100
pct

# dplyr
lateDelayed <- myDF %>% filter(DepDelay > 30, Origin %in% c('IND', 'ORD')) %>% group_by(Origin, Month) %>% tally
lateDelayed <- as.data.frame(lateDelayed)
allDelayed <- myDF %>% filter(DepDelay > 0, Origin %in% c('IND', 'ORD')) %>% group_by(Origin, Month) %>% tally
allDelayed <-  as.data.frame(allDelayed)
allDelayed <- allDelayed %>% mutate(LatePercentages = lateDelayed$n / n * 100) %>% select(Origin, Month, LatePercentages)

# 12a) find the percentage of flights with long delays and plot with dotchart()
dotchart(pct)

# dplyr
dotchart(allDelayed$LatePercentages, allDelayed$Origin, allDelayed$Month, 
         main = "Percentages of Late Flights in IND and ORD month by month in 2008", 
         xlab="Percentages of late flights", ylab="Airport and Months")

# 12b) How many flights departed altogether from IND 
# or ORD in 2008 with a delay of more than 30 minutes each?
sum(delayedFlight)

# dplyr
myDF %>% filter(DepDelay > 30, Origin %in% c('IND', 'ORD')) %>% nrow

#12c) In which month of 2008 was the percentage of long delays 
#(i.e., flights with more than 30 minute delays) the highest?
delayPerMonth <- tapply(myDF$DepDelay > 30, myDF$Month, sum, na.rm=T)
delayPerMonth
allFlightPerMonth <- tapply(myDF$Month, myDF$Month, length)
allFlightPerMonth
delayPctPerMonth <- delayPerMonth / allFlightPerMonth * 100
delayPctPerMonth
names(which.max(delayPctPerMonth))

# dplyr
longDelay <- myDF %>% filter(DepDelay > 30) %>% group_by(Month) %>% tally
longDelay <- as.data.frame(longDelay); longDelay
allFlight <- myDF %>% group_by(Month) %>% tally
allFlight <- as.data.frame(allFlight); allFlight
longDelay <- longDelay %>% mutate(LatePercentages = n / allFlight$n * 100) 
which.max(longDelay$LatePercentages)

# 13) Analyzing Flights by Time of Day for Departure
# Break the day into 4 parts:
# early morning (1) correspond to the times to 6 am
# late morning (2) correspond to the times to 6 am to 12 noon
# early evening (3) correspond to the times to 12 noon to 6 pm
# late evening (4) correspond to the times to 6 pm to 12 midnight
v<-ceiling(myDF$DepTime/600)
# dplyr

# build a vector called parts of the day
partsofday <- rep(NA, times=dim(myDF)[1])
partsofday
partsofday[v==1]<-"early morning"
partsofday[v==2]<-"late morning"
partsofday[v==3]<-"early evening"
partsofday[v==4]<-"late evening"
table(partsofday)

# dplyr

# and we can create a new column in the myDF data frame called "timeofday"
# and we can store this information we just found into this column
myDF$timeofday <- partsofday
dim(myDF)

# dplyr
myDF <- myDF %>% mutate(timeofday = partsofday)
dim(myDF)

# just check to make sure that the first 6 flights were done properly
head(myDF$timeofday)
head(myDF$DepTime)

# dplyr
myDF %>% select(timeofday) %>% slice(1:6)
myDF %>% select(DepTime) %>% slice(1:6)

# 13a) How many flights departed from IND early in the morning?
sum(myDF$Origin=='IND' & myDF$timeofday=='early morning', na.rm = TRUE)

# dplyr
myDF %>% filter(Origin=='IND', timeofday=='early morning') %>% nrow

# 13b) Tabulate how many flights occur, by splitting the flights according to
# both the city of origin and also the time of the day when the flight departed
tapply(myDF$DepDelay, list(myDF$Origin, myDF$timeofday), length)

# dplyr
myDF %>% group_by(Origin, timeofday) %>% tally