#################################### WQD7004: ASSIGNMENT 2 #######################################

# Name : NG KANG WEI
# Student ID : WQD170068

path <- file.path("E:\\academic\\mds-sem2\\WQD7004-Rprogramming\\2008.csv")
myDF <- read.csv(path)

# 1) Sort in decreasing order the ten most popular airports according to the number of origins of flights
# # (hint : use decreasing=T and indexing )
flightOrigin <- table(myDF$Origin)
sort(flightOrigin, decreasing=TRUE)[1:10]

# 2) Assign the names of the ten most popular airports according to the number of origins of flights to 
## variable called mostPopularOrg
# # (hint : use names() and indexing)
mostPopularOrg <- names(sort(flightOrigin, decreasing=TRUE))[1:10]
mostPopularOrg

# 3) Assign the names of the ten most popular airports according to the number of destinations of flights to 
## variable called mostPopularDes
flightDestination <- table(myDF$Dest)
mostPopularDest <- names(sort(flightDestination, decreasing=TRUE))[1:10]
mostPopularDest

# 4) How many flights had their origin in one of these 10 most popular airports
## (hint : use %in%)
sum(myDF$Origin %in% mostPopularOrg)

# 5)How many flights had their destinationn in one of these 10 most popular airports
## (hint : use %in%)
sum(myDF$Dest %in% mostPopularDest)

# 6) Find flights for which the origin and the destination
# were among the 10 most popular airports
## (hint : use %in%)
sum(myDF$Origin %in% mostPopularOrg & myDF$Dest %in% mostPopularDest)

# 7) For the purposes of this question, treat the group 
# of the 200 least popular airports according to the 
# number of flights having these as the origins.
# How many flights had one of these 200 least popular 
# airports as their origin?
leastPopularOrg <- names(sort(flightOrigin)[1:200])
leastPopularOrg
sum(myDF$Origin %in% leastPopularOrg)

# 8) Index a vector according to the names of the elements in the vector
##8a) How many flights departed from "IND" ?
flightOrigin['IND']

##8b) How many flights departed from "IND","ORD","JFK","EWR","IAD" ?
sum(flightOrigin[c('IND', 'ORD', 'JFK', 'EWR', 'IAD')])

# org <- c('IND', 'ORD', 'JFK', 'EWR', 'IAD')
# sum(myDF$Origin %in% org, na.rm = TRUE)

##8c) How many flights departed from 10 most popular airports ?
sum(flightOrigin[mostPopularOrg])

##8d) How many flights departed from 10 least popular airports ?
sum(flightOrigin[leastPopularOrg[1:10]])

# 8e) How many flights landed at Ronald Reagan Washington
## National ("DCA") or Washington Dulles Airport ("IAD") in 2008? 
sum(flightDestination[c('DCA', 'IAD')])

# 9)Check the first 20 flights and see which one departed on time or early
first20 <- head(myDF, 20)
first20DepOnTimeEarly <- subset(first20, first20$DepDelay <= 0); first20DepOnTimeEarly
nrow(first20DepOnTimeEarly)

##9a) Restrict attention to only the 10 most popular airports 
##and see which one departed on time or early
mostPop <- subset(myDF, myDF$Origin %in% mostPopularOrg)
mostPopDepOnTimeEarly <- subset(mostPop, mostPop$DepDelay <= 0); head(mostPopDepOnTimeEarly)
nrow(mostPopDepOnTimeEarly)

##9b)Find the percentage of flights at each of the 10 most popular 
# airports that departed on time or early

flight_percentage <- function(count, total) {
  pct <- count / total * 100
  return(round(pct, digits = 2))
}

popAirportDelayPct <- c()
for (org in mostPopularOrg) {
  noDelay <- sum(myDF$Origin == org & myDF$DepDelay <= 0, na.rm = TRUE)
  popAirportDelayPct[org] <- flight_percentage(noDelay, nrow(myDF))
}
popAirportDelayPct

# 9c) What percentage of flights departed from IND on time or early?
flight_percentage(sum(myDF$Origin=='IND' & myDF$DepDelay <=0, na.rm=TRUE), nrow(myDF))

#10) Analyze Flights by Origin Airport and Month of Departure
##10a) Break the data in the DepDelay vector according to which city of origin 
depOrg <- tapply(myDF$DepDelay, myDF$Origin, mean, na.rm=TRUE)
head(depOrg)

##10b) Break the data in the DepDelay vector according to month
depMonth <- tapply(myDF$DepDelay, myDF$Month, mean, na.rm=TRUE)
head(depMonth)

#11) How many flights delay occur from each airport in each month ?

# uniqueAirport <- unique(myDF$Origin)
# length(uniqueAirport)
# 
# airportFlightDelay <- array(c(), dim=c(2,12,303))
# for (airport in unique(myDF$Origin)) {
#  # airportFlightDelay[airport] <- 
#    tapply(myDF$Month[myDF$DepDelay>0 & myDF$Origin==airport],
#         myDF$Month[myDF$DepDelay>0 & myDF$Origin==airport], length)
# }
# 
# length(airportFlightDelay)
# test <-array()
# test[,,'IAD'] <- tapply(myDF$Month[myDF$DepDelay>0 & myDF$Origin=='IAD'],
#        myDF$Month[myDF$DepDelay>0 & myDF$Origin=='IAD'], length)

##11a) Extract the data from origin airport = "IND"
# and from the month of June
juneInd <- subset(myDF, myDF$Origin=='IND' & myDF$Month==6)
# juneInd <- subset(myDF, myDF$Origin=='IND' & myDF$Month==6 & myDF$DepDelay>0)
nrow(juneInd)

# tapply(myDF$Month[myDF$DepDelay>0 & myDF$Origin=='IND'],
#        myDF$Month[myDF$DepDelay>0 & myDF$Origin=='IND'], length)[6]

tapply(myDF$Month[myDF$Origin=='IND'],
       myDF$Month[myDF$Origin=='IND'], length)[6]

##11b) Extract the data from origin airport = "ATL"
# and from the month of March
marchAtl <- subset(myDF, myDF$Origin == 'ATL' & myDF$Month == 3)
# marchAtl <- subset(myDF, myDF$Origin == 'ATL' & myDF$Month == 3 & myDF$DepDelay > 0)
nrow(marchAtl)

tapply(myDF$Month[myDF$Origin=='ATL'],
       myDF$Month[myDF$Origin=='ATL'], length)[3]

# tapply(myDF$Month[myDF$DepDelay>0 & myDF$Origin=='ATL'],
#        myDF$Month[myDF$DepDelay>0 & myDF$Origin=='ATL'], length)[3]

# 11c) The number of flights delay from 3 airports = "ATL","AUS","BDL"
# during the months of July through October
flightDelay3airport <- tapply(myDF$Month[myDF$DepDelay>0 & myDF$Origin %in% c('ATL','AUS','BDL')],
                              myDF$Month[myDF$DepDelay>0 & myDF$Origin %in% c('ATL','AUS','BDL')], length)[7:10]

flightDelay3airport

# 11d) How many flights departed altogether from ATL, AUS, and BDL during the months of 
#July 2008 through October 2008?
sum(flightDelay3airport)

# 11e) All the flight delays, month by month, frm IND airport
tapply(myDF$Month[myDF$DepDelay>0 & myDF$Origin=='IND'],
       myDF$Month[myDF$DepDelay>0 & myDF$Origin=='IND'], length)

# 11f) All the flight delays, month by month, frm both IND and ORD at once
tapply(myDF$Month[myDF$DepDelay>0 & myDF$Origin %in% c('IND','ORD')],
       myDF$Month[myDF$DepDelay>0 & myDF$Origin %in% c('IND','ORD')], length)

# 12) Calculating Percentages of Flights with delayed more than 30 minutes when departing
moreThan30min <- flight_percentage(sum(myDF$DepDelay > 30, na.rm = TRUE), nrow(myDF))
moreThan30min

# 12a) find the percentage of flights with long delays and plot with dotchart()
dotchart(moreThan30min)

# 12b) How many flights departed altogether from IND 
# or ORD in 2008 with a delay of more than 30 minutes each?
sum(myDF$Origin %in% c('IND', 'ORD') & myDF$DepDelay > 30, na.rm = TRUE)

#12c) In which month of 2008 was the percentage of long delays 
#(i.e., flights with more than 30 minute delays) the highest?
delayPerMonth <- tapply(myDF$Month[myDF$DepDelay > 30], myDF$Month[myDF$DepDelay > 30], length)
delayPerMonth

monthDelayPct <- c()
for (month in as.integer(names(delayPerMonth))) {
  monthDelayPct[as.character(month)] <- flight_percentage(delayPerMonth[month], sum(myDF$Month==month))
}
monthDelayPct
names(which(monthDelayPct == max(monthDelayPct)))

# 13) Analyzing Flights by Time of Day for Departure
# Break the day into 4 parts:
# early morning (1) correspond to the times to 6 am
# late morning (2) correspond to the times to 6 am to 12 noon
# early evening (3) correspond to the times to 12 noon to 6 pm
# late evening (4) correspond to the times to 6 pm to 12 midnight
v<-ceiling(myDF$DepTime/600)
# build a vector called parts of the day
partsofday <- rep(NA, times=dim(myDF)[1])
partsofday
partsofday[v==1]<-"early morning"
partsofday[v==2]<-"late morning"
partsofday[v==3]<-"early evening"
partsofday[v==4]<-"late evening"
table(partsofday)
# and we can create a new column in the myDF data frame called "timeofday"
# and we can store this information we just found into this column
myDF$timeofday <- partsofday
dim(myDF)
# just check to make sure that the first 6 flights were done properly
head(myDF$timeofday)
head(myDF$DepTime)
# 13a) How many flights departed from IND early in the morning?
sum(myDF$Origin=='IND' & myDF$timeofday=='early morning', na.rm = TRUE)

# 13b) Tabulate how many flights occur, by splitting the flights according to
# both the city of origin and also the time of the day when the flight departed
orgTimePair <- paste(myDF$Origin, "in", myDF$timeofday)
table(orgTimePair)