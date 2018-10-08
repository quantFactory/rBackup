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
popularOrg <- myDF %>% group_by(Origin) %>% summarize(origin_count=n()) %>% arrange(desc(origin_count))
popularOrg[1:10,]

# 2) Assign the names of the ten most popular airports according to the number of origins of flights to 
## variable called mostPopularOrg
# # (hint : use names() and indexing)
mostPopularOrg <- names(sort(flightOrigin, decreasing=TRUE))[1:10]
mostPopularOrg

# dplyr
namesOfPopOrg <- popularOrg %>% select(Origin) %>% slice(1:10)
namesOfPopOrg


# 3) Assign the names of the ten most popular airports according to the number of destinations of flights to 
## variable called mostPopularDes
flightDestination <- table(myDF$Dest)
mostPopularDes <- names(sort(flightDestination, decreasing=TRUE))[1:10]
mostPopularDes

# dplyr
mostPopularDest <- myDF %>% group_by(Dest) %>% summarize(dest_count=n()) %>% arrange(desc(dest_count)) %>% 
                  select(Dest) %>% slice(1:10)
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
leastPopularAirport <- myDF %>% group_by(Origin) %>% summarize(origin_count=n()) %>% 
                  arrange(origin_count) %>% select(Origin) %>% slice(1:200)
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
myDF %>% group_by(Origin) %>% nrow

##8c) How many flights departed from 10 most popular airports ?
flightOrigin[mostPopularOrg]
# dplyr

##8d) How many flights departed from 10 least popular airports ?
flightOrigin[names(leastPopularOrg[1:10])]
# dplyr

# 8e) How many flights landed at Ronald Reagan Washington
## National ("DCA") or Washington Dulles Airport ("IAD") in 2008? 
flightDestination[c('DCA', 'IAD')]
# dplyr

# 9)Check the first 20 flights and see which one departed on time or early
first20 <- head(myDF, 20)
first20DepOnTimeEarly <- subset(first20, first20$DepDelay <= 0); first20DepOnTimeEarly
nrow(first20DepOnTimeEarly)
# dplyr

##9a) Restrict attention to only the 10 most popular airports 
##and see which one departed on time or early
mostPopDepOnTimeEarly <- tapply(myDF$DepDelay <= 0, myDF$Origin, sum, na.rm=TRUE)[mostPopularOrg]
mostPopDepOnTimeEarly
# dplyr

##9b)Find the percentage of flights at each of the 10 most popular 
# airports that departed on time or early
mostPopDepOnTimeEarly / flightOrigin[mostPopularOrg]  * 100

# 9c) What percentage of flights departed from IND on time or early?
sum(myDF$Origin=='IND' & myDF$DepDelay <= 0, na.rm=TRUE) / flightOrigin['IND'] * 100
# dplyr

#10) Analyze Flights by Origin Airport and Month of Departure
##10a) Break the data in the DepDelay vector according to which city of origin 
depOrg <- tapply(myDF$DepDelay, myDF$Origin, length)
head(depOrg)
# dplyr

##10b) Break the data in the DepDelay vector according to month
depMonth <- tapply(myDF$DepDelay, myDF$Month, length)
head(depMonth)
# dplyr

#11) How many flights delay occur from each airport in each month ?
tapply(myDF$DepDelay[myDF$DepDelay > 0], 
       list(myDF$Origin[myDF$DepDelay > 0], myDF$Month[myDF$DepDelay > 0]), length)
# dplyr

##11a) Extract the data from origin airport = "IND"
# and from the month of June
tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)['IND', 6]
# dplyr

##11b) Extract the data from origin airport = "ATL"
# and from the month of March
tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)['ATL',3]
# dplyr

# 11c) The number of flights delay from 3 airports = "ATL","AUS","BDL"
# during the months of July through October
flightDelay3airport <- tapply(myDF$DepDelay[myDF$DepDelay > 0], list(myDF$Origin[myDF$DepDelay > 0], myDF$Month[myDF$DepDelay > 0]), length)[c('ATL', 'AUS', 'BDL'), 7:10]
flightDelay3airport
# dplyr

# 11d) How many flights departed altogether from ATL, AUS, and BDL during the months of 
#July 2008 through October 2008?
sum(flightDelay3airport)
colSums(flightDelay3airport)
rowSums(flightDelay3airport)
# dplyr

# 11e) All the flight delays, month by month, frm IND airport
tapply(myDF$DepDelay[myDF$DepDelay > 0], 
       list(myDF$Origin[myDF$DepDelay > 0], myDF$Month[myDF$DepDelay > 0]), length)['IND', ]
# tapply(myDF$Month[myDF$DepDelay>0 & myDF$Origin=='IND'],
#        myDF$Month[myDF$DepDelay>0 & myDF$Origin=='IND'], length)

# dplyr

# 11f) All the flight delays, month by month, frm both IND and ORD at once
allFlightDelay <- tapply(myDF$DepDelay[myDF$DepDelay > 0], 
       list(myDF$Origin[myDF$DepDelay > 0], myDF$Month[myDF$DepDelay > 0]), length)[c('IND', 'ORD'), ]
allFlightDelay

tapply(myDF$DepDelay, 
       list(myDF$Origin, myDF$Month), length)[c('IND', 'ORD'), ]

# dplyr

# 12) Calculating Percentages of Flights with delayed more than 30 minutes when departing
moreThan30min <- subset(myDF, myDF$DepDelay > 30)
delayedFlight <- tapply(moreThan30min$DepDelay, list(moreThan30min$Origin, moreThan30min$Month), length)[c('IND', 'ORD'),]
allFlight <- tapply(myDF$DepDelay, list(myDF$Origin, myDF$Month), length)[c('IND', 'ORD'),]
pct <- delayedFlight / allFlight * 100
pct

# dplyr

# 12a) find the percentage of flights with long delays and plot with dotchart()
dotchart(pct)

# dplyr

# 12b) How many flights departed altogether from IND 
# or ORD in 2008 with a delay of more than 30 minutes each?
sum(tapply(myDF$DepDelay[myDF$DepDelay>30], myDF$Origin[myDF$DepDelay>30], length)[c('IND', 'ORD')])
sum(delayedFlight)
# dplyr

#12c) In which month of 2008 was the percentage of long delays 
#(i.e., flights with more than 30 minute delays) the highest?
delayPerMonth <- tapply(myDF$Month[myDF$DepDelay > 30], myDF$Month[myDF$DepDelay > 30], length)
delayPerMonth
allFlightPerMonth <- tapply(myDF$Month, myDF$Month, length)
allFlightPerMonth
delayPctPerMonth <-   delayPerMonth / allFlightPerMonth * 100
delayPctPerMonth
names(which.max(delayPctPerMonth))

which.max(colSums(delayedFlight/allFlight))

monthDelayPct <- c()
for (month in as.integer(names(delayPerMonth))) {
  monthDelayPct[as.character(month)] <- delayPerMonth[month] /  sum(myDF$Month==month) * 100
}
monthDelayPct
names(which(monthDelayPct == max(monthDelayPct)))

# dplyr

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

# just check to make sure that the first 6 flights were done properly
head(myDF$timeofday)
head(myDF$DepTime)
# dplyr

# 13a) How many flights departed from IND early in the morning?
sum(myDF$Origin=='IND' & myDF$timeofday=='early morning', na.rm = TRUE)

# dplyr

# 13b) Tabulate how many flights occur, by splitting the flights according to
# both the city of origin and also the time of the day when the flight departed
tapply(myDF$DepDelay, list(myDF$Origin, myDF$timeofday), length)
# dplyr
