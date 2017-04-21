library(lubridate)
library(highcharter)
library(jsonlite)
getContent <- function(url) {
  library(httr)
  content(GET(url))
}
library(dplyr)
library(purrr)
library(ggplot2)
library(data.table)

#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","data/ocean-data-raw.csv.bz2")
stormData <- read.csv("data/ocean-data-raw.csv.bz2", header = TRUE, stringsAsFactors = FALSE, sep = "," )
# stormData <- read.csv("data/ocean-data-raw.csv.bz2", header = TRUE, stringsAsFactors = FALSE,
#       sep = ",", colClasses = c("STATE__" = "factor", "BGN_DATE" = "date", "BGN_TIME" = "character",
#       "TIME_ZONE" = "factor", "COUNTY" = "factor", "COUNTYNAME" = "factor", "STATE" = "factor", "EVTYPE" = "factor",
#       "BGN_RANGE" = "numeric", "BGN_AZI" = "factor", "BGN_LOCATI" = "factor", "END_DATE" = "date",
#       "END_TIME" = "character", "COUNTY_END" = "factor", "COUNTYENDN" = "logical",
#       "END_RANGE" = "numeric", "END_AZI" = "factor", "END_LOCATI" = "character", "LENGTH" = "numeric",
#       "WIDTH" = "numeric", "F" = "factor", "MAG" = "numeric", "FATALITIES" = "numeric",
#       "INJURIES" = "numeric", "PROPDMG" = "numeric", "PROPDMGEXP" = "factor", "CROPDMG" = "numeric",
#       "CROPDMGEXP" = "factor", "WFO" = "character", "STATEOFFIC" = "character", "ZONENAMES" = "character",
#       "LATITUDE" = "numeric", "LONGITUDE" = "numeric",  "LATITUDE_E" = "numeric", "LONGITUDE_" = "numeric",
#       "REMARKS" = "characteer", "REFNUM" = "numeric"))
# Because apparentlly colClasses didn't stick -_-
stormData$STATE__ <- as.factor(stormData$STATE__)
stormData$BGN_DATE <- as.Date(stormData$BGN_DATE, format = "%m/%d/%Y")
stormData$BGN_TIME <- hms(paste0(substring(stormData$BGN_TIME,1,2),":", substring(stormData$BGN_TIME,3,4),":00"))
stormData$BGN_TIMESTAMP <- stormData$BGN_DATE + stormData$BGN_TIME
stormData$DECADE <- (year(stormData$BGN_DATE) %/% 10) * 10
stormData$TIME_ZONE <- as.factor(stormData$TIME_ZONE)
stormData$COUNTY <- as.factor(stormData$COUNTY)
stormData$COUNTYNAME <- as.factor(stormData$COUNTYNAME)
stormData$STATE <- as.factor(stormData$STATE )
stormData$EVTYPE <- as.factor(toupper(stormData$EVTYPE))
stormData$BGN_AZI <- as.factor(stormData$BGN_AZI)
stormData$BGN_LOCATI <- as.factor(stormData$BGN_LOCATI)
stormData$END_DATE <- as.Date(stormData$END_DATE, format = "%m/%d/%Y")
stormData$END_TIME <- hms(paste0(substring(stormData$END_TIME,1,2),":", substring(stormData$END_TIME,3,4),":00"))
stormData$END_TIMESTAMP <- stormData$END_DATE + stormData$END_TIME
stormData$COUNTY_END <- as.factor(stormData$COUNTY_END)
stormData$END_AZI <- as.factor(stormData$END_AZI)
stormData$F <- as.factor(stormData$F)
stormData$FATALITIES <- as.integer(stormData$FATALITIES)
stormData$INJURIES <- as.integer(stormData$INJURIES)
stormData$PROPDMGEXP <- as.factor(stormData$PROPDMGEXP)
stormData$CROPDMGEXP <- as.factor(stormData$CROPDMGEXP)
stormData$LATITUDE <- as.numeric(stormData$LATITUDE)
stormData$LONGITUDE <- as.numeric(stormData$LONGITUDE)
stormData$LATITUDE_E <- as.numeric(stormData$LATITUDE_E)
stormData$LONGITUDE_ <- as.numeric(stormData$LONGITUDE_)
stormData$HARMED <- stormData$INJURIES + stormData$FATALITIES

# Removing nonsense records
stormData <- stormData[!grepl("SUMMARY", stormData$EVTYPE),]
stormData <- stormData[stormData$EVTYPE != "NONE", ]
stormData <- stormData[stormData$EVTYPE != "NO SEVERE WEATHER",]


# Extra cleaning
# Lots of mixes of "Heat" for evtypes. Basically all the same thing though. Some others can be generalized
evCat <- trimws(as.character(stormData$EVTYPE))
uniqueEv[grep("TSTM",uniqueEv)]
evCat[grep("HURRICANE", evCat)] <- "HURRICANE"
evCat[grep("HAIL", evCat)] <- "HAIL"
evCat[grep("VOLCAN", evCat)] <- "VOLCANO"
evCat[grep("HEAT", evCat)] <- "HEAT"
evCat[grep("SNOW", evCat)] <- "SNOW"
evCat[grep("TORNADO", evCat)] <- "TORNADO"
evCat[grep("SNOW", evCat)] <- "SNOW"
evCat[grep("FLOOD", evCat)] <- "FLOOD"
evCat[grep("THUNDER", evCat)] <- "THUNDERSTORM"
evCat[grep("LIGHTNING", evCat)] <- "THUNDERSTORM"
evCat[grep("WIND", evCat)] <- "WIND"




# Odd/boring events or mixed up ones
evCat[grep("HOT", evCat)] <- "HEAT"
evCat[grep("COLD", evCat)] <- "COLD"
evCat[grep("COOL", evCat)] <- "COLD"
evCat[grep("DRY", evCat)] <- "DRY"
evCat[grep("DRI", evCat)] <- "DRY"
evCat[grep("DROUGHT", evCat)] <- "DRY"


evCat[grep("WET", evCat)] <- "WET"
evCat[grep("WARM", evCat)] <- "HEAT"
evCat[grep("HIGH TEMPERATURE", evCat)] <- "HEAT"

evCat[grep("RAIN", evCat)] <- "RAIN"
evCat[grep("MIX", evCat)] <- "MIXED"
evCat[grep("ICE", evCat)] <- "ICE"
evCat[grep("HIGH", evCat)] <- "HIGH WATER"
evCat[grep("HAIL", evCat)] <- "HAIL"
evCat[grep("FREEZ", evCat)] <- "FREEZE"
evCat[grep("BLIZZARD", evCat)] <- "BLIZZARD"
evCat[grep("FIRE", evCat)] <- "FIRE"
evCat[grep("SURGE", evCat)] <- "HIGH WATER"
evCat[grep("FUNNEL", evCat)] <- "FUNNEL"
evCat[grep("SHOWER", evCat)] <- "RAIN"
evCat[grep("TIDE", evCat)] <- "HIGH WATER"
evCat[grep("LAND", evCat)] <- "LAND"
evCat[grep("MUD", evCat)] <- "LAND"
evCat[grep("ROCK", evCat)] <- "LAND"
evCat[grep("WINTER", evCat)] <- "WINTER"
evCat[grep("WATER", evCat)] <- "WATER"
evCat[grep("ICE", evCat)] <- "ICE"
evCat[grep("ICY", evCat)] <- "ICE"
evCat[grep("PRECIPITATION", evCat)] <- "RAIN"
evCat[grep("TROPICAL", evCat)] <- "TROPICAL"
evCat[grep("FROST", evCat)] <- "FREEZE"
evCat[grep("FLOOOD", evCat)] <- "FLOOD"


uniqueEv <- sort(unique(evCat))
table(evCat)[order(names(table(evCat)))]

stormData$EVCAT <- evCat

stormDT <- data.table(stormData)

# Pull out only the most common events
thresh <- .05
evtypes <- unique(stormData$EVCAT)
propev <- round(sort(prop.table(table(stormData$EVCAT))),2)
commonPropEv <- propev[propev >= thresh]
commonData <- stormData[stormData$EVCAT %in% names(commonPropEv),]
commonDT <- data.table(commonData)

