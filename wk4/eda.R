range(stormData$BGN_DATE)
propev <- round(sort(prop.table(table(stormData$EVTYPE))),2)


# Occurances of type over time
annualEvtype <- aggregate(as.character(commonData$EVTYPE), by = list(year(commonData$BGN_DATE)), FUN = count)
mean(stormData$INJURIES)
hist(stormData$INJURIES[stormData$INJURIES > 0 & stormData$INJURIES <= 10])

commonEvTable <- as.data.frame(table(year(commonData$BGN_DATE),as.character(commonData$EVTYPE)))

qplot(Var1,Freq, data = commonEvTable, geom = "line") + aes(group = Var2, color = Var2)

fatalityYrSum <- aggregate(stormData$FATALITIES,by = list(year(stormData$BGN_DATE)), FUN = sum)
names(fatalityYrSum) <- c("year","fatalities")
injuryYrSum <- aggregate(stormData$INJURIES,by = list(year(stormData$BGN_DATE)), FUN = sum)
names(injuryYrSum) <- c("year","injuries")
harmYrSum <- merge(fatalityYrSum,injuryYrSum, by = "year")
meltedHarmYrSum <- melt(harmYrSum, id.vars = "year")

qplot(year,value, data = meltedHarmYrSum, geom = "line") + aes(group = variable, color = variable)

injByYrByEv <- aggregate(commonData$INJURIES,by = list(year(commonData$BGN_DATE), commonData$EVTYPE), FUN = sum)
names(injByYrByEv) <- c("year","evtype","injuries")
qplot(year,injuries, data = injByYrByEv, geom = "line") + aes(group = evtype, color = evtype)


fatByYrByEv <- aggregate(commonData$FATALITIES,by = list(year(commonData$BGN_DATE), commonData$EVTYPE), FUN = sum)
names(fatByYrByEv) <- c("year","evtype","fatalities")
qplot(year,fatalities, data = fatByYrByEv, geom = "line") + aes(group = evtype, color = evtype)


harmByYrByEv <- aggregate(commonData$HARMED,by = list(year(commonData$BGN_DATE), commonData$EVTYPE), FUN = sum)
names(harmByYrByEv) <- c("year","evtype","harmed")
qplot(year,harmed, data = harmByYrByEv, geom = "line") + aes(group = evtype, color = evtype)

# Although less populous, tornados injur and kill vastly more people

# Aggregate for multiple box plots of injuries and fatalities (faceted) by evtype for all years

ggplot(injuryYrSum, aes(EVTYPE, Injurie)) + geom_boxplot()

  aggregate(stormData$INJURIES,by = list(year(stormData$BGN_DATE)), FUN = sum)



summary(stormData$INJURIES[stormData$EVTYPE == "TORNADO"])
#mostInjuring <- stormData[order(stormData$INJURIES,decreasing = TRUE),]

# Only caring about common evcats across all years
injuryYrSum <- commonDT[,j = list("Fatalities" = sum(FATALITIES), "Injuries" = sum(INJURIES)),by = list(EVCAT)]
injuryYrSumMelt <- melt(injuryYrSum, id.vars = c("EVCAT"))
ggplot(injuryYrSumMelt, aes(EVCAT, value), main = "Human harm of storms over all years") + geom_bar(aes(fill = variable), position = "dodge", stat = "identity")

# harm by year and category
injuryByYrSum <- data.frame(commonDT[,j = list("Injuries" = sum(INJURIES)),
                        by = list(DECADE,EVCAT)])
#injuryByYrSumMelt <- reshape2::melt(injuryByYrSum, id.vars = c("DECADE","EVCAT"))
#ggplot(injuryByYrSumMelt, aes(EVCAT, value), main = "Human harm of storms over all years") + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + facet_wrap(~DECADE)
ggplot(injuryByYrSum, aes(EVCAT, Injuries)) + geom_bar(aes(fill = EVCAT), position = "dodge", stat = "identity") + facet_wrap(~DECADE)
