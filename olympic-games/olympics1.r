setwd("D:\\E books\\Machine Learning Resources\\Kaggle\\olympic-games")
events <- read.csv("events.csv")
athletes <- read.csv("athletes.csv")
countries <- read.csv("countries.csv")

athletes$nationality <- as.factor(athletes$nationality)
athletes$sex <- as.factor(athletes$sex)
athletes$sport <- as.factor(athletes$sport)

events$sport <- as.factor(events$sport )
events$discipline <-as.factor(events$discipline)
events$sex <- as.factor(events$sex)

countries$code <- as.factor(countries$code)

getAthletes <- function(arg) { 
	for(val in arg)
	{
		result=list()
		athlete <- athletes[athletes$nationality==val,]
		result <- athlete[athlete$wonGold==1,]
		print(countries$country[countries$code==val])
		cat("\n")
	    print(result$name)
		cat("\n")
	}
}

countries[46,]$population = 21000
countries[46,]$gdp_per_capita = 9100
countries[62,]$population = 6330000
countries[62,]$gdp_per_capita = 544
countries[131,]$population = 227049
countries[131,]$gdp_per_capita = 16000
countries[140,]$population = 4170000
countries[140,]$gdp_per_capita = 880
countries[178,]$population = 23520000
countries[178,]$gdp_per_capita = 31900

countries[4,]$gdp_per_capita = 8000
countries[5,]$gdp_per_capita = 53000
countries[10,]$gdp_per_capita = 28924
countries[21,]$gdp_per_capita = 84381
countries[28,]$gdp_per_capita = 43366
countries[37,]$gdp_per_capita = 47000
countries[50,]$gdp_per_capita = 7657
countries[75,]$gdp_per_capita = 30500
countries[87,]$gdp_per_capita = 5383
countries[97,]$gdp_per_capita = 1000
countries[106,]$gdp_per_capita = 6169
countries[107,]$gdp_per_capita = 143151
countries[117,]$gdp_per_capita = 1243
countries[122,]$gdp_per_capita = 187649
countries[142,]$gdp_per_capita = 2517
countries[148,]$gdp_per_capita = 27399
countries[157,]$gdp_per_capita = 44208
countries[177,]$gdp_per_capita = 2802
countries[196,]$gdp_per_capita = 15102
countries[198,]$gdp_per_capita = 18729


missing_athletes <- athletes[complete.cases(athletes) == FALSE,]
missing_events <- events[complete.cases(events) == FALSE,]
missing_countries <- countries[complete.cases(countries) == FALSE,]

library(ggplot2)



	