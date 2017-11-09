### Analyzing 2012 London Olympics Data ###
### Author: Asil Cetin ###

# Attach the data
attach(OlympicsData)
# Reorder the data based on BordaPoints
OlympicsData = OlympicsData[order(-BordaPoints),] 
# Get the top fifty most successful countries into a dataset
TopFiftyCountries = OlympicsData[1:50,]
# Get the top fifty most successful countries into a dataset
BottomFiftyCountries = OlympicsData[order(BordaPoints),]
BottomFiftyCountries = BottomFiftyCountries[1:50,]
# Get countries with at least 1 Borda Point
OneBordaCountries = OlympicsData[BordaPoints>0,]
# Get countries with at least 5 Borda Points
FiveBordaCountries = OlympicsData[BordaPoints>4,]
# Get the summary of all countries popsize
AllPopsizeSum = summary(OlympicsData[,c('Popsize')])
# Get the summary of top 50 countries popsize
TopFiftyPopsizeSum = summary(TopFiftyCountries[,c('Popsize')])
# Get the summary of bottom 50 countries popsize
BottomFiftyPopsizeSum = summary(BottomFiftyCountries[,c('Popsize')])
# Get the summary of all countries income
AllIncomeSum = summary(OlympicsData[,c('Income')])
# Get the summary of top 50 countries income
TopFiftyIncomeSum = summary(TopFiftyCountries[,c('Income')])
# Get the summary of bottom 50 countries income
BottomFiftyIncomeSum = summary(BottomFiftyCountries[,c('Income')])
# Nigeria's summary
NigeriaSum = OlympicsData[Country == "Nigeria",c('BordaPoints', 'Popsize', 'Income')]
row.names(NigeriaSum) <- "Nigeria"

# Combination of Ln(PopSize)+(Ln(Income)
OlympicsData$CombineParsPopInc <- `Ln(PopnSize)` + `Ln(Income)`

# Distribution of Borda Points by Population (ln)
plot(`Ln(PopnSize)`, BordaPoints, main="Distribution of Borda Points by Population (ln)", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
abline(lm(BordaPoints ~ `Ln(PopnSize)`), col = 4)
# Distribution of Borda Points by Population (ln) of Countries at least 1 Borda Point
plot(OneBordaCountries$`Ln(PopnSize)`, OneBordaCountries$BordaPoints, main="Distribution of Borda Points by Population (ln) of Countries at least 1 Borda Point", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
abline(lm(OneBordaCountries$BordaPoints ~ OneBordaCountries$`Ln(PopnSize)`), col = 4)
# Distribution of Borda Points by Population (ln) of Countries at least 5 Borda Point
plot(FiveBordaCountries$`Ln(PopnSize)`, FiveBordaCountries$BordaPoints, main="Distribution of Borda Points by Population (ln) of Countries at least 5 Borda Point", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
abline(lm(FiveBordaCountries$BordaPoints ~ FiveBordaCountries$`Ln(PopnSize)`), col = 4)
# Distribution of Borda Points by Income (ln)
plot(`Ln(Income)`, BordaPoints, main="Distribution of Borda Points by Income (ln)", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
abline(lm(BordaPoints ~ `Ln(Income)`), col = 4)
# Distribution of Borda Points by Income (ln) of Countries at least 1 Borda Point
plot(OneBordaCountries$`Ln(Income)`, OneBordaCountries$BordaPoints, main="Distribution of Borda Points by Income (ln) of Countries at least 1 Borda Point", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
abline(lm(OneBordaCountries$BordaPoints ~ OneBordaCountries$`Ln(Income)`), col = 4)
# Distribution of Borda Points by Income (ln) of Countries at least 5 Borda Point
plot(FiveBordaCountries$`Ln(Income)`, FiveBordaCountries$BordaPoints, main="Distribution of Borda Points by Income (ln) of Countries at least 5 Borda Point", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
abline(lm(FiveBordaCountries$BordaPoints ~ FiveBordaCountries$`Ln(Income)`), col = 4)

