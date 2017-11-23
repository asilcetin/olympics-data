### Analyzing 2012 London Olympics Data ###
### Author: Asil Cetin ###

# Attach the data
attach(OlympicsData)
# Reorder the data based on BordaPoints
OlympicsData = OlympicsData[order(-BordaPoints),] 
# Add new variable: Combination of Ln(PopSize)+(Ln(Income)
OlympicsData$CombinedVarPopInc <- `Ln(PopnSize)` + `Ln(Income)`
# Get the top fifty most successful countries into a dataset
TopFiftyCountries = OlympicsData[1:50,]
# Get the top fifty most successful countries into a dataset
BottomFiftyCountries = OlympicsData[order(BordaPoints),]
BottomFiftyCountries = BottomFiftyCountries[1:50,]
# Get countries with at least 1 Borda Point
OBC = OlympicsData[BordaPoints>0,]
# Get countries with at least 5 Borda Points
FBC = OlympicsData[BordaPoints>4,]
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
# Get the summary of all countries income & popsize
AllWorldSum = summary(OlympicsData[,c('Income', 'Popsize')])
# Pakistan's summary
PakistanSum = OlympicsData[Country == "Pakistan",c('BordaPoints', 'Popsize', 'Income')]
row.names(PakistanSum) <- "Pakistan"
# Nigeria's summary
NigeriaSum = OlympicsData[Country == "Nigeria",c('BordaPoints', 'Popsize', 'Income')]
row.names(NigeriaSum) <- "Nigeria"
# Monaco's summary
MonacoSum = OlympicsData[Country == "Monaco",c('BordaPoints', 'Popsize', 'Income')]
row.names(MonacoSum) <- "Monaco"
# Liechtenstein's summary
LiechtensteinSum = OlympicsData[Country == "Liechtenstein",c('BordaPoints', 'Popsize', 'Income')]
row.names(LiechtensteinSum) <- "Liechtenstein"
# Ethiopia's summary
EthiopiaSum = OlympicsData[Country == "Ethiopia",c('BordaPoints', 'Popsize', 'Income')]
row.names(EthiopiaSum) <- "Ethiopia"
# Ethiopia's summary
JamaicaSum = OlympicsData[Country == "Jamaica",c('BordaPoints', 'Popsize', 'Income')]
row.names(JamaicaSum) <- "Jamaica"

# Distribution of Borda Points by Population (ln)
plot(`Ln(PopnSize)`, BordaPoints, main="Distribution of Borda Points by Population (ln)", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
text(`Ln(PopnSize)`, BordaPoints, labels = Country, pos = 4, cex=0.25)
abline(lm(BordaPoints ~ `Ln(PopnSize)`), col = 4)
# Distribution of Borda Points by Population (ln) of Countries at least 1 Borda Point
plot(OBC$`Ln(PopnSize)`, OBC$BordaPoints, main="Distribution of Borda Points by Population (ln) of Countries at least 1 Borda Point", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
text(OBC$`Ln(PopnSize)`, OBC$BordaPoints, labels = OBC$Country, pos = 4, cex=0.25)
abline(lm(OBC$BordaPoints ~ OBC$`Ln(PopnSize)`), col = 4)
# Distribution of Borda Points by Population (ln) of Countries at least 5 Borda Point
plot(FBC$`Ln(PopnSize)`, FBC$BordaPoints, main="Distribution of Borda Points by Population (ln) of Countries at least 5 Borda Point", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
text(FBC$`Ln(PopnSize)`, FBC$BordaPoints, labels = FBC$Country, pos = 4, cex=0.25)
abline(lm(FBC$BordaPoints ~ FBC$`Ln(PopnSize)`), col = 4)
# Distribution of Borda Points by Income (ln)
plot(`Ln(Income)`, BordaPoints, main="Distribution of Borda Points by Income (ln)", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
text(`Ln(Income)`, BordaPoints, labels = Country, pos = 4, cex=0.25)
abline(lm(BordaPoints ~ `Ln(Income)`), col = 4)
# Distribution of Borda Points by Income (ln) of Countries at least 1 Borda Point
plot(OBC$`Ln(Income)`, OBC$BordaPoints, main="Distribution of Borda Points by Income (ln) of Countries at least 1 Borda Point", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
text(OBC$`Ln(Income)`, OBC$BordaPoints, labels = OBC$Country, pos = 4, cex=0.25)
abline(lm(OBC$BordaPoints ~ OBC$`Ln(Income)`), col = 4)
# Distribution of Borda Points by Income (ln) of Countries at least 5 Borda Point
plot(FBC$`Ln(Income)`, FBC$BordaPoints, main="Distribution of Borda Points by Income (ln) of Countries at least 5 Borda Point", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)
text(FBC$`Ln(Income)`, FBC$BordaPoints, labels = FBC$Country, pos = 4, cex=0.25)
abline(lm(FBC$BordaPoints ~ FBC$`Ln(Income)`), col = 4)
# Correlations
cor.test(`Ln(Income)`, BordaPoints)
cor.test(`Ln(PopnSize)`, BordaPoints)
cor.test(FBC$`Ln(Income)`, FBC$BordaPoints)
cor.test(FBC$`Ln(PopnSize)`, FBC$BordaPoints)
# Multiple Linear Regression Model for Raw Data
model1 <- lm(BordaPoints ~ Income + Popsize + Income:Popsize)
summary(model1)
plot(model1, which=c(2))
# Confidence Intervals for the Model Coefficents
confint(model1, conf.level=0.95)
# Multiple Linear Regression Model for Raw Data on LN
model2 <- lm(BordaPoints ~ `Ln(Income)` + `Ln(PopnSize)` + `Ln(Income)`:`Ln(PopnSize)`)
summary(model2)
plot(model2, which=c(2))
# Confidence Intervals for the Model Coefficents
confint(model2, conf.level=0.95)
# Multiple Linear Regression Model on > 5 Borda-Point-Countries
model3 <- lm(FBC$BordaPoints ~ FBC$Income + FBC$Popsize + FBC$Income:FBC$Popsize)
summary(model3)
plot(model3, which=c(2))
# Confidence Intervals for the Model Coefficents
confint(model3, conf.level=0.95)
# Multiple Linear Regression Model on > 5 Borda-Point-Countries on LN
model4 <- lm(FBC$BordaPoints ~ FBC$`Ln(Income)` + FBC$`Ln(PopnSize)`)
summary(model4)
plot(model4, which=c(2))
# Confidence Intervals for the Model Coefficents
confint(model4, conf.level=0.95)