### Analyzing 2012 London Olympics Data ###
### Author: Asil Cetin ###

# Attach the data
attach(OlympicsData)

# Reorder the data based on BordaPoints
OlympicsData <- OlympicsData[order(-BordaPoints),] 

# Distribution of Borda Points by Population (ln)
plot(`Ln(PopnSize)`, BordaPoints, main="Distribution of Borda Points by Population (ln)", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)