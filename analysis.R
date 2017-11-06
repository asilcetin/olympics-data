### Analyzing 2012 London Olympics Data ###
### by Asil Cetin ###

# First we save our working directory path
projectWD <- "/Users/asilcetin/Google\ Drive/Development/localhost/uni/bi2/olympics-data"

# Set the working directory
setwd(projectWD)

# Install readxl, uncomment the following line if not installed already
# install.packages("readxl")

# Load readxl
library("readxl")

# Load the data
OlympicsData <- read_excel("OlympicsData.xlsx")

# Attach the data
attach(OlympicsData)

# Reorder the data based on BordaPoints
OlympicsData <- OlympicsData[order(-BordaPoints),] 

# Distribution of Borda Points by Population (ln)
plot(`Ln(PopnSize)`, BordaPoints, main="Distribution of Borda Points by Population (ln)", cex=0.5, cex.main=0.5, cex.lab=0.5, cex.axis=0.5, font.main=4, font.lab=2, font.axis=3, col=3, pch=1)

