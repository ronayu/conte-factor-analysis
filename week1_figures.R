setwd("Documents/Caltech/Senior\ Year/Spring/cs11")
# load the file
conteData <- read.csv(file = 'PARL_Basics_Scores_20200401_Rona.csv', stringsAsFactors = FALSE)
# preview raw data (382 observations)
head(conteData)

# see internal structure of data
str(conteData)

# exclude values with low Quality measure
conteExcluded <- subset(conteData, Quality == 'Include')
# preview subsetted data (110 observations)
head(conteExcluded)

# Analyze 4 columns related to emotion 
# MSCEIT standard scores: Faces, Facilitation, Changes, Management
MSCEIT_Faces <- conteExcluded$MSCEIT_Facial
hist(MSCEIT_Faces, main = "MSCEIT Facial Expression Data", xlab = "MSCEIT Score")
MSCEIT_Facilitation <- conteExcluded$MSCEIT_Facilitation
hist(MSCEIT_Facilitation, main = "MSCEIT Facilitation Data", xlab = "MSCEIT Score")
MSCEIT_Changes <- conteExcluded$MSCEIT_Changes
hist(MSCEIT_Changes, main = "MSCEIT Change Data", xlab = "MSCEIT Score")
MSCEIT_Management <- conteExcluded$MSCEIT_Management
hist(MSCEIT_Management, main = "MSCEIT Management Data", xlab = "MSCEIT Score")

# Remove NA data in facilitation and management
Facilitation_noNA <- na.omit(MSCEIT_Facilitation)
Management_noNA <- na.omit(MSCEIT_Management)
# Calculate the pearson correlation of the facilitation and mangement data
pearsonCor = cor(Facilitation_noNA, Management_noNA, method = "pearson")
# Calculate the spearman correlation
spearCor = cor(Facilitation_noNA, Management_noNA, method = "spearman")
# Calculate the kendall correlation 
kendallCor = cor(Facilitation_noNA, Management_noNA, method = "kendall")