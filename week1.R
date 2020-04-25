setwd("Documents/Caltech/Senior\ Year/Spring/cs11")
library(corrplot)
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

# Analyze CERQ columns
CERQ_Self_Blame <- conteExcluded$CERQ.Self.blame
hist(CERQ_Self_Blame, main = "CERQ Self Blame Data", xlab = "CERQ Score")
CERQ_Rumination <- conteExcluded$CERQ.Rumination
hist(CERQ_Rumination, main = "CERQ Rumination Data", xlab = "CERQ Score")
CERQ_Catastroph <- conteExcluded$CERQ.Catastrophizing
hist(CERQ_Catastroph, main = "CERQ Catastrophizing Data", xlab = "CERQ Score")

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

# Create a matrix of CERQ Self Blame, CERQ Rumination, and MSCEIT Facilitation
CERQ_MSCEIT <- cbind(CERQ_Self_Blame, CERQ_Rumination, MSCEIT_Facilitation)
# Remove NA data
CERQ_MSCEIT <- na.omit (CERQ_MSCEIT)
# Calculate the pearson correlation
CERQ_MSCEIT_cor <- cor(CERQ_MSCEIT)

# Plot the correlation matrix of the CERQ_MSCEIT data
corrplot(CERQ_MSCEIT_cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Remove NA data
Face_noNA <- na.omit(MSCEIT_Faces)
Facilitation_noNA <- na.omit(MSCEIT_Facilitation)
Changes_noNA <- na.omit(MSCEIT_Changes)
Management_noNA <- na.omit(MSCEIT_Management)

# Create a matrix of the MSCEIT data without NA values
MSCEIT_noNA <- cbind(Face_noNA, Facilitation_noNA, Changes_noNA, Management_noNA)

# Calculate the pearson correlation of the facilitation and mangement data
pearsonCor = cor(Facilitation_noNA, Management_noNA, method = "pearson")
# Calculate the spearman correlation
spearCor = cor(Facilitation_noNA, Management_noNA, method = "spearman")
# Calculate the kendall correlation 
kendallCor = cor(Facilitation_noNA, Management_noNA, method = "kendall")
# Calculate the pearson correlation between face and facilitation data
pearsonFaceFac = cor(Face_noNA, Facilitation_noNA, method = "pearson")

MSCEIT_cor <- cor(MSCEIT_noNA)

# Plot the correlation matrix of the MSCEIT data
# tl.col specifies text color
# t1.srt specifies text rotation
# order reorders the correlation matrix
library(corrplot)
corrplot(MSCEIT_cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
