setwd("Documents/Caltech/Senior\ Year/Spring/cs11")
library(corrplot)
# load the file
conteData <- read.csv(file = 'PARL_Basics_Scores_20200401_Rona.csv', stringsAsFactors = FALSE)
# preview raw data (382 observations)
head(conteData)
# exclude values with low Quality measure
conteExcluded <- subset(conteData, Quality == 'Include')
# preview subsetted data (110 observations)
head(conteExcluded)

BDI <- conteExcluded$BDI_Total.Total
hist(BDI, main = "Total BDI", xlab = "BDI Score")
PSS <- conteExcluded$PSS.Total
hist(PSS, main = "Total PSS", xlab = "PSS Score")
PANAS_POS <- conteExcluded$PANAS.Positive
hist(PANAS_POS, main = "Positive PANAS", xlab = "PANAS Score")
PANAS_NEG <- conteExcluded$PANAS.Negative
hist(PANAS_NEG, main = "Negtive PANAS", xlab = "PANAS Score")
STAI_TRAIT <- conteExcluded$STAI.Trait
hist(STAI_TRAIT, main = "Trait STAI", xlab = "STAI Trait Score")
STAI_STATE <- conteExcluded$STAI.State
hist(STAI_STATE, main = "State STAI", xlab = "STAI Trait Score")

BDI_NONA <- na.omit(BDI)
PSS_NONA <- na.omit(PSS)
PANAS_POS_NONA <- na.omit(PANAS_POS)
PANAS_NEG_NONA <- na.omit(PANAS_NEG)
STAI_TRAIT_NONA <- na.omit(STAI_TRAIT)
STAI_STATE_NONA <- na.omit(STAI_STATE)

MOOD_NONA <- cbind(BDI_NONA, PSS_NONA, PANAS_POS_NONA, PANAS_NEG_NONA, 
                   STAI_TRAIT_NONA, STAI_STATE_NONA)
MOOD_COR <- cor(MOOD_NONA)
corrplot(MOOD_COR, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

MOOD_COR_KENDALL <- cor(MOOD_NONA, method = "kendall")
corrplot(MOOD_COR_KENDALL, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Will use this for future analyses (doesn't assume normality)
MOOD_COR_SPEARMAN <- cor(MOOD_NONA, method = "spearman")
corrplot(MOOD_COR_SPEARMAN, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Horn's Parallel Analysis
library(paran)
## Perform standard parallel analysis on mood data
paran(MOOD_NONA, iterations=5000, cfa=TRUE)
paran(MOOD_NONA, iterations=5000, cfa=TRUE, graph=TRUE)

# Read SQ and EQ
SQ <- conteExcluded$SQ.score
EQ <- conteExcluded$EQ.score
hist(SQ, main = "SQ", xlab = "SQ Score")
hist(EQ, main = "EQ", xlab = "EQ Score")
MOOD_EQ_NONA <- cbind(BDI_NONA, PSS_NONA, PANAS_POS_NONA, PANAS_NEG_NONA, 
                   STAI_TRAIT_NONA, STAI_STATE_NONA, EQ)
# Will use this for future analyses (doesn't assume normality)
MOOD_EQ_SPEARMAN <- cor(MOOD_EQ_NONA, method = "spearman")
corrplot(MOOD_EQ_SPEARMAN, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
paran(MOOD_EQ_SPEARMAN, iterations=5000, cfa=TRUE, graph=TRUE)

# Velicer's Minimum Average Partial (MAP) Test
# Warning in install.packages :
# package ‘paramap’ is not available (for R version 3.6.3)
#library(paramap)
#MAP(MOOD_EQ_SPEARMAN, 'spearman', verbose=TRUE)
#MAP(MOOD_EQ_NONA, 'spearman', verbose=TRUE)

# Plot QQ plots to assess normality 
qqnorm(STAI_TRAIT_NONA, main = "QQ Plot of STAI Trait", pch = 19)
qqline(STAI_TRAIT_NONA)

# Cattell's Scree Plot
library(nFactors)
data(dFactors) # nFactors example dataset
names(dFactors) # List the names in dFactors
dFactors$Cliff1$eigenvalues
# Scree plot of Cliff1
plotuScree(dFactors$Cliff1$eigenvalues)
# Convert MOOD_EQ_NONA to data frame
mood_eq_df <- as.data.frame(MOOD_EQ_NONA)
# Compute Eigenvalues of MOOD_EQ_NONA
mood_eq_ev <- eigenComputes(mood_eq_df, cor=FALSE, model="factors")

# Permutation parallel analysis distribution
aparallel <- eigenBootParallel(x=mood_eq_df, quantile=0.95)$quantile

# Number of components to retain
results <- nScree(x=mood_eq_ev, aparallel = aparallel)
results$Components
plotnScree(results)

# WASI Full Scale IQ (based on all 4 subtests)
FSIQ <- conteExcluded$IQ_FSIQ
FSIQ_NONA <- na.omit(FSIQ)
hist(FSIQ_NONA, main = "WASI Full Scale IQ", xlab = "IQ Score")
qqnorm(FSIQ_NONA, main = "QQ Plot of Full Scale IQ", pch = 19)
qqline(FSIQ_NONA)

# Find index of outlier(s)
library(outliers)
FSIQ_outlier <- grubbs.test(FSIQ_NONA)
# Find index of highest value - returns 68 
which(FSIQ_NONA == 239)[1]
# Plot FSIQ without index 68
FSIQ_no_outlier <- FSIQ_NONA[-68]
hist(FSIQ_no_outlier, main = "WASI Full Scale IQ (modified)", xlab = "IQ Score")
qqnorm(FSIQ_no_outlier, main = "QQ Plot of Full Scale IQ (modified)", pch = 19)
qqline(FSIQ_no_outlier)

# Calculate the correlation matrix of mood, EQ, and FSIQ
MOOD_EQ_FSIQ_NONA <- cbind(BDI_NONA, PSS_NONA, PANAS_POS_NONA, PANAS_NEG_NONA, 
                      STAI_TRAIT_NONA, STAI_STATE_NONA, EQ, FSIQ_NONA)
# Remove the 68th row (corresponds to outlier for FSIQ_NONA)
MOOD_EQ_FSIQ <- MOOD_EQ_FSIQ_NONA[-68,]
MOOD_EQ_FSIQ_cor <- cor(MOOD_EQ_FSIQ, method="spearman")
corrplot(MOOD_EQ_FSIQ_cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45) 
paran(MOOD_EQ_FSIQ_cor, iterations=5000, cfa=TRUE, graph=TRUE)
