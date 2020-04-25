setwd("Documents/Caltech/Senior\ Year/Spring/cs11")
# load the file
conteData <- read.csv(file = 'PARL_Basics_Scores_20200401_Rona.csv', stringsAsFactors = FALSE)
# exclude values with low Quality measure
conteExcluded <- subset(conteData, Quality == 'Include')

# Negative affect categories
STAI_Trait <- conteExcluded$STAI.Trait
STAI_State <- conteExcluded$STAI.State
BDI_Total <- conteExcluded$BDI_Total.Total
PANAS_Pos <- conteExcluded$PANAS.Positive
Tension <- conteExcluded$X16PF.Basics..Sten..Q4.Tension
PANAS_Neg <- conteExcluded$PANAS.Negative
MSCEIT_Facilitation <- conteExcluded$MSCEIT_Facilitation # has 84 non-NA data points

# Intellect/Cognition categories
PC_SS <- conteExcluded$Passage.Comprehension.SS # passage comp., has 72 non-NA data points
X16_Reasoning <- conteExcluded$X16PF.Basics..Sten..B.Reasoning
VCI <- conteExcluded$IQ_VCI
PRI <- conteExcluded$IQ_PRI
AP_SS <- conteExcluded$Applied.Problems.SS
X16PF_Rule_Consc <- conteExcluded$X16PF.Basics..Sten..G.Rule.Consciousness
X16_Open_Change <- conteExcluded$X16PF.Basics..Sten..Q1.Openness.to.Change
Pair_Cancellation_SS <- conteExcluded$Pair.Cancellation.SS # has 66 non-NA data points

# Social categories
Warmth <- conteExcluded$X16PF.Basics..Sten..A.Warmth
Self_Reliance <- conteExcluded$X16PF.Basics..Sten..Q2.Self.Reliance
Privateness <- conteExcluded$X16PF.Basics..Sten..N.Privateness
Liveliness <- conteExcluded$X16PF.Basics..Sten..F.Liveliness
Social_Boldness <- conteExcluded$X16PF.Basics..Sten..H.Social.Boldness
SNI_PIN <- conteExcluded$SNI.People_in_network

# Emotional perception categories
Sensations <- conteExcluded$MSCEIT..standard.scores..F_Sensations
Facial <- conteExcluded$MSCEIT_Facial
Blends <- conteExcluded$MSCEIT..standard.scores..G_Blends
Managing <- conteExcluded$MSCEIT..standard.scores..B4_Managing
Pictures <- conteExcluded$MSCEIT..standard.scores..E_Pictures
Abstractedness <- conteExcluded$X16PF.Basics..Sten..M.Abstractedness
MSCEIT_Changes <- conteExcluded$MSCEIT_Changes
Social_Management <- conteExcluded$MSCEIT..standard.scores..H_Social.Management

# Cognitive Focus
Perfectionism <- conteExcluded$X16PF.Basics..Sten..Q3.Perfectionism
Dominance <- conteExcluded$X16PF.Basics..Sten..E.Dominance
Vigilance <- conteExcluded$X16PF.Basics..Sten..L.Vigilance
Sensitivity <- conteExcluded$X16PF.Basics..Sten..I.Sensitivity

# Combine the relevant columns 
combined_all <- cbind(STAI_Trait, STAI_State, BDI_Total, PANAS_Pos, Tension, 
                  PANAS_Neg, MSCEIT_Facilitation, PC_SS, X16_Reasoning, VCI,
                  PRI, AP_SS, X16PF_Rule_Consc, X16_Open_Change, Pair_Cancellation_SS,
                  Warmth, Self_Reliance, Privateness, Liveliness, Social_Boldness,
                  SNI_PIN, Sensations, Facial, Blends, Managing, Pictures,
                  Abstractedness, MSCEIT_Changes, Social_Management, Perfectionism,
                  Dominance, Vigilance, Sensitivity)
combined_all <- na.omit(combined_all)

# Combine without Passage_Comprehension_SS or Pair_Cancellation_SS 
combined_no_ss <- cbind(STAI_Trait, STAI_State, BDI_Total, PANAS_Pos, Tension, 
                      PANAS_Neg, MSCEIT_Facilitation, X16_Reasoning, VCI,
                      PRI, X16PF_Rule_Consc, X16_Open_Change, 
                      Warmth, Self_Reliance, Privateness, Liveliness, Social_Boldness,
                      SNI_PIN, Sensations, Facial, Blends, Managing, Pictures,
                      Abstractedness, MSCEIT_Changes, Social_Management, Perfectionism,
                      Dominance, Vigilance, Sensitivity)
combined_no_ss <- na.omit(combined_no_ss) # n = 84 

# Find index of outlier(s)
library(outliers)
outliers <- grubbs.test(combined_all)
# Find index of highest value
which(combined_all == 145)[1]

all_cor <- cor(combined_all, method="spearman")
library(corrplot)
# Horn's Parallel Analysis
library(paran)
paran(combined_all, iterations=5000, cfa=TRUE, graph=TRUE)

# Without SS data 
paran(combined_no_ss, iterations=5000, cfa=TRUE, graph=TRUE)

# Combine without MSCEIT or SS data?

# Cattell's Scree Plot
library(nFactors)
# Convert to data frame
combined_all_df <- as.data.frame(combined_all)
# Verify df type - should say "data"
type <- eigenFrom(combined_all_df)
# Compute eigenvalues from data
combined_all_ev <- eigenComputes(combined_all_df, cor=TRUE, model="factors")

# Permutation parallel analysis distribution
aparallel <- eigenBootParallel(x=combined_all_df, cor=TRUE, quantile=0.95,
                               method="spearman")$quantile
# Determine number of components to retain. criteria: default eig >= mean
scree_res_all <- nScree(x=combined_all_df, aparallel=aparallel, 
                        cor=TRUE, model = "factors", method="spearman")
# Print component results
scree_res_all$Components
# Plot scree results
plotnScree(scree_res_all)
# Print analyses
scree_res_all$Analysis
# CNG Scree
cng_res_all <- nCng(x=combined_all_df, cor=TRUE, model="factors",
                    details=TRUE, method="spearman")
# Number of CNG Factors
cng_res_all$nFactors
# Matrix of details for each index
cng_res_all$detail
# Determine the number of factors via multiple regression (best)
mreg_res_all <- nMreg(x=combined_all_df, cor=TRUE, model="factors",
                     details=TRUE, method="spearman")
# Number of MREG Factors
mreg_res_all$nFactors
# Standard Error Scree
sescree_all <- nSeScree(combined_all_ev, cor=TRUE, model="factors", details=TRUE,
                        method="spearman", r2limen=0.75)
sescree_all$nFactors


########## Repeat without SS data (n = 84)
# Convert to data frame
combined_no_ss_df <- as.data.frame(combined_no_ss)
# Verify df type - should say "data"
type <- eigenFrom(combined_no_ss_df)
# Compute eigenvalues from data
combined_no_ss_ev <- eigenComputes(combined_no_ss_df, cor=TRUE, model="factors")

# Permutation parallel analysis distribution
aparallel <- eigenBootParallel(x=combined_no_ss_df, cor=TRUE, quantile=0.95,
                               method="spearman")$quantile
# Determine number of components to retain. criteria: default eig >= mean
scree_res_no_ss <- nScree(x=combined_no_ss_df, aparallel=aparallel, 
                        cor=TRUE, model = "factors", method="spearman")
# Print component results
scree_res_no_ss$Components
# Plot scree results
plotnScree(scree_res_no_ss)

# CNG Scree
cng_res_no_ss <- nCng(x=combined_no_ss_df, cor=TRUE, model="factors",
                    details=TRUE, method="spearman")
# Number of CNG Factors
cng_res_no_ss$nFactors
# Matrix of details for each index
cng_res_all$detail
# Determine the number of factors via multiple regression (best)
mreg_res_no_ss <- nMreg(x=combined_no_ss_df, cor=TRUE, model="factors",
                      details=TRUE, method="spearman")
# Number of MREG Factors
mreg_res_no_ss$nFactors
# Standard Error Scree
sescree_all <- nSeScree(combined_all_ev, cor=TRUE, model="factors", details=TRUE,
                        method="spearman", r2limen=0.75)
sescree_all$nFactors