setwd("Documents/Caltech/Senior\ Year/Spring/cs11")
# load the file
conteData <- read.csv(file = 'PARL_Basics_Scores_20200401_Rona.csv', stringsAsFactors = FALSE)
# exclude values with low Quality measure
conteExcluded <- subset(conteData, Quality == 'Include')

# Negative affect categories
STAI_Trait <- conteExcluded$STAI.Trait
hist(STAI_Trait, main = "STAI Trait Raw Data", xlab = "STAI Trait Score")
boxplot(STAI_Trait, main = "STAI Trait Box Plot")
library("car")
qqPlot(STAI_Trait, distribution="norm", envelope="FALSE")

STAI_State <- conteExcluded$STAI.State
hist(STAI_State, main = "STAI State Raw Data", xlab = "STAI State Score")
boxplot(STAI_State, main = "STAI State Box Plot")
qqPlot(STAI_State, distribution="norm", envelope="FALSE")

BDI_Total <- conteExcluded$BDI_Total.Total
hist(BDI_Total, main = "Total BDI Raw Data", xlab = "Total BDI Score")
boxplot(BDI_Total, main = "Total BDI Box Plot")
qqPlot(BDI_Total, distribution="norm", envelope="FALSE")

PANAS_Pos <- conteExcluded$PANAS.Positive
hist(PANAS_Pos, main = "Positive PANAS Raw Data", xlab = "Positive PANAS Score")
boxplot(PANAS_Pos, main = "Positive PANAS Box Plot")
qqPlot(PANAS_Pos, distribution="norm", envelope="FALSE")

Tension <- conteExcluded$X16PF.Basics..Sten..Q4.Tension
hist(Tension, main = "16PF Basics Tension Raw Data", xlab = "16PF Tension Score")
boxplot(Tension, main = "16PF Tension Box Plot")
qqPlot(Tension, distribution="norm", envelope="FALSE")

PANAS_Neg <- conteExcluded$PANAS.Negative
hist(PANAS_Neg, main = "Negative PANAS Raw Data", xlab = "Negative PANAS Score")
boxplot(PANAS_Neg, main = "Negative PANAS Box Plot")
qqPlot(PANAS_Neg, distribution="norm", envelope="FALSE")

MSCEIT_Facilitation <- conteExcluded$MSCEIT_Facilitation # has 84 non-NA data points
hist(MSCEIT_Facilitation, main = "MSCEIT Facilitation Raw Data", xlab = "MSCEIT Facilitation Score")
boxplot(MSCEIT_Facilitation, main = "MSCEIT Facilitation Box Plot")
qqPlot(MSCEIT_Facilitation, distribution="norm", envelope="FALSE")

# Intellect/Cognition categories
PC_SS <- conteExcluded$Passage.Comprehension.SS # passage comp., has 72 non-NA data points
hist(PC_SS, main = "Passage Comprehension Raw Data", xlab = "Woodcock-Johnson-III Scaled Score (Age-Group Norms)")
boxplot(PC_SS, main = "Passage Comprehension Box Plot")
qqPlot(PC_SS, distribution="norm", envelope="FALSE")

X16_Reasoning <- conteExcluded$X16PF.Basics..Sten..B.Reasoning
hist(X16_Reasoning, main = "16PF Basics Reasoning Raw Data", xlab = "16PF Reasoning Score")
boxplot(X16_Reasoning, main = "16PF Reasoning Box Plot")

VCI <- conteExcluded$IQ_VCI
hist(VCI, main = "VCI IQ Raw Data", xlab = "VCI IQ Score")
boxplot(VCI, main = "VCI IQ Box Plot")
qqPlot(VCI, distribution="norm", envelope="FALSE")

PRI <- conteExcluded$IQ_PRI
hist(PRI, main = "PRI IQ Raw Data", xlab = "PRI IQ Score")
boxplot(PRI, main = "PRI IQ Box Plot")

AP_SS <- conteExcluded$Applied.Problems.SS
hist(AP_SS, main = "Applied Problems Scaled Score Raw Data", xlab = "Woodcock-Johnson-III Scaled Score (Age-Group Norms)")
boxplot(AP_SS, main = "Applied Problems Box Plot")
qqPlot(AP_SS, distribution="norm", envelope="FALSE")

X16PF_Rule_Consc <- conteExcluded$X16PF.Basics..Sten..G.Rule.Consciousness
hist(X16PF_Rule_Consc, main = "16PF Rule Consciousness Raw Data", xlab = "16PF Rule Consciousness")
boxplot(X16PF_Rule_Consc, main = "16PF Rule Consciousness Box Plot")
qqPlot(X16PF_Rule_Consc, distribution="norm", envelope="FALSE")

X16_Open_Change <- conteExcluded$X16PF.Basics..Sten..Q1.Openness.to.Change
hist(X16_Open_Change, main = "16PF Openness to Change Raw Data", xlab = "16PF Openness to Change Score")
boxplot(X16_Open_Change, main = "16PF Openness to Change Box Plot")

Pair_Cancellation_SS <- conteExcluded$Pair.Cancellation.SS # has 66 non-NA data points
hist(Pair_Cancellation_SS, main = "Pair Cancellation Raw Data", xlab = "Woodcock-Johnson-III Scaled Score (Age-Group Norms)")
boxplot(Pair_Cancellation_SS, main = "Pair Cancellation Box Plot")
qqPlot(Pair_Cancellation_SS, distribution="norm", envelope="FALSE")

# Social categories
Warmth <- conteExcluded$X16PF.Basics..Sten..A.Warmth
hist(Warmth, main = "16PF Warmth Raw Data", xlab = "16PF Warmth Score")
boxplot(Warmth, main = "16PF Warmth Box Plot")

Self_Reliance <- conteExcluded$X16PF.Basics..Sten..Q2.Self.Reliance
hist(Self_Reliance, main = "16PF Self Reliance Raw Data", xlab = "16PF Self Reliance Score")
boxplot(Self_Reliance, main = "16PF Self Reliance Box Plot")

Privateness <- conteExcluded$X16PF.Basics..Sten..N.Privateness
hist(Privateness, main = "16PF Privateness Raw Data", xlab = "16PF Privateness Score")
boxplot(Privateness, main = "16PF Privateness Box Plot")
qqPlot(Privateness, distribution="norm", envelope="FALSE")

Liveliness <- conteExcluded$X16PF.Basics..Sten..F.Liveliness
hist(Liveliness, main = "16PF Liveliness Raw Data", xlab = "16PF Liveliness Score")
boxplot(Liveliness, main = "16PF Liveliness Box Plot")

Social_Boldness <- conteExcluded$X16PF.Basics..Sten..H.Social.Boldness
hist(Social_Boldness, main = "16PF Social Boldness Raw Data", xlab = "16PF Social Boldness Score")
boxplot(Social_Boldness, main = "16PF Social Boldness Box Plot")

SNI_PIN <- conteExcluded$SNI.People_in_network
hist(SNI_PIN, main = "SNI People in Network Raw Data", xlab = "Number of People")
boxplot(SNI_PIN, main = "SNI People in Network Box Plot")
qqPlot(SNI_PIN, distribution="norm", envelope="FALSE")

# Emotional perception categories
Sensations <- conteExcluded$MSCEIT..standard.scores..F_Sensations
hist(Sensations, main = "MSCEIT Sensations Raw Data", xlab = "MSCEIT Score")
boxplot(Sensations, main = "MSCEIT Sensations Box Plot")
qqPlot(Sensations, distribution="norm", envelope="FALSE")

Facial <- conteExcluded$MSCEIT_Facial
hist(Facial, main = "MSCEIT Facial Raw Data", xlab = "MSCEIT Score")
boxplot(Facial, main = "MSCEIT Facial Box Plot")
qqPlot(Facial, distribution="norm", envelope="FALSE")

Blends <- conteExcluded$MSCEIT..standard.scores..G_Blends
hist(Blends, main = "MSCEIT Blends Raw Data", xlab = "MSCEIT Score")
boxplot(Blends, main = "MSCEIT Blends Box Plot")
qqPlot(Blends, distribution="norm", envelope="FALSE")

Managing <- conteExcluded$MSCEIT..standard.scores..B4_Managing
hist(Managing, main = "MSCIET Managing Raw Data", xlab = "MSCEIT Score")
boxplot(Managing, main = "MSCEIT Managing Box Plot")
qqPlot(Managing, distribution="norm", envelope="FALSE")

Pictures <- conteExcluded$MSCEIT..standard.scores..E_Pictures
hist(Pictures, main = "MSCEIT Pictures Raw Data", xlab = "MSCEIT Score")
boxplot(Pictures, main = "MSCEIT Pictures Box Plot")
qqPlot(Pictures, distribution="norm", envelope="FALSE")

Abstractedness <- conteExcluded$X16PF.Basics..Sten..M.Abstractedness
hist(Abstractedness, main = "16PF Abstractedness Raw Data", xlab = "16PF Score")
boxplot(Abstractedness, main = "16 PF Abstractedness Box Plot")
qqPlot(Abstractedness, distribution="norm", envelope="FALSE")

MSCEIT_Changes <- conteExcluded$MSCEIT_Changes
hist(MSCEIT_Changes, main = "MSCEIT Changes Raw Data", xlab = "MSCEIT Score")
boxplot(MSCEIT_Changes, main = "MSCEIT Changes Box Plot")
qqPlot(MSCEIT_Changes, distribution="norm", envelope="FALSE")

Social_Management <- conteExcluded$MSCEIT..standard.scores..H_Social.Management
hist(Social_Management, main = "MSCEIT Social Management Raw Data", xlab = "MSCEIT Score")
boxplot(Social_Management, main = "MSCEIT Social Management Box Plot")
qqPlot(Social_Management, distribution="norm", envelope="FALSE")

# Cognitive Focus
Perfectionism <- conteExcluded$X16PF.Basics..Sten..Q3.Perfectionism
hist(Perfectionism, main = "16PF Perfectionism Raw Data", xlab = "16PF Score")
boxplot(Perfectionism, main = "16PF Perfectionism Box Plot")
qqPlot(Perfectionism, distribution="norm", envelope="FALSE")

Dominance <- conteExcluded$X16PF.Basics..Sten..E.Dominance
hist(Dominance, main = "16PF Dominance Raw Data", xlab = "16PF Score")
boxplot(Dominance, main = "16PF Dominance Box Plot")

Vigilance <- conteExcluded$X16PF.Basics..Sten..L.Vigilance
hist(Vigilance, main = "16PF Vigilance Raw Data", xlab = "16PF Score")
boxplot(Vigilance, main = "16PF Vigilance Box Plot")

Sensitivity <- conteExcluded$X16PF.Basics..Sten..I.Sensitivity
hist(Sensitivity, main = "16PF Sensitivity Raw Data", xlab = "16PF Score")
boxplot(Sensitivity, main = "16PF Sensitivity Box Plot")

# Combine the relevant columns 
combined_all <- cbind(STAI_Trait, STAI_State, BDI_Total, PANAS_Pos, Tension, 
                  PANAS_Neg, MSCEIT_Facilitation, PC_SS, X16_Reasoning, VCI,
                  PRI, AP_SS, X16PF_Rule_Consc, X16_Open_Change, Pair_Cancellation_SS,
                  Warmth, Self_Reliance, Privateness, Liveliness, Social_Boldness,
                  SNI_PIN, Sensations, Facial, Blends, Managing, Pictures,
                  Abstractedness, MSCEIT_Changes, Social_Management, Perfectionism,
                  Dominance, Vigilance, Sensitivity)
# determine the percentage of missing data by column
combined_df <- data.frame(combined_all)
sum(is.na(combined_df)/prod(dim(combined_df)))
# mising by column
colMeans(is.na(combined_df))

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

# Very Simple Structure
vss(x=combined_all_df)

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

# Velicer's MAP Test
library(paramap)
vmap_all_nona <- MAP(combined_all_df, corkind="spearman", verbose=TRUE)
vmap_noss_nona <- MAP(combined_no_ss_df, corkind="spearman", verbose=TRUE)

# Impute missing values
combined_w_na <- cbind(STAI_Trait, STAI_State, BDI_Total, PANAS_Pos, Tension, 
                                       PANAS_Neg, MSCEIT_Facilitation, PC_SS, X16_Reasoning, VCI,
                                       PRI, AP_SS, X16PF_Rule_Consc, X16_Open_Change, Pair_Cancellation_SS,
                                       Warmth, Self_Reliance, Privateness, Liveliness, Social_Boldness,
                                       SNI_PIN, Sensations, Facial, Blends, Managing, Pictures,
                                       Abstractedness, MSCEIT_Changes, Social_Management, Perfectionism,
                                       Dominance, Vigilance, Sensitivity)
# Convert to data frame
combined_w_na <- as.data.frame(combined_w_na)
for(i in 1:ncol(combined_w_na)) {
  combined_w_na[is.na(combined_w_na[,i]), i] <- mean(combined_w_na[,i], na.rm=TRUE)
}
vmap_imputed <- MAP(combined_w_na, corkind="spearman", verbose=TRUE)

# Scree test on imputed
aparallel <- eigenBootParallel(x=combined_w_na, cor=TRUE, quantile=0.95, 
                               method="spearman")$quantile
scree_res_all <- nScree(x=combined_w_na, aparallel=aparallel, 
                        cor=TRUE, model="factors", method="spearman")
scree_res_all$Components
# Horn's on imputed
paran(combined_w_na, iterations=5000, cfa=TRUE, graph=TRUE)
# CNG Scree
cng_res_wna <- nCng(x=combined_w_na, cor=TRUE, model="factors",
                    details=TRUE, method="spearman")
cng_res_all$nFactors
# multiple regression
mreg_wna <- nMreg(x=combined_w_na, cor=TRUE, model="factors", 
                  details=TRUE, method="spearman")
mreg_wna#nFactors

## Conduct factor analysis with 29 categories
combined_29 <- cbind(STAI_Trait, PANAS_Pos, Tension, 
                      MSCEIT_Facilitation, PC_SS, X16_Reasoning, VCI,
                      PRI, AP_SS, X16PF_Rule_Consc, X16_Open_Change, Pair_Cancellation_SS,
                      Warmth, Self_Reliance, Privateness, Liveliness, Social_Boldness,
                      Sensations, Facial, Blends, Managing, Pictures,
                      Abstractedness, MSCEIT_Changes, Social_Management, Perfectionism,
                      Dominance, Vigilance, Sensitivity)
combined29_ <- na.omit(combined_29)
combined29_nona_df <- as.data.frame(combined29_nona)
# VMAP
vmap_all_nona <- MAP(combined29_nona_df, corkind="spearman", verbose=TRUE)
# Horn's PA 
paran(combined29_nona, iterations=5000, cfa=TRUE, graph=TRUE)
# Scree NOC and Kaiser
aparallel <- eigenBootParallel(x=combined29_nona_df, cor=TRUE, quantile=0.95,
                               method="spearman")$quantile
scree_res_29nona <- nScree(x=combined29_nona_df, aparallel=aparallel, 
                        cor=TRUE, model = "factors", method="spearman")
# Cng
cng_res_29nona <- nCng(x=combined29_nona_df, cor=TRUE, model="factors",
                    details=TRUE, method="spearman")
cng_res_29nona$nFactors
# Zoski and Jur's B coeff
b_coeff29nona <- nMreg(x=combined29_nona_df, cor=TRUE, model="factors",
                        details=TRUE, method="spearman")
# Number of MREG Factors
b_coeff29nona$nFactors

# Impute 29 factors
combined_29_wna <- cbind(STAI_Trait, PANAS_Pos, Tension, 
                     MSCEIT_Facilitation, PC_SS, X16_Reasoning, VCI,
                     PRI, AP_SS, X16PF_Rule_Consc, X16_Open_Change, Pair_Cancellation_SS,
                     Warmth, Self_Reliance, Privateness, Liveliness, Social_Boldness,
                     Sensations, Facial, Blends, Managing, Pictures,
                     Abstractedness, MSCEIT_Changes, Social_Management, Perfectionism,
                     Dominance, Vigilance, Sensitivity)
c29_wna_df <- as.data.frame(combined_29)
for(i in 1:ncol(c29_wna_df)) {
  c29_wna_df[is.na(c29_wna_df[,i]), i] <- mean(c29_wna_df[,i], na.rm=TRUE)
}
# VMAP
vmap_wna <- MAP(c29_wna_df, corkind="spearman", verbose=TRUE)
# Horn's PA 
paran(c29_wna_df, iterations=5000, cfa=TRUE, graph=TRUE)
# Scree NOC and Kaiser
aparallel <- eigenBootParallel(x=c29_wna_df, cor=TRUE, quantile=0.95,
                               method="spearman")$quantile
scree_res_29nona <- nScree(x=c29_wna_df, aparallel=aparallel, 
                           cor=TRUE, model = "factors", method="spearman")
# Cng
cng_res_29nona <- nCng(x=c29_wna_df, cor=TRUE, model="factors",
                       details=TRUE, method="spearman")
cng_res_29nona$nFactors
# Zoski and Jur's B coeff
b_coeff29nona <- nMreg(x=c29_wna_df, cor=TRUE, model="factors",
                       details=TRUE, method="spearman")
# Number of MREG Factors
b_coeff29nona$nFactors



## Extract factors from 33 category data, no NA
fit <- factanal(combined_all_df, 4, rotation="varimax")
# Plot factor 1 by factor 2
load <- fit$loadings[,1:2]
plot(load, type="n") #set up plot
text(load, labels=names(combined_all_df), cex=.7)
