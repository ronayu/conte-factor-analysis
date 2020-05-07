setwd("Documents/Caltech/Senior\ Year/Spring/cs11")

# Libraries
library(corrplot)
library("car")

# load the file
conteData <- read.csv(file = 'PARL_Basics_Scores_20200401_Rona.csv', stringsAsFactors = FALSE)
# exclude values with low Quality measure
conteExcluded <- subset(conteData, Include..Exclude. == 'Include')

# Negative affect categories
STAI_Trait <- conteExcluded$STAI.Trait
hist(STAI_Trait, main = "STAI Trait Raw Data", xlab = "STAI Trait Score")
boxplot(STAI_Trait, main = "STAI Trait Box Plot")
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

# 16PF Apprehension 
Apprehension <- conteExcluded$X16PF.Basics..Sten..O.Apprehension
hist(Apprehension, main = "16PF Apprehension Raw Data", xlab = "16PF Score")
boxplot(Apprehension, main = "16PF Apprehension Box Plot")
qqPlot(Apprehension, distribution="norm", envelope="FALSE")

# 16PF Emotional stability
Emotional_Stability <- conteExcluded$X16PF.Basics..Sten..C.Emotional.Stability
hist(Emotional_Stability, main = "16PF Emotional Stability Raw Data", xlab = "16PF Score")
boxplot(Emotional_Stability, main = "16PF Emotional Stability Box Plot")
qqPlot(Emotional_Stability, distribution="norm", envelope="FALSE")

MSCEIT_D <- conteExcluded$MSCEIT..standard.scores..D_Emotion.Mangement
hist(MSCEIT_D, main = "MSCEIT Emotion Management", xlab = "MSCEIT Score")
boxplot(MSCEIT_D, main = "MSCEIT Emotion Management Box Plot")
qqPlot(MSCEIT_D, distribution="norm", envelope="FALSE")

# Combined all categories - all of 16PF Category 1, MSCEIT Option 1 without D_Emotion Management
combined_all <- cbind(STAI_Trait, STAI_State, BDI_Total, PANAS_Pos, Tension, Emotional_Stability,
                      PANAS_Neg, MSCEIT_Facilitation, PC_SS, X16_Reasoning, VCI, Apprehension,
                      PRI, AP_SS, X16PF_Rule_Consc, X16_Open_Change, Pair_Cancellation_SS,
                      Warmth, Self_Reliance, Privateness, Liveliness, Social_Boldness,
                      SNI_PIN, Sensations, Facial, Blends, Pictures,
                      Abstractedness, MSCEIT_Changes, Social_Management, Perfectionism, MSCEIT_D,
                      Dominance, Vigilance, Sensitivity)

