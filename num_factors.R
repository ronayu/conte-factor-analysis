setwd("Documents/Caltech/Senior\ Year/Spring/cs11")

# Libraries
library(corrplot)
library(paran)
library(nFactors)
library(paramap)

# load the file
conteData <- read.csv(file = 'PARL_Basics_Scores_20200401_Rona.csv', stringsAsFactors = FALSE)
# exclude values with low Quality measure
cex <- subset(conteData, Include..Exclude. == 'Include')

# Option 1 categories
option_1 <- cbind(cex$STAI.State, cex$STAI.Trait, cex$BDI_Total.Total, cex$PANAS.Negative, 
                  cex$PANAS.Positive, cex$X16PF.Basics..Sten..Q4.Tension, 
                  cex$X16PF.Basics..Sten..C.Emotional.Stability, cex$MSCEIT..standard.scores..B_Facilitation,
                  cex$Pair.Cancellation.SS, cex$X16PF.Basics..Sten..B.Reasoning,
                  cex$Applied.Problems.SS, cex$IQ_VCI, cex$X16PF.Basics..Sten..O.Apprehension,
                  cex$IQ_PRI, cex$X16PF.Basics..Sten..G.Rule.Consciousness, 
                  cex$X16PF.Basics..Sten..Q1.Openness.to.Change, cex$Passage.Comprehension.SS, 
                  cex$X16PF.Basics..Sten..A.Warmth, cex$X16PF.Basics..Sten..Q2.Self.Reliance, 
                  cex$X16PF.Basics..Sten..N.Privateness, cex$X16PF.Basics..Sten..F.Liveliness, 
                  cex$X16PF.Basics..Sten..H.Social.Boldness, cex$SNI.People_in_network,
                  cex$MSCEIT..standard.scores..F_Sensations, cex$MSCEIT..standard.scores..A_Faces,
                  cex$MSCEIT..standard.scores..G_Blends, cex$MSCEIT..standard.scores..E_Pictures,
                  cex$X16PF.Basics..Sten..M.Abstractedness, cex$MSCEIT..standard.scores..C_Changes, 
                  cex$MSCEIT..standard.scores..H_Social.Management, cex$X16PF.Basics..Sten..Q3.Perfectionism,
                  cex$MSCEIT..standard.scores..D_Emotion.Mangement, cex$X16PF.Basics..Sten..E.Dominance,
                  cex$X16PF.Basics..Sten..L.Vigilance, cex$X16PF.Basics..Sten..I.Sensitivity)

# Option 1 categories without SS
option_1_noss <- cbind(cex$STAI.State, cex$STAI.Trait, cex$BDI_Total.Total, cex$PANAS.Negative, 
                  cex$PANAS.Positive, cex$X16PF.Basics..Sten..Q4.Tension, 
                  cex$X16PF.Basics..Sten..C.Emotional.Stability, cex$MSCEIT..standard.scores..B_Facilitation,
                  cex$X16PF.Basics..Sten..B.Reasoning, cex$IQ_VCI, cex$X16PF.Basics..Sten..O.Apprehension,
                  cex$IQ_PRI, cex$X16PF.Basics..Sten..G.Rule.Consciousness, 
                  cex$X16PF.Basics..Sten..Q1.Openness.to.Change,cex$X16PF.Basics..Sten..A.Warmth, 
                  cex$X16PF.Basics..Sten..Q2.Self.Reliance, 
                  cex$X16PF.Basics..Sten..N.Privateness, cex$X16PF.Basics..Sten..F.Liveliness, 
                  cex$X16PF.Basics..Sten..H.Social.Boldness, cex$SNI.People_in_network,
                  cex$MSCEIT..standard.scores..F_Sensations, cex$MSCEIT..standard.scores..A_Faces,
                  cex$MSCEIT..standard.scores..G_Blends, cex$MSCEIT..standard.scores..E_Pictures,
                  cex$X16PF.Basics..Sten..M.Abstractedness, cex$MSCEIT..standard.scores..C_Changes, 
                  cex$MSCEIT..standard.scores..H_Social.Management, cex$X16PF.Basics..Sten..Q3.Perfectionism,
                  cex$MSCEIT..standard.scores..D_Emotion.Mangement, cex$X16PF.Basics..Sten..E.Dominance,
                  cex$X16PF.Basics..Sten..L.Vigilance, cex$X16PF.Basics..Sten..I.Sensitivity)

# Function to impute NA values with the column mean
impute_df <- function(df) {
  to_ret <- df
  for(i in 1:ncol(df)) {
    to_ret[is.na(to_ret[,i]), i] <- mean(to_ret[, i], na.rm=TRUE)
  }
  return(to_ret)
}

op1_df <- data.frame(option_1) # 110 subjects
sum(is.na(op1_df)/prod(dim(op1_df))) # 0.08675
op1_nona_df <- data.frame(na.omit(op1_df)) # 65 subjects
imputed_op1 <- impute_df(op1_df) #110 subjects 

op1_noss_df <- data.frame(option_1_noss)
sum(is.na(op1_noss_df)/prod(dim(op1_noss_df))) # 0.05909
op1_noss_nona_df <- data.frame(na.omit(op1_noss_df)) # 84 subjects
imp_op1_noss <- impute_df(op1_noss_df)

# Function to run all the packages and find the number of factors
find_factors <- function(df) {
  # Velicer's MAP
  MAP(df, corkind="spearman", verbose=TRUE)
  # Horn's 
  paran(df, cfa=TRUE, graph=TRUE)
  # Scree NOC
  aparallel <- eigenBootParallel(x=df, cor=TRUE, quantile=0.95,method="spearman")$quantile
  r <- nScree(x=df, aparallel=aparallel, cor=TRUE, model = "factors", method="spearman")
  print("*****Scree's Test*****")
  print(r)
  # CNG
  cng <- nCng(x=df, cor=TRUE, model="factors",
              details=TRUE, method="spearman")
  print("*****Cng*****")
  print(cng)
  # Multiply regression (b coeff)
  print("*****Zoski B Coefficient*****")
  print(nMreg(x=df, cor=TRUE, model="factors",
        details=TRUE, method="spearman"))
}

# Find the number of factors for option 1
op1_nona <- find_factors(op1_nona_df)
# Repeat above with imputed option 1 data
op1_imp <- find_factors(imputed_op1)

# Repeat for no ss 
op1_noss_nona <- find_factors(op1_noss_nona_df)
op1_noss_imp <- find_factors(imp_op1_noss)

# Function to compute the overall correlation matrix
plot_corr <- function(df){
  M <- cor(df)
  corrplot(M, method="color")
}

plot_corr(data.frame(op1_nona_df))
