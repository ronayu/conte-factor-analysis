setwd("Documents/Caltech/Senior\ Year/Spring/cs11")

# Libraries
library(corrplot)
library(paran)
library(nFactors)
library(paramap)

# load the file
conteData <- read.csv(file = 'PARL_Basics_Scores_20200401_Rona.csv', stringsAsFactors = FALSE)
# exclude values with low Quality measure
cex <- subset(conteData, Include..Exclude. != 'Exclude all data')

# Function to impute NA values with the column mean
impute_df <- function(df) {
  to_ret <- df
  for(i in 1:ncol(df)) {
    to_ret[is.na(to_ret[,i]), i] <- mean(to_ret[, i], na.rm=TRUE)
  }
  return(to_ret)
}

# Function to compute the overall correlation matrix
plot_corr <- function(df){
  M <- cor(df, method="spearman")
  corrplot(M, method="color")
}

# Function to extract factors
exFac <- function(df, numFac){
  fit <- factanal(df, numFac, rotation="varimax")
  # Plot factor 1 by factor 2
  load <- fit$loadings[,1:2]
  plot(load, type="n") #set up plot
  text(load, labels=names(df), cex=.7)
  return(fit$loadings)
}

# MSCEIT Option 2 Categories (31 categories) 
option_2 <- cbind(cex$STAI.State, cex$STAI.Trait, cex$BDI_Total.Total, cex$PANAS.Negative, 
                  cex$PANAS.Positive, cex$X16PF.Basics..Sten..Q4.Tension, 
                  cex$X16PF.Basics..Sten..C.Emotional.Stability,
                  cex$Pair.Cancellation.SS, cex$X16PF.Basics..Sten..B.Reasoning,
                  cex$Applied.Problems.SS, cex$IQ_VCI, cex$X16PF.Basics..Sten..O.Apprehension,
                  cex$IQ_PRI, cex$X16PF.Basics..Sten..G.Rule.Consciousness, 
                  cex$X16PF.Basics..Sten..Q1.Openness.to.Change, cex$Passage.Comprehension.SS, 
                  cex$X16PF.Basics..Sten..A.Warmth, cex$X16PF.Basics..Sten..Q2.Self.Reliance, 
                  cex$X16PF.Basics..Sten..N.Privateness, cex$X16PF.Basics..Sten..F.Liveliness, 
                  cex$X16PF.Basics..Sten..H.Social.Boldness, cex$SNI.People_in_network,
                  cex$X16PF.Basics..Sten..M.Abstractedness, cex$X16PF.Basics..Sten..Q3.Perfectionism,
                  cex$X16PF.Basics..Sten..E.Dominance,cex$X16PF.Basics..Sten..L.Vigilance, 
                  cex$X16PF.Basics..Sten..I.Sensitivity, cex$MSCEIT..standard.scores..B1_Perceiving,
                  cex$MSCEIT..standard.scores..B2_Using, cex$MSCEIT..standard.scores..B3_Understanding,
                  cex$MSCEIT..standard.scores..B4_Managing)
# MSCEIT Option 2 Categories (remove SS)
option_2_noss <- cbind(cex$STAI.State, cex$STAI.Trait, cex$BDI_Total.Total, cex$PANAS.Negative, 
                       cex$PANAS.Positive, cex$X16PF.Basics..Sten..Q4.Tension, 
                       cex$X16PF.Basics..Sten..C.Emotional.Stability,
                       cex$X16PF.Basics..Sten..B.Reasoning, cex$IQ_VCI, cex$X16PF.Basics..Sten..O.Apprehension,
                       cex$IQ_PRI, cex$X16PF.Basics..Sten..G.Rule.Consciousness, 
                       cex$X16PF.Basics..Sten..Q1.Openness.to.Change,
                       cex$X16PF.Basics..Sten..A.Warmth, cex$X16PF.Basics..Sten..Q2.Self.Reliance, 
                       cex$X16PF.Basics..Sten..N.Privateness, cex$X16PF.Basics..Sten..F.Liveliness, 
                       cex$X16PF.Basics..Sten..H.Social.Boldness, cex$SNI.People_in_network,
                       cex$X16PF.Basics..Sten..M.Abstractedness, cex$X16PF.Basics..Sten..Q3.Perfectionism,
                       cex$X16PF.Basics..Sten..E.Dominance,cex$X16PF.Basics..Sten..L.Vigilance, 
                       cex$X16PF.Basics..Sten..I.Sensitivity, cex$MSCEIT..standard.scores..B1_Perceiving,
                       cex$MSCEIT..standard.scores..B2_Using, cex$MSCEIT..standard.scores..B3_Understanding,
                       cex$MSCEIT..standard.scores..B4_Managing)

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

op2_df <- data.frame(option_2) # 144 subjects
sum(is.na(op2_df)/prod(dim(op2_df))) # 0.0719
op2_nona_df <- data.frame(na.omit(op2_df)) # 82 subjects
imputed_op2 <- impute_df(op2_df) # 144 subjects 

op2_noss_df <- data.frame(option_2_noss) # 144 subjects
sum(is.na(op2_noss_df)/prod(dim(op2_noss_df))) # 0.04018
op2_noss_nona_df <- data.frame(na.omit(op2_noss_df)) # 102 subjects
imp_op2_noss <- impute_df(op2_noss_df)

find_factors(op2_noss_nona_df)
find_factors(imp_op2_noss)

res_noss_nona3 <- exFac(op2_noss_nona_df, 3)
