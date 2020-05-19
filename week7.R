setwd("Documents/Caltech/Senior\ Year/Spring/cs11")

# Libraries
library(corrplot)
library(paran)
library(nFactors)
library(paramap)

# ************* Load File *************
conteData <- read.csv(file = 'PARL_Basics_Scores_20200401_Rona.csv', stringsAsFactors = FALSE)
# exclude values with low Quality measure
cex <- subset(conteData, Include..Exclude. != 'Exclude all data')
# Added PSS, SQ, and EQ
w_msceit <- cbind(cex$STAI.State, cex$STAI.Trait, cex$BDI_Total.Total, cex$PANAS.Negative, 
                  cex$PSS.Total, cex$SQ.score, cex$EQ.score,
                  cex$PANAS.Positive, cex$X16PF.Basics..Sten..Q4.Tension, 
                  cex$X16PF.Basics..Sten..C.Emotional.Stability,
                  cex$X16PF.Basics..Sten..B.Reasoning,
                  cex$IQ_VCI, cex$X16PF.Basics..Sten..O.Apprehension,
                  cex$IQ_PRI, cex$X16PF.Basics..Sten..G.Rule.Consciousness, 
                  cex$X16PF.Basics..Sten..Q1.Openness.to.Change,
                  cex$X16PF.Basics..Sten..A.Warmth, cex$X16PF.Basics..Sten..Q2.Self.Reliance, 
                  cex$X16PF.Basics..Sten..N.Privateness, cex$X16PF.Basics..Sten..F.Liveliness, 
                  cex$X16PF.Basics..Sten..H.Social.Boldness, cex$SNI.People_in_network,
                  cex$X16PF.Basics..Sten..M.Abstractedness, cex$X16PF.Basics..Sten..Q3.Perfectionism,
                  cex$X16PF.Basics..Sten..E.Dominance,cex$X16PF.Basics..Sten..L.Vigilance, 
                  cex$X16PF.Basics..Sten..I.Sensitivity, cex$MSCEIT..standard.scores..B1_Perceiving,
                  cex$MSCEIT..standard.scores..B2_Using, cex$MSCEIT..standard.scores..B3_Understanding)

# ************* Functions *************
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

# Function to calculate number of factors 
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

# Function to extract factors
exFac <- function(df, numFac){
  fit <- factanal(df, numFac, rotation="varimax")
  # Plot factor 1 by factor 2
  load <- fit$loadings[,1:2]
  plot(load, type="n") #set up plot
  text(load, labels=names(df), cex=.7)
  write.table(fit$loadings, file="factor_output.txt", sep = "\t")
  return((fit$loadings))
}

# Function to conduct factor extraction after removing X random subjects
# df: dataframe. numSubj: maximum number of subjects to remove 
remSubj <- function(df, numSubj){
  n <- dim(df)[1] # number of rows 
  r <- seq(1, numSubj) # 1 to numSubj
  for(numRemoved in r){
    toRemove <- sample(1:n, numRemoved, replace=F)
    rem <- df[-toRemove, ]
    print("Number of subjects removed is %i", numRemoved)
    find_factors(rem) # find factors 
  }
}

# ************* Initialize Data Frames *************
all_df <- data.frame(w_msceit) # 144 subjects
sum(is.na(all_df)/prod(dim(all_df))) # 0.0287037
nona_df <- data.frame(na.omit(all_df)) # 100 subjects
imputed_df <- impute_df(all_df) # 144 subjects

# ************* Function Calls *************
r <- exFac(nom_nona_df, 2)
write.table(r, file="factor_output.txt", sep = "\t")

remSubj(nom_nona_df, 20)