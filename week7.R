setwd("/Users/ronayu/Documents/Caltech/Senior Year/Spring/cs11")

# Libraries
library(corrplot)
library(paran)
library(nFactors)
library(paramap)

# ************* Load File *************
conteData <- read.csv(file = 'PARL_Basics_Scores_20200401_Rona.csv', stringsAsFactors = FALSE) # n = 191
# exclude values with low Quality measure
cex <- subset(conteData, Include..Exclude. != 'Exclude all data') # n = 144
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
                  cex$MSCEIT..standard.scores..B2_Using, cex$MSCEIT..standard.scores..B3_Understanding,
                  cex$MSCEIT..standard.scores..B4_Managing)
names <- cbind("STAI State", "STAI Trait", "BDI Total", "PANAS Negative", "PSS", "SQ", "EQ", 
               "PANAS Positive", "16PF Tension", "16PF Emotional Stability", "16PF Reasoning",
               "VCI IQ", "16PF Apprehension", "PRI IQ", "16PF Rule Conciousness", "16PF Openness to Change",
               "16PF Warmth", "16PF Self Reliance", "16PF Privateness", "16PF Liveliness", 
               "16PF Social Boldness", "SNI People in Network", "16PF Abstractedness", "16PF Perfectionism", 
               "16PF Dominance", "16PF Vigilance", "16PF Sensitivity", "MSCEIT B1", "MSCEIT B2", 
               "MSCEIT B3", "MSCEIT B4")

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

# Function to write number of factors to file 
write_file <- function(method, numFac, fileName){ 
  cat(method, "\t", numFac, "\n", file=fileName,fill=FALSE, append=TRUE)
}

# Function to calculate number of factors 
find_factors <- function(df, fileName) {
  # Velicer's MAP
  a <- MAP(df, corkind="spearman", verbose=TRUE)
  write_file("Velicer's 1976", a[[3]][1], fileName)
  write_file("Velicer's 2000", a[[4]][1], fileName)
  # Horn's 
  a <- paran(df, cfa=TRUE, graph=FALSE)
  write_file("Horn's PA", a[[1]][1], fileName)
  # Scree NOC
  aparallel <- eigenBootParallel(x=df, cor=TRUE, quantile=0.95,method="spearman")$quantile
  r <- nScree(x=df, aparallel=aparallel, cor=TRUE, model = "factors", method="spearman")
  a <- r[[1]][1]
  a <- as.integer(a)
  print(r) 
  write_file("Cattell's Scree Test", a, fileName)
  # CNG
  cng <- nCng(x=df, cor=TRUE, model="factors", details=TRUE, method="spearman")
  write_file("Cng", cng[[2]], fileName)
  print("*****Cng*****")
  print(cng)
  # Multiply regression (b coeff)
  print("*****Zoski B Coefficient*****")
  a <- nMreg(x=df, cor=TRUE, model="factors",details=TRUE, method="spearman")
  write_file("Zoski", a[[2]][[1]], fileName)
  print(a)
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

# Function to estimate number of factors after removing X random subjects
# df: dataframe. numSubj: maximum number of subjects to remove 
remSubj <- function(df, numSubj, fileName){
  cat("New Trial", "\n", file=fileName, fill=FALSE, append=FALSE)
  n <- dim(df)[1] # number of rows 
  r <- seq(1, numSubj) # 1 to numSubj
  for(numRemoved in r){
    toRemove <- sample(1:n, numRemoved, replace=F)
    rem <- df[-toRemove, ]
    print(rem)
    cat("Number of subjects removed is: ", numRemoved, "\n", file=fileName, fill=FALSE, append=TRUE)
    find_factors(rem, fileName) # find factors 
  }
}

# Function to extract factors after removing X random subjects
remExtract <- function(df, numSubj, numFac, fileName){
  n <- dim(df)[1]
  toRemove <- sample(1:n, numSubj, replace=F)
  rem <- df[-toRemove, ]
  res <- exFac(rem, numFac)
  write.table(res, file=fileName, sep="\'t")
}

# ************* Initialize Data Frames *************
all_df <- data.frame(w_msceit) # 144 subjects
sum(is.na(all_df)/prod(dim(all_df))) # 0.0367385
nona_df <- data.frame(na.omit(all_df)) # 100 subjects, 31 variables
imputed_df <- impute_df(all_df) # 144 subjects

# ************* Function Calls *************
remSubj(nona_df, 50, "factor_robustness.tsv")
remExtract(nona_df, 1, 3, "factor_output.txt")
