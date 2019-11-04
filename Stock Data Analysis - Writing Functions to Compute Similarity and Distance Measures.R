# Stock Data Analysis - Writing Functions to Compute Similarity and Distance Measures

# Let us first load the data provided.

Fund <- read.csv("fundamentals.csv")
Sec <- read.csv("securities.csv")

# Now let us first filter the rows (years) and columns that are not required from the Fund Data

Fund <- subset(Fund, Fund$For.Year == 2013)

reqd.names <- c("Ticker.Symbol","Total.Equity", "Total.Liabilities", "Total.Assets", "Investments", "Net.Cash.Flow", "Total.Revenue", "Gross.Profit", "Profit.Margin","Earnings.Per.Share")

Fund <- Fund[reqd.names]

# Let us extract the state data from the address in Sec as the same may be considered as 
# a categorical variable

for(i in 1:nrow(Sec)){
  Sec$State[i] <- unlist(strsplit(as.character(Sec$Address.of.Headquarters[i]), split = ","))[2]
}

# Now let us determine 100 companies from sec data using the sample function which will randomly
# choose the names

# Sel.secs <- Sec[sample(100),]
# save(Sel.secs, file = "csv")

load("csv")

# Now we shall use a for loop to pull out the data with respect to tickers from the Fund dataset

secs.names <- as.character(Sel.secs$Ticker.symbol)
Fund.Data <- data.frame()
for (i in secs.names) {      
  row.no <- which(Fund$Ticker.Symbol == i)
  data.tmp <- Fund[row.no,]
  Fund.Data <- rbind(Fund.Data, data.tmp)
}

# It is interesting to observe that only 85 rows are there in data.frame. This
# is because the details of the remaining securities are unavailable in fund data for 2013.

# We have pulled out the missing values below

miss.no <- which(!Sel.secs$Ticker.symbol %in% Fund.Data$Ticker.Symbol)
miss.names <- vector()
for (i in miss.no) {
  tmp <- as.character(Sel.secs$Ticker.symbol[i])
  miss.names <- c(miss.names, tmp)
}

# Now let us remove the duplicate value from the rows in the Fund Data

Fund.Data <- Fund.Data[-max(which(Fund.Data$Ticker.Symbol == "ANTM")),]

# Let us now you use the ticker details to assign row names to each row

row.names(Fund.Data) <- Fund.Data[,1]

# Computing distances with large values may be difficult and therefore, let us look at all
# quantitiative data in millions.

Fund.Data$Total.Liabilities <- Fund.Data$Total.Liabilities/(10^6)
Fund.Data$Total.Assets <- Fund.Data$Total.Assets/(10^6)
Fund.Data$Total.Equity <- Fund.Data$Total.Equity/(10^6)
Fund.Data$Investments <- Fund.Data$Investments/(10^6)
Fund.Data$Total.Revenue <- Fund.Data$Total.Revenue/(10^6)
Fund.Data$Net.Cash.Flow <- Fund.Data$Net.Cash.Flow/(10^6)
Fund.Data$Gross.Profit <- Fund.Data$Gross.Profit/(10^6)

# Given that the industry of the security will also be an interesting data to observe.
# we can add that from the secs dataframe.

indus.name <- vector()
secs.id <- as.character(Fund.Data$Ticker.Symbol)
for (i in secs.id) {
  b <- which(Sec$Ticker.symbol == i)
  tmp <- as.character(Sec$GICS.Sector[b])
  indus.name <- c(indus.name,tmp)
}

Fund.Data.QC <- cbind(Fund.Data, indus.name)

# Similarly we can add the state to the Fund.Data.QC using the above formula

Sec$State <- as.character(Sec$State)
state.name <- vector()
for (i in secs.id) {
  b <- which(Sec$Ticker.symbol == i)
  tmp <- as.character(Sec$State[b])
  state.name <- c(state.name,tmp)
}

Fund.Data.QC <- cbind(Fund.Data.QC, state.name)

# Let us test now begin the distance and similarity computation for the selected stocks.

# We shall isolate datasets with quantiative data and categorical data to ensure we
# ease of computation. 

Fund.Data.Q <- Fund.Data[,-1]             # For Quant Data
Fund.Data.C <- Fund.Data.QC[,-(2:10)]     # For Categorical Data
Fund.Data.QC                              # For combined Data

# Now let us begin computation of the distances. The distances have been computed
# using the dist function.

Lp.norm.1 <- as.matrix(dist(Fund.Data.Q, method = "manhattan"))
Lp.norm.2 <- as.matrix(dist(Fund.Data.Q, method = "euclidean"))
Lp.norm.3 <- as.matrix(dist(Fund.Data.Q, method = "minkowski", p = 3))
Lp.norm.10 <- as.matrix(dist(Fund.Data.Q, method = "minkowski", p = 10))

# Now let us create a ranking function to rank the top 10 and the bottom 
# 10 distances.

Ranking.top <- function(matrix){
  Ticker.names <- rownames(matrix)
  rank.data <- as.data.frame(matrix(0,ncol = 3, nrow = ((length(Ticker.names)^2))))
  ind.tmp <- 1
  for(i in Ticker.names){
    for(j in Ticker.names){
      tmp <- c(i,j, matrix[i,j])
      rank.data[ind.tmp,] <- tmp
      ind.tmp <- ind.tmp + 1
      }
  }
  colnames(rank.data) <- c("Tick1", "Tick2", "Measure")
  rank.data$Measure <- as.numeric(rank.data$Measure)
  top.10 <- rank.data[order(rank.data$Measure, decreasing=TRUE)[1:20],]
  return(top.10)
  }

Ranking.bot <- function(matrix){
  Ticker.names <- rownames(matrix)
  rank.data <- as.data.frame(matrix(0,ncol = 3, nrow = ((length(Ticker.names)^2))))
  ind.tmp <- 1
  for(i in Ticker.names){
    for(j in Ticker.names){
      tmp <- c(i,j, matrix[i,j])
      rank.data[ind.tmp,] <- tmp
      ind.tmp <- ind.tmp + 1
    }
  }
  colnames(rank.data) <- c("Tick1", "Tick2", "Measure")
  rank.data$Measure <- as.numeric(rank.data$Measure)
  rank.data <- rank.data[-which(rank.data$Tick1 == rank.data$Tick2),]
  top.10 <- rank.data[order(rank.data$Measure, decreasing=FALSE)[1:20],]
  return(top.10)
}

Ranking.top(Lp.norm.1)
Ranking.top(Lp.norm.2)
Ranking.top(Lp.norm.3)
Ranking.top(Lp.norm.10)

Ranking.bot(Lp.norm.1)
Ranking.bot(Lp.norm.2)
Ranking.bot(Lp.norm.3)
Ranking.bot(Lp.norm.10)

# We can observe that each distance computation gives us different top 10 rankings showing
# the importance of type od distance function used.

# e) Now , let us write a function to compute the Minkovski distance. The standard weights will
# be equal for all variables and this may be modified if required.

minkowski = function(i,j, data.frame,p = 1,vector = rep((1/(ncol(data.frame))),ncol(data.frame))){
  if(sum(vector) != 1){
    stop("Weights not equal to 1")
  }else if(length(vector) != ncol(data.frame)){
    stop("Error: Number of weights not equal to columns")
  }else{
    if(i != j){
      wts  <- vector
      tot.i = 0
      for (k in 1:ncol(data.frame)) {
        xk <- data.frame[i,k]
        yk <- data.frame[j,k]
        pk <- wts[k]*((abs(xk - yk))^p)
        tot.i <- tot.i + pk
      }
    }}
  tot.r <- tot.i^(1/p)
  return(tot.r)
}

minkowski(3,4,Fund.Data.Q, p = 1, c(0.2,0.05,0.05,0.1,0.1,0.1,0.15,0.15,0.1)) # Here we have 
# given weights for each variables based on our experience. This can be modified as required 
# by the user.

# Note: The above minkowski function can be modified to compute the Lp Norms as well. Given
# the presence of dist function I have not used this method. If required we can
# create separate functions for Lp norms as well.

# Now let us create a general function to compute the minkowski distance for 
# a given matrix and then rank the top 10 and bottom 10 rankings from the values
# computed.

Ranking <- function(data.frame, func, typ){ # Based on the type of ranking required
  Ticker.names <- rownames(data.frame)
  ind.tmp <- 1
  rank.data <- as.data.frame(matrix(0,ncol = 3, nrow = ((length(Ticker.names)^2)-length(Ticker.names))))
  for(i in Ticker.names){
    for(j in Ticker.names[-which(Ticker.names == i)]){
        result <- func(i,j,data.frame)
        tmp <- c(i,j, result)
        rank.data[ind.tmp,] <- tmp
        ind.tmp <- ind.tmp + 1 
    }
  }
  colnames(rank.data) <- c("Tick1", "Tick2", "Measure")
  rank.data$Measure <- as.numeric(rank.data$Measure)
  top.10    <- rank.data[order(rank.data$Measure, decreasing = TRUE)[1:20],]
  bot.10    <- rank.data[order(rank.data$Measure, decreasing = FALSE)[1:20],]
  if(typ == "Top"){
    return(top.10)
  }else{
    return(bot.10)   
  }
}

Ranking(Fund.Data.Q,minkowski,"Top")
Ranking(Fund.Data.Q,minkowski,"Bot")

# g) Mahalonibis Distance

maha.dist = function(i,j, data.frame){
  if(i != j){
    tot.i = 0
    xk <- as.vector(as.numeric(data.frame[i,]))
    yk <- as.vector(as.numeric(data.frame[j,]))
    pk <- (xk-yk)^2
    sq.sum <- sum(pk)
    tot.i <- tot.i + sq.sum
    
  }
  tot.r <- (tot.i/cov(xk,yk))^(1/2)
  return(tot.r)
}

Ranking(Fund.Data.Q,maha.dist,"Top")
Ranking(Fund.Data.Q,maha.dist,"Bot")

# f) Match based similarity computation

match.sim = function(i,j, data.frame, p = 1, k = 3){
  Categories <- vector()
  for(n in 1:(nrow(data.frame)/k)){
    Categories <- c(Categories,rep(n,k))
  }
  if(i != j){
    tot.i = 0
    for (d in 1:ncol(data.frame)) {
      ni <- rownames(data.frame[i,])
      nj <- rownames(data.frame[j,])
      rank.var <- data.frame[order(data.frame[,d], decreasing = FALSE),]
      rank.var$Cat <- Categories
      xc <- rank.var$Cat[which(rownames(rank.var) == ni)]
      yc <- rank.var$Cat[which(rownames(rank.var) == nj)]
      if(xc != yc){
        tot.i = tot.i
      }else{
        xd <- data.frame[i,d]
        yd <- data.frame[j,d]
        md <- rank.var[(xc*k),d]
        nd <- rank.var[(xc*k)-(k-1),d]
        pd <- (1-(abs(xd-yd)/(md-nd)))^p
        tot.i <- tot.i + pd
      }
    }
  }
  tot.r <- tot.i^(1/p)
  return(tot.r)
}

Ranking(Fund.Data.Q,match.sim,"Top")
Ranking(Fund.Data.Q,match.sim,"Bot")

# Now let us write a function to compute similarity overlap measure. Simlarity measures are used
# for computing similarity between categorical data. The emphasis is on rewarding similare values
# and giving zero for unequal values.

# Therefore, generally higher values tend to mean higher similarity between the stocks

# h) Similarity overlap

sim.olap = function(i,j, data.frame){
      if(i != j){
      tot.i = 0
      for (k in 2:ncol(data.frame)) {
        xk <- data.frame[i,k]
        yk <- data.frame[j,k]
        if(xk == yk){
          pk <- 1
          tot.i <- tot.i + pk  
        }
      }
    }
  return(tot.i)
  }

sim.olap(1,3, Fund.Data.C)

Ranking(Fund.Data.C,sim.olap,"Top")
Ranking(Fund.Data.C,sim.olap,"Bot")

# i) Similarity Inverse

library(plyr)

sim.inv = function(i,j, data.frame){
  if(i != j){
    tot.i = 0
    for (k in 2:ncol(data.frame)) {
      xk <- data.frame[i,k]
      yk <- data.frame[j,k]
      if(xk == yk){
        freq.tab <- count(data.frame[,k])
        pk <- 1/((freq.tab$freq[which(freq.tab  == as.character(xk))]/sum(freq.tab$freq))^2)
        tot.i <- tot.i + pk  
      }
    }
  }
  return(tot.i)
}

Ranking(Fund.Data.C,sim.inv,"Top")
Ranking(Fund.Data.C,sim.inv,"Bot")

# j) Similarity Goodall

sim.good = function(i,j, data.frame){
  if(i != j){
    tot.i = 0
    for (k in 2:ncol(data.frame)) {
      xk <- data.frame[i,k]
      yk <- data.frame[j,k]
      if(xk == yk){
        freq.tab <- count(data.frame[,k])
        pk <- 1 - ((freq.tab$freq[which(freq.tab  == as.character(xk))]/sum(freq.tab$freq))^2)
        tot.i <- tot.i + pk  
      }
    }
  }
  return(tot.i)
}

Ranking(Fund.Data.C,sim.good,"Top")
Ranking(Fund.Data.C,sim.good,"Bot")

# Now we shall create overall similarity functions to compute combined similarities between
# quantiative data and categorical data. We shall compute similarity between categorical data
# with similarity overlap and for quantiative data with match based similarity. We shall
# take a standard lambda of 0.5

# k) Overall Similarity

overall.sim = function(i,j, data.frame.q, data.frame.c,func.q = match.sim, func.c  = sim.olap, lambda = 0.5){
  if(i != j){
    tot.i = 0
    result.q <- func.q(i,j,data.frame.q)
    result.c <- func.c(i,j,data.frame.c)
    tot.i = (lambda*result.q) + (1-lambda)*result.c
  }
  return(tot.i)
}

overall.sim(31,76,Fund.Data.Q,Fund.Data.C)

# We shall need a new ranking function for this with inputs for data.frame.q
# and data.frame.c

Ranking.M <- function(data.frame, data.frame.q, data.frame.c,func,typ, func.q, func.c){
  Ticker.names <- rownames(data.frame)
  ind.tmp <- 1
  rank.data <- as.data.frame(matrix(0,ncol = 3, nrow = ((length(Ticker.names)^2)-length(Ticker.names))))
  for(i in Ticker.names){
    for(j in Ticker.names[-which(Ticker.names == i)]){
      result <- func(i,j,data.frame.q, data.frame.c)
      tmp <- c(i,j, result)
      rank.data[ind.tmp,] <- tmp
      ind.tmp <- ind.tmp + 1 
    }
  }
  colnames(rank.data) <- c("Tick1", "Tick2", "Measure")
  rank.data$Measure <- as.numeric(rank.data$Measure)
  top.10    <- rank.data[order(rank.data$Measure, decreasing = TRUE)[1:20],]
  bot.10    <- rank.data[order(rank.data$Measure, decreasing = FALSE)[1:20],]
  if(typ == "Top"){
    return(top.10)
  }else{
    return(bot.10)   
  }
}

Ranking.M(Fund.Data.Q,Fund.Data.Q,Fund.Data.C,overall.sim, "Top")
Ranking.M(Fund.Data.Q,Fund.Data.Q,Fund.Data.C,overall.sim, "Bot")

# Normalalized Standard Deviation

## This function requires computing sample Standard deviation of the simialarities
## based on sample records. We shall use stock tickers 31,76 and 3,4 to compute 
## the sample standard deviations.

Sample.Rec.Q1 <- match.sim(31,76, Fund.Data.Q)
Sample.Rec.Q2 <- match.sim(3,4,Fund.Data.Q)

Q.sd <- sd(c(Sample.Rec.Q1,Sample.Rec.Q2))

Sample.Rec.C1 <- sim.olap(31,76, Fund.Data.Q)
Sample.Rec.C2 <- sim.olap(3,4,Fund.Data.Q)

C.sd <- sd(c(Sample.Rec.C1,Sample.Rec.C2))

norm.over.sim = function(i,j, data.frame.q, data.frame.c,func.q = match.sim, func.c  = sim.olap, lambda = 0.5){
  if(i != j){
    tot.i = 0
    result.q <- func.q(i,j,data.frame.q)
    result.c <- func.c(i,j,data.frame.c)
    tot.i = (lambda*result.q)/Q.sd + ((1-lambda)*result.c)/C.sd
  }
  return(tot.i)
}

norm.over.sim(31,76,Fund.Data.Q,Fund.Data.C)

Ranking.M(Fund.Data.Q,Fund.Data.Q,Fund.Data.C,norm.over.sim, "Top")
Ranking.M(Fund.Data.Q,Fund.Data.Q,Fund.Data.C,norm.over.sim, "Bot")

# It is clear from the above computations that each distance and similarity functions
# give different relationships between different stocks. Based on the purpose and 
# characteristics of each analysis we would need to use respective measures.

