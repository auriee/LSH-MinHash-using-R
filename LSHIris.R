library(mlbench)
library(igraph)
library(flexclust)
mydata1 = iris[,-5]
set.seed(10)
print(is.data.frame(mydata1))
print(ncol(mydata1))
print(nrow(mydata1))

result1 <- t(mydata1)
mode(result1) <- "numeric"
print(result1)
cols <- c(1:nrow(mydata1))
rows <- ncol(mydata1)

normalize <- function(result1, cols) {
  result <- result1
  
  for (j in cols) { 
    m1 <- min(result1[,j]) 
    m2 <- max(result1[,j]) 
    
    for (i in 1:nrow(result)) { 
      result[i,j] = (result[i,j] - m1) / (m2-m1)
    }
  }
  return(result)
}
result1 <- normalize(result1,cols)
print(result1, digits = 3) 

result2<-ifelse(result1 > median(result1), 1,0)


M2 <- as.matrix(result2)
print(M2)
hash1 <- function(x)
{
  for(h1 in 1:20)
  {
    a <- sample(1:20,1)
    b <- sample(1:50,1)
    p <- 1912312442
    return ((a*x+b) %% p)
  }
}
minhash <- function(M2)
{
  hashfuncs = list()
  for(i1 in 1:30)
  {
    hashfuncs[[i1]] <- (hash1)
  }
  
  M <- matrix(Inf,nrow = 30,ncol = nrow(mydata1))
  
  rows1 <- nrow(result2)
  cols1 <- ncol(result2)
  sigrows <- length(hashfuncs)
  for(r in 1:rows1)
  {
    hashvalue <- lapply(hashfuncs, function(x) { return (x(r)) })
    for(c1 in 1:cols1)
    {
      if(M2[r,c1] == 0)
      {
        next
      }
      for(v in 1:sigrows)
      {
        if(M[v,c1] > hashvalue[[v]])
        {
          M[v,c1] <- hashvalue[[v]]
        }
      }
    }
    
  }
  return (M)
}
cat("\nSignature matrix formed:\n")
M3 <- minhash(M2)
M4 <- t(M3)
print(M4)
t <- 0.8
b <- 20
i<-1
j<-0
cou <- 0
cou1 <- 0
r3 <- as.integer((nrow(mydata1)/b))
list1 <- list()
el1 <- c(-99,-99)
l3 <- split(cols, as.integer((seq_along(cols) - 1) / b))
for(i in 1:nrow(mydata1))
{
  for(j in (i+1):nrow(mydata1))
  {
    if(j<=nrow(mydata1))
    {
      Js <- ((length(intersect(M4[i,],M4[j,])))/(length(union(M4[i,],M4[j,]))))
      cou1 <- cou1 + 1
      if(Js >= t)
      {
        #cat("Jaccard Similarity\n")
        #print(Js)
        Prob <- (1 - (1 - (Js**r3))**b)
        #cat("Probability that they are candidate pairs\n")
        #print(Prob)
        cou <- cou +1
        list1 <- append(i,j)
      }
    }
  }
  el1 <- rbind(el1,unlist(list1))
  list1 <- list()
  Prob <- 0
  Js <- 0
}
el<-el1[-1,]
print(el)

g1 <- graph_from_edgelist(el,directed = FALSE)
print(g1)
V(g1)$color = as.factor(Ionosphere$Class)
plot(g1,vertex.label=NA,vertex.size=5)
clstur <- cluster_louvain(g1)
print(clstur)
pred <- rep(0,nrow(mydata1))
for(y in 1:length(clstur))
{
  pred[clstur[[y]]] = y
}

randIndex(pred, iris$Species,correct = T,original = F)
table(iris$Species,pred)
