mydata1 = Ionosphere[,1:34]
set.seed(100)
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
  for(i1 in 1:50)
  {
    hashfuncs[[i1]] <- (hash1)
  }
  
  M <- matrix(Inf,nrow = 50,ncol = nrow(mydata1))
  
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
t <- 0.95
b <- 20
i<-1
j<-0
cou <- 0
cou1 <- 0
r3 <- as.integer((nrow(mydata1)/b))
list1 <- list()
el1 <- c(-99,-99)
l3 <- split(cols, as.integer((seq_along(cols) - 1) / b))
for(i in 1:(r3+1))
{
  M5 <- combn(l3[[i]],2)
  len2 <- ncol(combn(l3[[i]],2))
  for(j in 1:len2)
  {
      u1 <- M5[1,j]
      u2 <- M5[2,j]
      Js <- ((length(intersect(M4[u1,],M4[u2,])))/(length(union(M4[u1,],M4[u2,]))))
      cou1 <- cou1 + 1
      if(Js >= t)
      {
        #cat("Jaccard Similarity\n")
        #print(Js)
        Prob <- (1 - (1 - (Js**r3))**b)
        #cat("Probability that they are candidate pairs\n")
        #print(Prob)
        cou <- cou +1
        list1 <- append(u1,u2)
      }
    
  }
  el1 <- rbind(el1,unlist(list1))
  list1 <- list()
  Prob <- 0
  Js <- 0
}
el<-el1[-1,]
print(el)
library(igraph)
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

library(flexclust)
randIndex(pred, Ionosphere$Class,correct = T,original = F)
table(Ionosphere$Class,pred)