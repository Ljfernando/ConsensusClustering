library(magrittr)
library(d3heatmap)

library(kmed) # k-medoids
library(kernlab) # spectral clustering
library(amap) # Kmeans

library(factoextra) #fviz_cluster


consKmeans <- function(X, K, method){
  out.clust <- rep(0, nrow(X))
  subs <- sample(x = 1:nrow(X), size = 0.8*nrow(X) %>% round(), replace = FALSE)
  kmeans.out <- Kmeans(X[subs,], centers = K, iter.max = 100,
                       nstart = 5, method = method)
  kmeans.clust <- kmeans.out$cluster
  
  out.clust[subs] <- kmeans.clust
  return(out.clust)
}
consHclust <- function(X, K, method, linkage){
  out.clust <- rep(0, nrow(X))
  subs <- sample(x = 1:nrow(X), size = 0.8*nrow(X) %>% round(), replace = FALSE)
  
  dist.mat <- dist(X[subs,], method = method)
  hier.out <- hclust(dist.mat, method = linkage)
  hier.clust <- cutree(hier.out, k = K)
  
  out.clust[subs] <- hier.clust
  return(out.clust)
}

consKmed <- function(X, K, method){
  out.clust <- rep(0, nrow(X))
  subs <- sample(x = 1:nrow(X), size = 0.8*nrow(X) %>% round(), replace = FALSE)
  
  dist.mat <- dist(X[subs, ], method = method)
  kmed.out <- fastkmed(dist.mat, ncluster = K, iterate = 50)
  kmed.clust <- kmed.out$cluster
  
  out.clust[subs] <- kmed.clust
  return(out.clust)
}

consSpec <- function(X, K){
  out.clust <- rep(0, nrow(X))
  subs <- sample(x = 1:nrow(X), size = 0.8*nrow(X) %>% round(), replace = FALSE)
  spec.out <- specc(x = X[subs,], centers = K,
                    iterations = 50)
  spec.clust <- spec.out@.Data
  
  out.clust[subs] <- spec.clust
  return(out.clust)
}

# Generates a 4x4 scatterplot matrix of the
# first four PCs of a numeric matrix and colors
# points by cluster assignment
genPCPlots <- function(mat, cluster){
  pr.out <- prcomp(mat)
  out <- data.frame(pr.out$x[,1:4], cluster = as.factor(cluster))
  pairs(pr.out$x[,1:4], col = unlist(cluster))
}

# Generates a biplot for the first two
# PCs and the cluster assignments for
# all points
genBiPCPlot <- function(mat, cluster){
  fviz_cluster(object = list(data = mat, 
                             cluster = cluster),
               main = "First Two PCs and Cluster Assignments")
}


consClustering <- function(X, K, func, nrs, ...){
  df <- numeric(0)
  for(i in 1:nrs){
    df <- switch(func,
                 kmeans = cbind(df, consKmeans(X, K, ...)),
                 hier = cbind(df, consHclust(X, K, ...)),
                 kmed = cbind(df, consKmed(X, K, ...)),
                 spec = cbind(df, consSpec(X, K)))
  }
  
  n <- nrow(X)
  pairs <- matrix(0, nrow = n, ncol = n, dimnames = list(rownames(X), rownames(X)))
  for (i in 1:n) {
    for (j in 1:n) {
      
      # Looking at two observations and their cluster assignments
      pair <- df[c(i,j), df[i,]!=0 & df[j,]!=0, drop = FALSE]
      
      # Total times the two obs were clustered similarly in the same iter
      aggr <- sum(pair[1,] == pair[2,])
      
      # Total times both obs existed in iter
      denom <- ncol(pair)
      
      # If neither of them appeared in iter together
      if(denom == 0) denom <- 1
      pairs[i,j] <- aggr/ denom
    }
  }
  
  dist <- as.dist(1-pairs)
  hier.out <- hclust(dist, method = "ward.D2")
  hier.clust <- cutree(hier.out, k = K)
  reorder.pairs <- pairs[order(hier.clust), order(hier.clust)]
  return(list(cons.mat = reorder.pairs,
              clusterings = hier.clust))
}

# 
# genBiPCPlot(dataset, out$clusterings)
# clustheatmap(out$cons.mat)
