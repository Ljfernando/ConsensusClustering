library(magrittr)
library(d3heatmap)
library(ggplot2)

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
               main = "Clustered PC1 and PC2", subtitle = "Highlight and double-click to zoom")
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
  # Normalized pairs divides all entries by number of iterations
  norm_pairs <- matrix(0, nrow = n, ncol = n, dimnames = list(rownames(X), rownames(X)))
  # Regular pairs just counts number of times clustered together
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
      
      pairs[i,j] <- aggr
      norm_pairs[i,j] <- aggr/ denom
    }
  }
  
  # Clustering consensus matrix
  dist <- as.dist(1-norm_pairs)
  hier.out <- hclust(dist, method = "ward.D2")
  hier.clust <- cutree(hier.out, k = K)
  # normalized matrix reordered
  reorder.npairs <- norm_pairs[order(hier.clust), order(hier.clust)]
  # regular, counting matrix reordered
  reorder.pairs <- pairs[order(hier.clust), order(hier.clust)]
  
  return(list(norm.mat = reorder.npairs,
              clusterings = hier.clust,
              count.mat = reorder.pairs))
}

computeClustCons <- function(clusters, cons.mat){
  num_clust <- unique(clusters) %>% length()
  consensus_metrics <- rep(0, num_clust)
  for(i in 1:num_clust){
    clust_names <- names(clusters)[which(clusters == i)] # names of obs in current cluster
    mat_idxs <- which(colnames(cons.mat) %in% clust_names) # matrix indeces of current 
    clust.mat <- cons.mat[mat_idxs, mat_idxs]
    # Grabbing upper-triangle entries, excluding diagonal
    up.t <- clust.mat[upper.tri(clust.mat, diag = FALSE)]
    if(length(up.t) <= 1){
      consensus_metrics[i] <- 0
    }else{
      consensus_metrics[i] <- mean(up.t)
    }
  }

    
  return(ggplot() + 
           geom_bar(mapping = aes(x = as.factor(1:num_clust),
                                  y = consensus_metrics),
                    fill = "steelblue",
                    stat = "identity") +
           theme_light() +
           labs(y = "Cluster Consensus", x = "Cluster Number", 
                  title = "Cluster Consensus For Each Cluster Grouping")) 
}

computeItemCons <- function(clusters, cons.mat){
  obs_names <- colnames(cons.mat)
  item_cons <- rep(0, length(obs_names))
  
  num_clust <- unique(clusters) %>% length()
  for(i in 1:num_clust){
    clust_names <- names(clusters)[which(clusters == i)] # names of obs in current cluster
    mat_idxs <- which(colnames(cons.mat) %in% clust_names) # matrix indeces of current 
    clust.mat <- cons.mat[mat_idxs, mat_idxs]
    
    if(length(clust.mat) <=1){
      item_cons[which(obs_names %in% clust_names)] <- 0
    }else{
      # zeroing matrix
      diag(clust.mat) <- 0
      for(col in 1:ncol(clust.mat)){
        curr_name <- colnames(clust.mat)[col]
        curr_mean <- clust.mat[,col] %>% mean()
        item_cons[which(obs_names == curr_name)] <- curr_mean
      }
    }
  }
  
  out.df <- data.frame(Songs = obs_names,
                       Item_Consensus = item_cons)
  return(out.df[order(out.df$Item_Consensus, decreasing = TRUE),])
}

computeDistribution <- function(cons.mat){
  upper_t <- cons.mat[upper.tri(cons.mat, diag = FALSE)] %>% unlist()
  return(ggplot() + 
           geom_density(mapping=aes(upper_t), fill = "steelblue") + 
           theme_light() + 
           labs(x = "Consensus Index Value", y = "Density", 
                title = "Consensus Distribution")) 
}



