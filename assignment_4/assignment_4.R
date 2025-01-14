library("cluster")
library("dbscan")

rmf <- readRDS("rmf.rds")
head(rmf)

rmf_pca <- princomp(scale(rmf))

sds <- rmf_pca$sdev
pve <- cumsum(sds^2) / sum(sds^2)
barplot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained")
abline(h = 0.95, lwd = 2, col = "red")

biplot(rmf_pca)

tmp <- (scale(rmf) %*% loadings(rmf_pca))[, 1:3]
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(tmp[, 1:2], asp = 1)
plot(tmp[, c(1, 3)], asp = 1)
plot(tmp[, 2:3], asp = 1)
par(opar)

kmcs <- lapply(ks <- 1:9, \(k) kmeans(rmf, nstart = 10, centers = k))
ss <- unlist(lapply(kmcs, \(obj) obj$tot.withinss))
plot(ks, ss, type = "b")

rmf_kmc <- kmeans(rmf, centers = 4, nstart = 10)
scores <- rmf_pca$scores[, 1:2]

plot(scores, pch = 20, xlim = 1.5 * range(scores[, 1]),
     ylim = 1.3 * range(scores[, 2]), col = rmf_kmc$cluster, asp = 1,
     main = "k-means clustering (4 clusters)")
text(scores, labels = rownames(rmf), adj = c(-0.1, 1), cex = 0.5,
     col = rmf_kmc$cluster)

rmf_dist <- dist(rmf, method = "euclidean")
rmf_clust <- hclust(rmf_dist)

single <- hclust(rmf_dist, method = "single")
clust_single <- cutree(single, k = 4)

plot(scores, pch = 20, xlim = 1.5 * range(scores[, 1]),
     ylim = 1.3 * range(scores[, 2]), col = clust_single, asp = 1,
     main = "Single Linkage Clustering")
text(scores, labels = rownames(rmf), adj = c(-0.1, 1), cex = 0.5, col = clust_single)

complete <- hclust(dist(rmf), method = "complete")
clust_complete <- cutree(complete, k = 4)

plot(scores, pch = 20, xlim = 1.5 * range(scores[, 1]),
     ylim = 1.3 * range(scores[, 2]), col = clust_complete, asp = 1,
     main = "Complete Linkage Clustering")
text(scores, labels = rownames(rmf), adj = c(-0.1, 1), cex = 0.5, col = clust_complete)

rmf_dbs <- dbscan(scale(rmf), eps = 0.5, minPts = 5)

plot(scores, pch = 20, xlim = 1.5 * range(scores[, 1]), asp = 1,
     ylim = 1.3 * range(scores[, 2]), col = 1 + rmf_dbs$cluster,
     main = "DBSCAN")

kmeans_result <- kmeans(rmf, centers = 4)
rmf$cluster <- kmeans_result$cluster

avg_recency <- numeric(4)
avg_frequency <- numeric(4)
avg_monetary <- numeric(4)
cluster_size <- numeric(4)

for (i in 1:4) {
  cluster_data <- rmf[rmf$cluster == i, ] 
  avg_recency[i] <- mean(cluster_data$Recency, na.rm = TRUE)
  avg_frequency[i] <- mean(cluster_data$Frequency, na.rm = TRUE)
  avg_monetary[i] <- mean(cluster_data$Monetary, na.rm = TRUE)
  cluster_size[i] <- nrow(cluster_data)
}

for (i in 1:4) {
  cat("\nCluster", i, "Summary:\n")
  cat("Average Recency:", avg_recency[i], "\n")
  cat("Average Frequency:", avg_frequency[i], "\n")
  cat("Average Monetary Value:", avg_monetary[i], "\n")
  cat("Number of Customers:", cluster_size[i], "\n")
}