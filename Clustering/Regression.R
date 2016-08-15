setwd("college_rankings/Regression")

library(cluster)
library(RColorBrewer)
library(gplots)

# Read in the data
timesData <- read.csv("timesData_top50_2016.csv")
rownames(timesData) <- timesData$university_name

# Convert factors into numeric and drop NAs
rapply(timesData,class)
timesData$international <- as.numeric(as.character(timesData$international))
timesData$income <- as.numeric(as.character(timesData$income))
timesData$total_score <- as.numeric(gsub("-","", timesData$total_score))
timesData[is.na(timesData)] <- 0

variables <- c("teaching", "research", "citations", "international", "income")

##########################################################################################
# Create Heatmap
##########################################################################################
timesData$color <- ifelse(timesData$continent=="Asia", "#626262",
        ifelse(timesData$continent=="North America", "#ca0020",
        ifelse(timesData$continent=="Europe", "#0571b0",
        ifelse(timesData$continent=="Australia", "#d8d8d8",
        NA))))

par(cex.main=1)
heatmap.2(as.matrix(timesData[variables]),
          dendrogram='none', Colv=F, Rowv = F,
          col=brewer.pal(9,"Spectral"), 
          RowSideColors = timesData$color,
          main="Heatmap of Times Top 50 Colleges",
          trace="none", srtCol=45,
          cexRow=0.9, cexCol=0.9, margins=c(5,12))
legend(x=0.2353449, y=0.9009054, 
          legend = c("Asia", "North America", "Europe", "Australia"),
          col = c("#626262", "#ca0020", "#0571b0","#d8d8d8"),
          horiz = TRUE,
          lty= 1,
          cex=.7,
          lwd = 4)

# Set the seed to replicate the result
set.seed(1)

##########################################################################################
# Determine the optimal number of clusters
##########################################################################################

# Elbow Method
wss <- sapply(1:12, function(k){kmeans(timesData[variables], k, nstart=10 )$tot.withinss})
plot(1:12, wss, type="b", pch = 19, frame = FALSE,xlab="Number of clusters K", 
     ylab="Total within-clusters sum of squares", main="Elbow Method")
abline(v = 3, lty =2)

# Average Silhouette
sil <- rep(0, 12)
for(i in 2:12){
  km.res <- kmeans(timesData[variables], centers = i, nstart = 10)
  ss <- silhouette(km.res$cluster, dist(timesData[variables]))
  sil[i] <- mean(ss[,3])
}
# Plot theaverage silhouette width
plot(1:12, sil, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",
     main="Average Silhouette Width")
abline(v = which.max(sil), lty = 2)

panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
}

##########################################################################################
# Pairs Plot of Clusters
##########################################################################################

model <- kmeans(timesData[variables], 3, nstart=10)
cluster <- as.factor(model$cluster)
pairs(timesData[variables], col=cluster, pch = 20, 
      main="College Clusters",
      upper.panel=panel.pearson,
      cex.labels=1.2)

##########################################################################################
# Quick PCA
##########################################################################################

clusplot(timesData[variables],cluster, color=TRUE, cex=.8, shade=F, labels=2, lines=1,
         col.p="#ee7e88",
         col.txt = "#272727",
         col.clus=c("#a3d5d3", "#cebcd6", "#d7ccae"))