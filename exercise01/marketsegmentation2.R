library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)

socialmarketing = read.csv('social_marketing.csv', header=TRUE)
summary(socialmarketing)
dim(socialmarketing)

# Center and scale the data
X = socialmarketing[,-(1:9)]
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

### finding the optimal K###
mydata <- X
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

### optimal output from above shows wither 3 or 6 for k so lets try both
clust5 = kmeanspp(X, k=5, nstart=25)
clust6 = kmeanspp(X, k=6, nstart=25)

#cluster 3 centers
clust5$center[1,]*sigma + mu
clust5$center[2,]*sigma + mu
clust5$center[3,]*sigma + mu
clust5$center[4,]*sigma + mu
clust5$center[5,]*sigma + mu

# qplot is in the ggplot2 library
qplot(news, politics,data=socialmarketing, color=factor(clust5$cluster))
qplot(family, food, data=socialmarketing, color=factor(clust5$cluster))

#checking to see sum of squares
clust5$tot.withinss
clust5$betweenss

options(scipen=999)
#cluster 6  centers
clust6$center[1,]*sigma + mu
clust6$center[2,]*sigma + mu
clust6$center[3,]*sigma + mu
clust6$center[4,]*sigma + mu
clust6$center[5,]*sigma + mu
clust6$center[6,]*sigma + mu

# qplot is in the ggplot2 library
qplot(news, politics,data=socialmarketing, color=factor(clust6$cluster))
qplot(family, food, data=socialmarketing, color=factor(clust6$cluster))

#checking to see sum of squares
clust6$tot.withinss
clust6$betweenss
