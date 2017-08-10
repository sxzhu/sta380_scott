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
set.seed(1)
mydata <- X
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

### optimal output from above shows wither 3 or 6 for k so lets try both
set.seed(1)
clust5 = kmeanspp(X, k=5, nstart=25)


#cluster 3 centers
clust5$center[1,]*sigma + mu
clust5$center[2,]*sigma + mu
clust5$center[3,]*sigma + mu
clust5$center[4,]*sigma + mu
clust5$center[5,]*sigma + mu

# 5 segments to customers
# cluster 1: college_uni, online_gaming, and sports_playing have high counts while all other counts are  1.5 and below.
# cluster 2: shopping, health_nutrition, news while all other counts are below 1 (mothers, women)
# cluster 3: cooking, health_nutrition, fashion, beauty, music, college university, food, personal fitness, shopping (young, women)
# cluster 4: food, family, parenting, religion, school (people with families, conservative)
# cluster 5: food, cooking, health nutrition, personal fitness (people really into fitness and eating right)

# qplot is in the ggplot2 library
qplot(college_uni, online_gaming,data=socialmarketing, size=I(3), color=factor(clust5$cluster))
qplot(health_nutrition, news,data=socialmarketing, size=I(3), color=factor(clust5$cluster))
qplot(beauty, fashion,data=socialmarketing, size=I(3), color=factor(clust5$cluster))
qplot(family, food, data=socialmarketing, size=I(3), color=factor(clust5$cluster))
qplot(health_nutrition, personal_fitness, data=socialmarketing, size=I(3), color=factor(clust5$cluster))

#checking to see sum of squares
clust5$tot.withinss
clust5$betweenss

options(scipen=999)

set.seed(1)
clust4 = kmeanspp(X, k=4, nstart=25)
#cluster 4  centers
clust4$center[1,]*sigma + mu
clust4$center[2,]*sigma + mu
clust4$center[3,]*sigma + mu
clust4$center[4,]*sigma + mu

# cluster 1: college_uni, news, online_gaming, and health_nutrition, shopping have high counts while all other counts are  1.5 and below.
# cluster 2: beauty and fashion, cooking, health nutrition
# cluster 3: food, family, parenting, religion
# cluster 4: health_nutrition, cooking, food, outdoors, personal fitness

# qplot is in the ggplot2 library
qplot(college_uni, shopping,data=socialmarketing, color=factor(clust4$cluster))
qplot(beauty, fashion,data=socialmarketing, color=factor(clust4$cluster))
qplot(family, food, data=socialmarketing, color=factor(clust4$cluster))

#checking to see sum of squares
clust4$tot.withinss
clust4$betweenss

set.seed(1)
clust6 = kmeanspp(X, k=6, nstart=25)
#cluster 4  centers
clust6$center[1,]*sigma + mu
clust6$center[2,]*sigma + mu
clust6$center[3,]*sigma + mu
clust6$center[4,]*sigma + mu
clust6$center[5,]*sigma + mu
clust6$center[6,]*sigma + mu


# cluster 1:news, health_nutrition, shopping have high counts while all other counts are  1.5 and below.
# cluster 2: food, health_nitrition, cooking, personal fitness
# cluster 3: health_nutrition, college_university, cooking,
# cluster 4: food, religion, parenting, family
# cluster 5: college_uni, online_gaming, sports playing
# cluster 6: fashion, beauty, cooking, health nutrition


# qplot is in the ggplot2 library
qplot(news, health_nutrition,data=socialmarketing, color=factor(clust6$cluster), size=I(3))
qplot(personal_fitness, health_nutrition,data=socialmarketing, color=factor(clust6$cluster), size=I(3))
qplot(health_nutrition, cooking, data=socialmarketing, color=factor(clust6$cluster), size=I(3))
qplot(food, religion, data=socialmarketing, color=factor(clust6$cluster), size=I(3))
qplot(college_uni, online_gaming, data=socialmarketing, color=factor(clust6$cluster), size=I(3))
qplot(fashion, beauty, data=socialmarketing, color=factor(clust6$cluster), size=I(3))

#checking to see sum of squares
clust6$tot.withinss
clust6$betweenss
