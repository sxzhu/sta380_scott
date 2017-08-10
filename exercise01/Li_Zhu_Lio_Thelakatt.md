---
title: "Exercise 1"
author: "Gaby Lio, Shirley Zhu, Jushira Thelakkat, Winnie Li"
date: "August 10, 2017"
output: html_document
---

 
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```
## Probability practice
**Part A: What fraction of people who are truthful clickers answered yes?**

* P(RC)=0.3
*	P(TC)=0.7
*	P(Y|RC)=0.5
*	P(N|RC)=0.5
*	P(Y)=0.65
*	P(N)=0.35

|   |RC|TC|Total|
|:--|--:|--:|--:|
|Y |0.15|0.5|0.65|
|N |0.15|0.2|0.35|
|Total|0.3|0.7|1|

P(Y|TC) = 0.5/0.7 = 71.43%

The fraction of people who answered yes given that they are truthful clickers is 71.43%.

**Part B: Suppose someone tests positive. What is the probability that they have the disease? In light of this calculation, do you envision any problems in implementing a universal testing policy for the disease?**

P(+|D)=0.9993

P(-|Dc)=0.9999

P(D) = 0.000025

P(Dc) = 1-0.000025=0.999975


|   |+|-|Total|
|:--|--:|--:|--:|
|D |2.49825e-05|0.00000018|0.000025|
|Dc|1e-04|0.999875|0.999975|
|Total|0.0001249825|0.999875|1|


P(+,D) = P(+|D) * P(D) = 2.49825e-05

P(-,Dc) =P(-|Dc)* P(Dc) = 0.999875

P(+,Dc) = P(Dc) - P(-,Dc) = 0.999975-0.999875 = 1e-04

P(+) = P(+,D)+P(+,Dc)=2.49825e-05+1e-04=0.0001249825

P(D|+) =P(+,D)/P(+)=2.49825e-05/0.0001249825=0.199888 = **19.99%**

The probability of that someone has the disease given that they test positive is very low, only 19.99%! If they were to implement a universal testing policy for this disease, most people who test positive will not have the disease ~ about 80.01% actually. This would cause chaos, and proves that a universal testing policy for this disease is not recommended.   

## Exploratory Analysis: Green Buildings
Looking through the stat gurus summary about green buildings we realized it was flawed in many ways. The first thing he did wrong was deciding to remove buildings that had less than 10% occupancy from the dataset. In our anlaysis we decided to keep these buildings.

We first wanted to check his claims that rent would be higher for a green building, therefore making a green building more profitable, and convincing his boss to build a green building. We created box plots of green building vs. Rent to assert these claims.
```{r, echo=TRUE}
green_data = read.csv('greenbuildings.csv')

green_data$renovated = as.factor(green_data$renovated)
green_data$class_a = as.factor(green_data$class_a)
green_data$class_b = as.factor(green_data$class_b)
green_data$green_rating = as.factor(green_data$green_rating)
green_data$LEED = as.factor(green_data$LEED)
green_data$Energystar = as.factor(green_data$Energystar)
green_data$net = as.factor(green_data$net)
green_data$amenities = as.factor(green_data$amenities)
green_data$green_rating = as.factor(green_data$green_rating)
```

```{r, echo=TRUE}
plot(green_data$green_rating, green_data$Rent, xlab='GREEN CERTIFIED', ylab='RENT', col='beige')

```

We found that having a green rating only slightly increased the amount of money you would be able to charge tenants for rent. In fact there is barely any variability between the two averages as shown in the box plots above, making the difference not statistically significant. If you go a step further and compare LEED certified vs. not and Energystar certified vs. not you see that in fact LEED energy buildings which are green certified charge a lower rent.
```{r, echo=TRUE}
# Compare RENT values vs. being LEED or ENERGYSTAR CERTIFIED 
par(mfrow = c(1,2))
plot(green_data$LEED, green_data$Rent, xlab='LEED CERTIFED', ylab='RENT', col='pink')
plot(green_data$Energystar, green_data$Rent, xlab='ENERGYSTAR CERTIFED', ylab='RENT', col='burlywood')
```

This already points in the direction of discrediting the gurus claim of a green building being able to charge tenants more for rent. Another major mistake the GURU did was that he only analyzed the data without looking at possible other confouding variables (i.e. age, stories, anemities, net, etc.). When just analyzing Rent vs. green or not green, you are not taking into account other effects variables have. We decided to run a linear regression to see, if holding all other variables constant, being a green building had a signficant impact on rent. 
```{r, echo=TRUE}
lmgreen = lm(green_data$Rent~., data=green_data)
summary(lmgreen)
```

As you can see from the output, when holding all other variables constant, having a green_rating was not significant in affecting Rent at all. Neither was a building having Energystar or LEED certifications (i.e. being green buildings). Other things that had a significant impact on rent included which cluster they belonged in, and each buildings size, age, class, net,amenities, perciption costs, heating days, gas costs, and electricity costs.


We then dived deeper into these insights by seeing if green buildings tended to have amenities thus increasing the price of rent. 
```{r, echo=TRUE}
plot(green_data$green_rating, green_data$amenities, xlab='GREEN RATING', ylab='AMENITIES')
```

As you can see from the plot above, about 70% of green buildings have amentities which means this could be influencing the higher price of rent.

We ran the same type of analysis for the variables net, class a, and class b. You can see that most green buildings are class A (about 80%) and few are Class B (about 20%). This proves that the upcharge in price is likely due to the building being class A and not green. 

```{r, echo=TRUE}
par(mfrow=c(1,2))
plot(green_data$green_rating, green_data$class_a, xlab='GREEN RATING', ylab='CLASS A')
plot(green_data$green_rating, green_data$class_b, xlab='GREEN RATING', ylab='CLASS B')
```


The same goes for the variable net. As you can see below, most green buildings do not have to pay for their own utilities, it is included in the rent costs. Thus adding another confunding variable to why Rent prices could be higher.
```{r, echo=TRUE}
plot(green_data$green_rating, green_data$net, xlab='GREEN', ylab='NET')
```

Lastly, it is told to us in the problem that the building will be 15 stories and will be new. When looking at the relative age of green buildings, they are much lower than non-green buildings.

```{r, echo=TRUE}

plot(green_data$green_rating, green_data$age, xlab='GREEN RATING', ylab='AGE', col='beige')
```

This is becuase green buildings are a newer concept, and did not exist a while ago. From the linear regression output we can see that Age is a significant variable, and if the building is newer, the rent will tend to be higher than if the building was old. Since most green buildings are newer than non-green buildings this could be another factor affecting the rent price.

Overall, there are too many confounding factors that effect the price of Rent. The guru solely basing his argument on the fact that green buildings have a higher rent is a wrong assumption, and therefore invalidates his analysis. 

He also miscalculated the premium one could charge for having a green building. Holding all other variables constant, the premium is only .07, much smaller than what the guru proposed per square foot. This means that it would take way longer than 8 years to pay off the building. This calculation although erroneous, still does not matter though becuase the variable was insignficant when other variables were used in the analysis.

Taking all other variables into account, the rent price is higher due to many other variables, and not just the fact that the building is green. If we had more data, such as which location cluster the building would fall into, we may be able to predict if the rent of the building would be higher. Since we only know that it will be new and have 15 stories, there is not much more we can predict and the developer should not listen to the gurus anlaysis.  



### Bootstrapping

### Market Segmentation

We decided to use clustering to see if we could find the different market segments for the company. As for the data pre-processing, we did not remove any variables but made sure to center and scale the variables before we ran the K-means regression. We figured that K-means would be the simpliest way to identify different segments in the market through clustering.
```{r,message=FALSE }
library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)

socialmarketing = read.csv('social_marketing.csv', header=TRUE)
dim(socialmarketing)

# Center and scale the data
X = socialmarketing[,-(1:1)]
X = scale(X, center=TRUE, scale=TRUE)
summary(X)
# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")
```

First we decided to make a plot of the sum of squares vs. the different choices of K to find the optimal K in which to use in our kmeans ++ model. Using the elbow method we found that 4-6 were the optimal numbers for K.
```{r, echo=TRUE}
### finding the optimal K###
set.seed(2)
mydata <- X
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
```

We ran a kmeans++ model using k=4, k=5, and k=6. Below you can see the outputs for each one. It turns out the using K=4 and K=5, did not give us enough clusters, and that when we used K=6 we could see distinct clusters that differed from one another.
```{r, echo=TRUE}

### optimal output from above shows wither 3 or 6 for k so lets try both
set.seed(1)
clust5 = kmeanspp(X, k=5, nstart=25)


#cluster 3 centers
clust5$center[1,]*sigma + mu #chatter, photo_sharing, not active
clust5$center[2,]*sigma + mu #cooking, fashion, beauty, chatter, photo sharing
clust5$center[3,]*sigma + mu #chatter, photo sharing, personal fitness, health nutrition
clust5$center[4,]*sigma + mu #chatter, photo sharing, parenting, sports fandom, food, family
clust5$center[5,]*sigma + mu #chatter, photo_sharing, news, politics, travel, 

#checking to see sum of squares
clust5$tot.withinss
clust5$betweenss

options(scipen=999)

set.seed(1)
clust4 = kmeanspp(X, k=4, nstart=25)
#cluster 4  centers
clust4$center[1,]*sigma + mu # food, sports fandom, religion, parenting
clust4$center[2,]*sigma + mu # not active
clust4$center[3,]*sigma + mu #travel, politics, news
clust4$center[4,]*sigma + mu #cooking, fashion, shopping, health_nutrition

#checking to see sum of squares
clust4$tot.withinss
clust4$betweenss

set.seed(1)
clust6 = kmeanspp(X, k=6, nstart=25)
#cluster 6  centers
clust6$center[1,]*sigma + mu #chatter, photo sharing
clust6$center[2,]*sigma + mu #online gaming, college universities
clust6$center[3,]*sigma + mu #health nutrition, personal fitness
clust6$center[4,]*sigma + mu # sports fandom, parenting, religion
clust6$center[5,]*sigma + mu # politics, travel, news
clust6$center[6,]*sigma + mu # fashion, cooking, beauty, photo sharing 


#checking to see sum of squares
clust6$tot.withinss
clust6$betweenss
```
Usin k=6, we found the following clusters to represent the following market segments:

1. Cluster 1
- Focused around those who just used a lot of chatter or photo sharing, and did not really focus on any specific topic when tweeting. They also did not seem to be using twitter a lot since their counts were low in every topic.

2. Cluster 2
- Focused around people who mentioned online gaming or college universities a lot. We figures that this could be a young male population consisting of 16-22 year olds who are active on twitter.

3. Cluster 3
- Focused around people who talked a lot about health nutrition and personal fitness. This customer segment could possibly be young adults or adults who are very into fitness and staying healthy, and who regulary attend the gym and eat nutrious foods. 

4. Cluster 4
- Focused around sports fandom, parenting and religion. This customer segment more liekly than not represents the parents of families who have children, therefore consisting of an older adult crowd.

5. Cluster 5
- Focused around those who mentioned politics, travel, automotive, computers and news. This customer segment probably consits of older men who are educated and probably have more money.

6. Cluster 6
- Focused on cooking, fashion, and beauty. This cusomter segment probably represents mothers or adult/young adult women.

Below we plotted some of the key variables that represent every customer segment using K=6. 
```{r, echo=TRUE}
# qplot is in the ggplot2 library
qplot(chatter, photo_sharing,data=socialmarketing, size=I(3), color=factor(clust6$cluster))
qplot(online_gaming, college_uni,data=socialmarketing, size=I(3), color=factor(clust6$cluster))
qplot(health_nutrition, personal_fitness, data=socialmarketing, size=I(3), color=factor(clust6$cluster))
qplot(religion, sports_fandom, data=socialmarketing, size=I(3), color=factor(clust6$cluster))
qplot(news, politics,data=socialmarketing, size=I(3), color=factor(clust6$cluster))
qplot(beauty, fashion,data=socialmarketing, size=I(3), color=factor(clust6$cluster))


```

