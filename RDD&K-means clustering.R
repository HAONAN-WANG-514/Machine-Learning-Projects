## ---------------------------------------------------------------------------
## 
## Title:       Homework 05: RDD & K-Means Clustering
## 
## ---------------------------------------------------------------------------

old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
par(mfrow=c(1,1))


########################################
#           Problem Set 1              #
########################################

library(ggplot2)
library(rdrobust)
library(rddensity)
library(rddtools)


class <- read.csv("class.csv")

###Question 1###

# explore the dataset
summary(class)

# boxplot
boxplot(class)

# histogram
ggplot(class) +
  geom_histogram(aes(x = class_size, fill = class_size < 30), color = 'black', binwidth = 1, boundary = 4) +
  geom_vline(xintercept = 30, size = 1) +
  labs(title = 'Histogram of the Rating variable', y = 'Number of Observations', x = 'class size') + 
  scale_fill_manual(name = '', values = alpha(c('red', 'blue'),0.3),
                    labels = c('Treat', 'Control')) +
  theme_bw() +
  theme(legend.position = c(.9, .9),
        legend.background = element_blank())

# scatter plot
class$color <- ifelse(class$class_size < 30, 'black', 'grey')
plot(x = class$class_size, y = class$mean_test_score, type = 'p',
     col = class$color, pch = 16, cex = 0.8, xlab = 'class size', ylab = 'mean test score')
abline(v = 30)



###Question 3###

# Step 1: make sure the rating variable and cut-point
# In this case, we use class size as rating variable.
# According to the scatter plot, we select 30 as cut-point.

# Step 2: test internal validity

## 1: According to the histogram, the distribution seems continuous.

## 2: density test
rdplotdensity(rddensity(X = class$class_size, c = 30), X = class$class_size)
summary(rddensity(X = class$class_size, c = 30))
# p-value is greater than 0.5, which indicates that the density distribution is continuous.

# Step 3: identify sharp or fuzzy rdd
class$treatment <- ifelse(class$class_size < 30, 0, 1)

plot(x = class$class_size, y = class$treatment, col = class$color,
     type = 'p', pch = 16, xlab = 'class size', ylab = 'Treatment',
     main = 'Relationship between rating variable and treatment')
abline(v = 30)
# According to the result, this belongs to sharp rdd.

# Step 4: select the best bandwidth
bw <- rdbwselect(y = class$mean_test_score, x = class$class_size, c = 30, bwselect = 'mserd', all = TRUE) 
summary(bw)
# According to the result, 3.556 is the best bandwidth for left and right of cut-point.
# And we can plot the results.CCT bandwidth in main estimation was about 3.556.
# so we restrict the x axis to (26,34).
rdplot(y=class$mean_test_score,x=class$class_size, c = 30, p = 1, x.lim=c(26, 34),title = 'RD plot - Class Data',
       x.label = 'Class Size',y.label = 'Mean Test Score')

# Step 5: estimate RD effect
fit <- rdrobust(class$mean_test_score, class$class_size, c = 30, p = 1, h = 3.556, all=TRUE)
summary(fit)
# According to the result, the p-value is lower than 0.5, which means class size will affect mean test score.

# Step 6: sensitivity test
# test sensitivity to bandwidth
class_rdd <- rdd_data(class$mean_test_score, class$class_size, cutpoint = 30)
reg_para <- rdd_reg_lm(rdd_object = class_rdd, bw = 3.556, order = 1)
reg_para
sensitivity_table <- plotSensi(reg_para, from = 1, to = 3.556, by = 1, output = c("data"), plot = TRUE, order = 1)
sensitivity_table





########################################
#           Problem Set 2              #
########################################

library(tidyverse)  # data manipulation
library(textir) # get the data from Rdata format
library(maptpx) # for the topics function
library(ggplot2)    # visualizations
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)  # fit multilpe plots in one view
library(glmnet)    


load("congress.RData")

###Question 1###

congress <- scale(as.matrix( congress109Counts/rowSums(congress109Counts) ))

km_05 <- kmeans(congress,5, nstart = 10)  
km_10 <- kmeans(congress,10, nstart = 10)  
km_15 <- kmeans(congress,15, nstart = 10)  
km_20 <- kmeans(congress,20, nstart = 10)  
km_25 <- kmeans(congress,25, nstart = 10)  

# full plots for appendix
pf1 <- fviz_cluster(km_05, data=congress) + ggtitle("k = 05")
pf2 <- fviz_cluster(km_10, data=congress) + ggtitle("k = 10")
pf3 <- fviz_cluster(km_15, data=congress) + ggtitle("k = 15")
pf4 <- fviz_cluster(km_20, data=congress) + ggtitle("k = 20")
pf5 <- fviz_cluster(km_25, data=congress) + ggtitle("k = 25")

# plots to compare
p1 <- fviz_cluster(km_05, geom = "point",  data=congress) + ggtitle("k = 05")
p2 <- fviz_cluster(km_10, geom = "point",  data=congress) + ggtitle("k = 10")
p3 <- fviz_cluster(km_15, geom = "point",  data=congress) + ggtitle("k = 15")
p4 <- fviz_cluster(km_20, geom = "point",  data=congress) + ggtitle("k = 20")
p5 <- fviz_cluster(km_25, geom = "point",  data=congress) + ggtitle("k = 25")
grid.arrange(p1, p2, p3, p4, p5, nrow = 3)



###Question 2###

AICc <- function(fit){
  df <- length(fit$centers) # K*dim
  n <- sum(fit$size)
  D <- fit$tot.withinss # deviance
  return(D + 2*df*n/(n-df-1))
}
AICc(km_05)
AICc(km_10)
AICc(km_15)
AICc(km_20)
AICc(km_25)
# 25 is the best

# Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 40.
k.max <- 40
wss <- sapply(1:k.max, 
              function(k){kmeans(congress, k, nstart=10)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlim = c(0,40),
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# 25 is the best



###Question 3###

x <- as.simple_triplet_matrix(congress109Counts)
tpcs <- topics(x, K=5*(1:5), verb=10) # it chooses 10 topics 
summary(tpcs, n=10)
# biggest BF: 10

# select the top 10 words in topic
rownames(tpcs$theta)[order(tpcs$theta[,1], decreasing=TRUE)[1:10]]
rownames(tpcs$theta)[order(tpcs$theta[,2], decreasing=TRUE)[1:10]]



###Question 4###

# tabulation
tabulation <- table(party = congress109Ideology$party, cluster = km_25$cluster)
tabulation

# percentage
round(tabulation/rowSums(tabulation), 3)*100

#find non-partisan topics

omega_df <- as.data.frame(tpcs$omega)
omega_df$name <- rownames(omega_df)

topic_all <- merge(omega_df, congress109Ideology[,1:2], by = 'name')
topic_R <- topic_all[(which(topic_all$party == 'R')),]
topic_D <- topic_all[(which(topic_all$party == 'D')),]
topic_I <- topic_all[(which(topic_all$party == 'I')),]

R <- colSums(topic_R[,2:11])
D <- colSums(topic_D[,2:11])
I <- colSums(topic_I[,2:11])

topics_final <- round(rbind(R,D,I),3)
colnames(topics_final) <- c("Topic -1","Topic -2","Topic -3","Topic -4","Topic -5","Topic -6","Topic -7","Topic -8","Topic -9","Topic -10")

topics_party <- round(topics_final,0)
topics_party


# fit topic regression on repshare for each party

omega_all <- cbind(as.data.frame(tpcs$omega), congress109Ideology$party)

## Republicans
set.seed(123)
omega_r <- omega_all[which(omega_all$`congress109Ideology$party`=='R'),]
omega_r <- omega_r[,-11]
repshare_r <- congress109Ideology[which(congress109Ideology$party=='R'),'repshare']

tpcreg_r <- gamlr(omega_r, repshare_r)

# repshare up or down for moving up 10% weight in that topic
drop(coef(tpcreg_r))*0.1

omega_r <- as.matrix(omega_r)
regtopics.cv_r <- cv.glmnet(omega_r, repshare_r)
min(regtopics.cv_r$cvm)

# give it the word %s as inputs
x_r <- 100*congress109Counts[which(congress109Ideology$party=='R'),]/rowSums(congress109Counts[which(congress109Ideology$party=='R'),])
regwords.cv_r <- cv.glmnet(x_r, repshare_r)
min(regwords.cv_r$cvm)

par(mfrow=c(1,2))
plot(regtopics.cv_r)
mtext("topic regression for Republicans", font=2, line=2)
plot(regwords.cv_r)
mtext("bigram regression for Republican", font=2, line=2)

# max OOS R^2s
max(1-regtopics.cv_r$cvm/regtopics.cv_r$cvm[1])
max(1-regwords.cv_r$cvm/regwords.cv_r$cvm[1])
# topic regression is better


## Democrats
set.seed(123)
omega_d <- omega_all[which(omega_all$`congress109Ideology$party`=='D'),]
omega_d <- omega_d[,-11]
repshare_d <- congress109Ideology[which(congress109Ideology$party=='D'),'repshare']

tpcreg_d <- gamlr(omega_d, repshare_d)

# repshare up or down for moving up 10% weight in that topic
drop(coef(tpcreg_d))*0.1

omega_d <- as.matrix(omega_d)
regtopics.cv_d <- cv.glmnet(omega_d, repshare_d)
min(regtopics.cv_d$cvm)

# give it the word %s as inputs
x_d <- 100*congress109Counts[which(congress109Ideology$party=='D'),]/rowSums(congress109Counts[which(congress109Ideology$party=='D'),])
regwords.cv_d <- cv.glmnet(x_d, repshare_d)
min(regwords.cv_d$cvm)

par(mfrow=c(1,2))
plot(regtopics.cv_d)
mtext("topic regression for Democrats", font=2, line=2)
plot(regwords.cv_d)
mtext("bigram regression for Democrats", font=2, line=2)

# max OOS R^2s
max(1-regtopics.cv_d$cvm/regtopics.cv_d$cvm[1])
max(1-regwords.cv_d$cvm/regwords.cv_d$cvm[1])
# topic regression is better


























