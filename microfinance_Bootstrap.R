#####Group: Ana Parra Vera, Danfeng Cao, Haonan Wang, Rajasa Kakkera#####


## microfinancenance network 
## data from BANERJEE, CHANDRASEKHAR, DUFLO, JACKSON 2012

## data on 8622 households
#setwd("~/Desktop/452 Machine Learning/Homework 4")
hh <- read.csv("microfinance_households.csv", row.names="hh")
hh$village <- factor(hh$village)

## We'll kick off with a bunch of network stuff.
## This will be covered in more detail in later lectures.
## Get igraph off of CRAN if you don't have it
## install.packages("igraph")
## this is a tool for network analysis
## (see http://igraph.sourceforge.net/)
library(igraph)
edges <- read.table("microfinance_edges.txt", colClasses="character")
## edges holds connections between the household ids
hhnet <- graph.edgelist(as.matrix(edges))
hhnet <- as.undirected(hhnet) # two-way connections.

## igraph is all about plotting.  
V(hhnet) ## our 8000+ household vertices
## Each vertex (node) has some attributes, and we can add more.
V(hhnet)$village <- as.character(hh[V(hhnet),'village'])
## we'll color them by village membership
vilcol <- rainbow(nlevels(hh$village))
names(vilcol) <- levels(hh$village)
V(hhnet)$color = vilcol[V(hhnet)$village]
## drop HH labels from plot
V(hhnet)$label=NA

# Graph plots try to force distances proportional to connectivity
# Imagine nodes connected by elastic bands that you are pulling apart
# The graphs can take a very long time, but I've found
# edge.curved=FALSE speeds things up a lot.  Not sure why.

## we'll use induced.subgraph and plot a couple villages 
village1 <- induced.subgraph(hhnet, v=which(V(hhnet)$village=="1"))
village33 <- induced.subgraph(hhnet, v=which(V(hhnet)$village=="33"))

# vertex.size=3 is small.  default is 15
plot(village1, vertex.size=3, edge.curved=FALSE)
plot(village33, vertex.size=3, edge.curved=FALSE)

######  now, on to the HW

## match id's
matches <- match(rownames(hh), V(hhnet)$name)

## calculate the 'degree' of each hh: 
##number of commerce/friend/family connections
degree <- degree(hhnet)[matches]
names(degree) <- rownames(hh)
degree[is.na(degree)] <- 0 # unconnected houses, not in our graph

## if you run a full glm, it takes forever and is an overfit mess
# > summary(full <- glm(loan ~ degree + .^2, data=hh, family="binomial"))
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 


###Question 1###
#I??d transform degree to create our treatment variable d. What would you do and why?#

min(degree)
max(degree)
degree_df <- as.data.frame(degree)
n <- nrow(degree_df)

hist(degree_df$degree)

# The distribution of degree is skewed, so we transform degree into log(1+degree).
for (i in 1:8622) {
  degree_df$d[i] <- log(1+degree_df$degree[i])
}

hist(degree_df$d)

###Question 2###
#Build a model to predict d from x, our controls. #
#Please interact all controls with one another (~.^2). Use lasso + CV. #
#Comment on how tight the fit is, and what that implies for estimation of a treatment effect.#

library(glmnet)

# Create a vector about lambda
grid = 10^seq(-2, 10, 1)

# create interation term
f <- as.formula(~ .+.*.)
x <- hh[,-1]
x_m <- model.matrix(f, x)[,-1]

# split data into training set and test set
set.seed(100)
sam <- sample(1:n, 0.8*n)
x_train <- data.matrix(x_m[sam,])
y_train <- degree_df[sam,]$d
x_test <- data.matrix(x_m[-sam,])
y_test <- degree_df[-sam,]$d

# run lasso model with cross validation, find best lambda
lasso_mod <- glmnet(x_train, y_train, alpha = 1, lambda = grid)
cv <- cv.glmnet(x_train, y_train, alpha = 1, type.measure="mse")
plot(cv)
bestlam <- cv$lambda.min
bestlam #0.01259388

# use best lambda to predict test data and calculate MSE and r-square
lasso_pred <- predict(lasso_mod, s = bestlam, newx = x_test)
mse <- mean((lasso_pred - y_test)^2)
mse # 0.7052198
sse.test <- sum((lasso_pred - y_test)^2)		
sst.test <- sum((y_test-mean(y_test))^2)	
r2 <- 1-sse.test/sst.test
r2 # 0.07107616

# fit lasso model on full dataset and predict dhat
lasso_full <- glmnet(data.matrix(x_m), degree_df$d, alpha = 1, lambda = grid)
lasso_coef <- coef(lasso_full, s = bestlam)
lasso_coef[lasso_coef != 0]
dhat <- predict(lasso_full, s = bestlam, newx = x_m)


###Question 3###
#Use predictions from [2] in an estimator for effect of d on loan.#

library(gamlr)
d <- degree_df$d
y_loan <- hh$loan
loan <- gamlr(cbind(d, dhat, x_m), y_loan, free = 2)
coef(loan)["d",]


###Question 4###
#Compare the results from [3] to those from a naive lasso (no CV) for loan on d and x. #
#Explain why they are similar or different.#

naive <- gamlr(cbind(d, x_m), y_loan)
coef(naive)["d",]


###Question 5###
#Bootstrap your estimator from [3] and describe the uncertainty.#

set.seed(100)
ds <- c()
for (b in 1:100) {
  boot <- sample(1:n, n, replace=TRUE)
  d_boot <- d[boot]
  dhat_boot <- dhat[boot]
  boot_loan <- gamlr(cbind(d_boot, dhat_boot, x_m[boot,]), y_loan[boot], free = 2)
  ds <- c(ds, coef(boot_loan)["d_boot",])
}

hist(ds)
abline(v=coef(loan)["d",])

summary(ds)
sd(ds)
left <- quantile(ds,0.05)
right <- quantile(ds,0.95)
sprintf("The 90 percent CI for the angle of the ideal vector is (%.2f, %.2f).", left, right)

