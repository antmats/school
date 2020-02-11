# setwd("/Users/antonmatsson/Desktop/Project/")

# Load libraries
library(tibble)
library(arm)
library(corrplot)
library(car)
source('utils.R')

cardata <- read.csv("CarPrice.csv", header = TRUE, sep = ",")

########## ########## ########## ########## ##########

data <- subset(cardata, select = -c(car_ID, enginelocation))

# Add covariates country and continent
carnames <- lapply(data$CarName, as.character)

data <- add_column(data, car = rep("", nrow(cardata)), .after = "CarName")
data <- add_column(data, country = rep("", nrow(cardata)), .after = "car")
data <- add_column(data, continent = rep("", nrow(cardata)), .after = "country")
data <- country_continent(carnames, data)

data$car <- as.factor(data$car)
data$country <- as.factor(data$country)
data$continent <- as.factor(data$continent)

########## ########## ########## ########## ##########

par(mfrow=c(1, 2))
hist(data$price, xlab = "Price [USD]", ylab = "Frequency", main = "")
hist(log(data$price), xlab = "log(Price [USD])", ylab = "Frequency", main = "")

# What cateogorical covariates could be interesting?
# boxplot(log(price)~symboling, data, varwidth=TRUE)  # quadratic -> deviation from "1"?
# boxplot(log(price)~continent, data, varwidth=TRUE)  # useful
boxplot(log(price)~country, data, varwidth=TRUE)  # useful
# boxplot(price~car, data, varwidth=TRUE)  # too many categories
boxplot(price~fueltype, data, varwidth=TRUE)  # not so useful (?)
# boxplot(log(price)~aspiration, data, varwidth=TRUE)  # (insug) useful (?)
# boxplot(price~doornumber, data, varwidth=TRUE)  # useless (?)
boxplot(log(price)~carbody, data, varwidth=TRUE)  # useful (related to car size)
boxplot(log(price)~drivewheel, data, varwidth=TRUE)  # useful (?)
boxplot(log(price)~enginetype, data, varwidth=TRUE)  # quite useful but perhaps too many categories
boxplot(log(price)~cylindernumber, data, varwidth=TRUE)  # useful (related to horsepower)
# boxplot(log(price)~fuelsystem, data, varwidth=TRUE)  # useful (?)

# Look on "cylindernumber" in more detail
three_cylinders <- subset(data, cylindernumber == "three")
twelve_cylinders <- subset(data, cylindernumber == "twelve")

cylinders <- data[data$cylindernumber == "eight" | 
                  data$cylindernumber == "five" |
                  data$cylindernumber == "four" |
                  data$cylindernumber == "six" |
                  data$cylindernumber == "two",]
cylinders$cylindernumber <- droplevels(cylinders$cylindernumber)
boxplot(log(price)~cylindernumber, cylinders, varwidth=TRUE)

# Best: country, aspiration, carbody, drivewheel, cylindernumber

# What numeric covariates could be interesting?
plot(data$wheelbase, log(data$price))  # (hjulbas) useful
plot(data$carlength, log(data$price))  # useful but trend seems quadratic (not so much with log(price))
plot(data$carwidth, log(data$price))  # useful
# plot(data$carheight, log(data$price))  # useless
plot(data$curbweight, log(data$price))  # useful
plot(data$enginesize, log(data$price))  # useful (log-log looks best)
# plot(data$boreratio, log(data$price))  # not the best
# plot(data$stroke, log(data$price))  # not the best
# plot(data$compressionratio, log(data$price))  # useless
plot(log(data$horsepower), log(data$price))  # useful (log-log looks nice)
# plot(data$peakrpm, log(data$price))  # useless (?)
plot(data$citympg, log(data$price))  # useful
plot(data$highwaympg, log(data$price))  # useful

# Best: wheelbase, carlength, carwidth, curbweight, enginesize, horsepower, citympg, highwaympg

par(mfrow=c(2, 2))
plot(data$wheelbase, log(data$price))  # (hjulbas) useful
plot(data$carlength, log(data$price))  # useful but trend seems quadratic (not so much with log(price))
plot(data$carwidth, log(data$price))  # useful
plot(data$curbweight, log(data$price))  # useful

par(mfrow=c(2, 2))
plot(log(data$enginesize), log(data$price))
plot(log(data$horsepower), log(data$price))
plot(1/data$citympg, log(data$price))
plot(1/data$highwaympg, log(data$price))

########## ########## ########## ########## ##########

# Look for multivariate relationships
pairs(~carlength + carwidth + enginesize + horsepower, data = data)

par(pty="s")
plot(data$enginesize, data$horsepower, xlab="Engine size", ylab="Horsepower")

# Create new data frame where the mean is subtracted from the interesting numeric covariates
avg.data <- data.frame(data)
avg.data$wheelbase <- avg.data$wheelbase - mean(avg.data$wheelbase)
avg.data$carlength <- avg.data$carlength - mean(avg.data$carlength)
avg.data$carwidth <- avg.data$carwidth - mean(avg.data$carwidth)
avg.data$curbweight <- avg.data$curbweight - mean(avg.data$curbweight)
avg.data$enginesize <- avg.data$enginesize - mean(avg.data$enginesize)
avg.data$horsepower <- avg.data$horsepower - mean(avg.data$horsepower)
avg.data$citympg <- avg.data$citympg - mean(avg.data$citympg)
avg.data$highwaympg <- avg.data$highwaympg - mean(avg.data$highwaympg)

# Using all useful numeric covariate
mm1 <- lm(log(price) ~ wheelbase + carlength + carwidth + curbweight + enginesize + 
           horsepower + citympg + highwaympg, data = data, x = T)
summary(mm1)
vif(mm1)

# Removing citympg
mm2 <- lm(log(price) ~ wheelbase + carlength + carwidth + curbweight + enginesize + 
            horsepower + highwaympg, data = data, x = T)
# summary(mm2)
vif(mm2)

# Removing curbweight
mm3 <- lm(log(price) ~ wheelbase + carlength + carwidth + enginesize + horsepower +
          highwaympg, data = data, x = T)
# summary(mm3)
vif(mm3)

# Removing wheelbase
mm4 <- lm(log(price) ~ carlength + carwidth + enginesize + horsepower + 
            highwaympg, data = data, x = T)
# summary(mm4)
vif(mm4)

########## ########## ########## ########## ##########

# Decide which categorical variables that are most relevant

mm <- lm(log(price) ~ country + aspiration + carbody + drivewheel + cylindernumber, data = data)
step(mm, directions = "backward")

########## ########## ########## ########## ##########

# Exhaustive search (if model is changed, "ncol = ..." and "force.in = c(...)" might also need to be changed)

# Create full model
# m <- lm(log(price) ~ aspiration + drivewheel + carlength + carwidth + enginesize + log(horsepower) + I(1/highwaympg) + wheelbase + curbweight + I(1/citympg), x=T, data=data)
m <- lm(log(price) ~ aspiration + drivewheel + carlength + carwidth + enginesize + log(horsepower) + I(1/highwaympg), x=T, data=data)
X <- m$x  # extract the design matrix
X <- X[, -c(1)]  # remove the intercept

all_indices <- seq(1, nrow(data))

# Create folds for cross-validation
k <- 9
new_folds <- T

while (new_folds) {
  folds <- createFolds(data$price, k = k, list = TRUE, returnTrain = FALSE)
  
  new_folds <- F
  
  # Check that all levels of all categorical covariates are represented in all folds
  for (i_fold in 1:length(folds)) {
    i_test <- folds[[i_fold]]
    i_train <- setdiff(all_indices, i_test)
    
    test_turbo <- X[i_test, 1]
    train_turbo <- X[i_train, 1]
    
    if (sum(test_turbo) == 0 || sum(train_turbo) == 0) {
      # print(paste("Level turbo is not represented in fold ", i_fold))
      new_folds <- T
    }
    if (!any(test_turbo == 0) || !any(train_turbo == 0)) {
      # print(paste("Level std is not represented in fold ", i_fold))
      new_folds <- T
    }
    
    test_fwd <- X[i_test, 2]
    train_fwd <- X[i_train, 2]
    
    test_rwd <- X[i_test, 3]
    train_rwd <- X[i_train, 3]
    
    if (sum(test_fwd) == 0 || sum(train_fwd) == 0) {
      # print(paste("Level fwd is not represented in fold ", i_fold))
      new_folds <- T
    }
    if (sum(test_rwd) == 0 || sum(train_rwd) == 0) {
      # print(paste("Level rwd is not represented in fold ", i_fold))
      new_folds <- T
    }
    
    temp1 <- test_fwd + test_rwd
    temp2 <- train_fwd + train_rwd
    
    # print(paste('Fold ', i_fold))
    if (!any(temp1 == 0) || !any(temp2 == 0)) {
      # Intercept is not represented
      # print(paste("Level 4wd is not represented in fold ", i_fold))
      new_folds <- T
    } else {
      # print(which(temp == 0))
    }
  }
}

prederrors <- matrix(nrow = length(folds), ncol = 31, byrow = T)  # HARDCODED VALUE (the number of models)!!!  (31)

for (i_fold in 1:length(folds)) {
  # Split data set into training data and test data
  i_test <- folds[[i_fold]]
  i_train <- setdiff(all_indices, i_test)
  
  xx <- as.matrix(X[i_train,])
  yy <- log(data$price)[i_train]
  xx_test <- as.matrix(X[i_test,])
  yy_test <- log(data$price)[i_test]
  
  # Perform regression for all subsets of covariates
  m <- regsubsets(xx, yy, int = T, nbest = 1000, nvmax=dim(X)[2], method = c("ex"),
                  really.big = T, force.in = c(1, 2, 3))  # include ALL levels of categorical variables
  print(m)  # To check that categorical variables are included
  cleaps <- summary(m, matrix=T)
  
  # When using force.in, regsubsets rearranges the order of covariates so that the dummy variables appear first
  col_order <- c(colnames(cleaps$which))
  xx <- xx[, col_order[-1]]
  xx_test <- xx_test[, col_order[-1]]
  
  # Calculate the prediction errors for the current fold
  temp_prederrors <- rep(0, dim(cleaps$which)[1])
  for (jj in (1:dim(cleaps$which)[1])) {
    # Fit model to training data
    mm <- lm(yy ~ xx[, cleaps$which[jj, -1] == T])
    
    # Obtain the design matrix for the given model
    design <- xx_test[, cleaps$which[jj, -1] == T]
    
    # Add a column of ones for the intercept
    design <- cbind(rep(1, dim(xx_test)[1]), design)
    
    # Calculate pMSE for the current model
    temp_prederrors[jj] <- sum((yy_test - design %*% mm$coef)^2)
  }
  
  prederrors[i_fold,] <- temp_prederrors
}

prederror_per_model <- apply(prederrors, 2, sum)
best_models_indices <- sort(prederror_per_model, index.return = TRUE)
five_best_models_indices <- best_models_indices$ix[1:5]

temp <- rep(0, length(five_best_models_indices) * (length(folds) + 1))
j <- 1
jj <- (length(folds) + 1)
for (i in five_best_models_indices) {
  temp[j:(jj-1)] <- prederrors[, i]
  temp[jj] <- sum(prederrors[, i]) / dim(X)[1]
  j <- jj + 1
  jj <- jj + (length(folds) + 1)
}

final_result <- matrix(temp, ncol=length(folds)+1, byrow=TRUE)
rownames(final_result) <- c(paste("Model ", five_best_models_indices[1]), 
                            paste("Model ", five_best_models_indices[2]),
                            paste("Model ", five_best_models_indices[3]),
                            paste("Model ", five_best_models_indices[4]),
                            paste("Model ", five_best_models_indices[5]))

if (k == 3) {
  colnames(final_result) <- c("Fold 1","Fold 2","Fold 3", "PE")
} else if (k == 5) {
  colnames(final_result) <- c("Fold 1","Fold 2","Fold 3", "Fold 4", "Fold 5", "PE")
} else if (k == 9) {
  colnames(final_result) <- c("Fold 1","Fold 2","Fold 3", "Fold 4", "Fold 5", "Fold 6", "Fold 7", "Fold 8", "Fold 9", "PE")
}

temp <- rep(F, length(five_best_models_indices) * (dim(cleaps$which)[2]))
j <- 1
jj <- dim(cleaps$which)[2]
for (i in five_best_models_indices) {
  temp[j:jj] <- cleaps$which[i, ]
  j <- jj + 1
  jj <- jj + dim(cleaps$which)[2]
}

covariates_included <- matrix(temp, ncol=dim(cleaps$which)[2], byrow=TRUE)
rownames(covariates_included) <- c(paste("Model ", five_best_models_indices[1]), 
                                   paste("Model ", five_best_models_indices[2]),
                                   paste("Model ", five_best_models_indices[3]),
                                   paste("Model ", five_best_models_indices[4]),
                                   paste("Model ", five_best_models_indices[5]))
colnames(covariates_included) <- c("intercept", colnames(as.data.frame(X)))

########## ########## ########## ########## ##########

# Proposed model from above: aspiration + drivewheel + carlength + carwidth + enginesize + log(horsepower)

# Does it make sense to use both categorical covariates? Check if "aspiration" is necessary
mm1 <- lm(log(price) ~ drivewheel + carlength + carwidth +
            enginesize + log(horsepower), data=data)
summary(mm1)

mm2 <- lm(log(price) ~ drivewheel + aspiration + carlength + carwidth +
            enginesize + log(horsepower), data=data)
summary(mm2)

anova(mm1, mm2)  # anova(reducedmodel, fullmodel)

# Does it make sense to add some of the other categorical variables (e.g. country)?
mm3 <- lm(log(price) ~ drivewheel + country + carlength + carwidth +
            enginesize + log(horsepower), data=data)
summary(mm3)

anova(mm1, mm3)  # anova(reducedmodel, fullmodel)

########## ########## ########## ########## ##########

# Focus on USA
usa <- subset(data, continent == "usa")
usa$car <- droplevels(usa$car)
plot(usa$carwidth, log(usa$price))
# id<-identify(1:nrow(usa), usa$price, usa$car, pos=T)
boxplot(price~car, data=usa, varwidth=TRUE)

plot(usa$horsepower, usa$price)

########## ########## ########## ########## ##########

# Focus on Europe
europe <- subset(data, continent == "europe")
europe$car <- droplevels(europe$car)

plot(europe$price)
boxplot(price~car, data=europe, varwidth=TRUE)

plot(europe$horsepower, europe$price)
id<-identify(europe$horsepower, europe$price, europe$car, pos=T)

# Sweden
sweden <- subset(europe, country == "sweden")
sweden$country <- droplevels(sweden$country)

plot(sweden$enginesize, log(sweden$price))

########## ########## ########## ########## ##########

# Focus on Japan
japan <- subset(data, continent == "asia")
japan$car <- droplevels(japan$car)
plot(log(japan$horsepower), log(japan$price))
# id<-identify(1:nrow(japan), japan$price, japan$car, pos=T)
boxplot(price~car, data=japan, varwidth=TRUE)

########## ########## ########## ########## ##########

# Proposed model from above: drivewheel + country + carlength + carwidth + enginesize + log(horsepower

# Change baselines
data$country <- relevel(data$country, ref="sweden")
data$drivewheel <- relevel(data$drivewheel, ref="fwd")

# Fit model to all data
mm <- lm(log(price) ~ drivewheel + country + carlength + carwidth + enginesize + log(horsepower), data = data)
summary(mm)

xtable(summary(mm))

########## ########## ########## ########## ##########

# Look for outliers

# Check residuals
plot(mm$fit, mm$res, xlab="Fitted values", ylab="Residuals")
abline(h=0)
id1 <- identify(mm$fit, mm$res,pos=T)

# Check normal error assumption
n <- nrow(data)
qq <- seq(0.5/n, (n-0.5)/n, length=n)
normq <- qnorm(p=qq)
rsort <- sort(rstandard(mm))
rlist <- sort.list(rstandard(mm))
plot(normq, rsort, xlab="Theoretical quantiles", ylab="Standardized residuals")
qr <- quantile(rstandard(mm))
qn <- quantile(qnorm(p=qq))
b <- (qr[4]-qr[2])/(qn[4]-qn[2])
a <- qr[4]-b*qn[4]
abline(a, b)
id2 <- identify(normq, sort(rstandard(mm)), label=rlist, pos=T)

# Check the constant error variance using absolute residuals
plot(mm$fit, abs(rstandard(mm)), xlab="Fitted values", ylab="|Standardized residuals|")
id3 <- identify(mm$fit, abs(rstandard(mm)), pos=T)

# Check the Cooks distance
lm1 <- lm.influence(mm)
cooksd <- cooks.distance(mm)
plot(cooksd, ylab="Cooks Distance", type="h")
idc <- identify(cooksd,pos=T)

# Check leverage
plot(lm1$hat, ylab="Leverage")
abline(h=(2*6)/n)  # 2p/n
idlev <- identify(lm1$hat, pos=T)

# Change in slope estimate
plot(lm1$coeff[, 13], ylab="Change in slope for log(horsepower)")  # 13 = log(horsepower)
idhp <- identify(lm1$coeff[,13], pos=T)

# Change in standard error estimate
plot(lm1$sig, ylab="Change in standard error estimate")
ids<-identify(lm1$sig, pos=T)

indvec<-sort(c(id1$ind, id2$ind, id3$ind, idc$ind, idlev$ind, idhp$ind, ids$ind))
print(table(indvec))

########## ########## ########## ########## ##########

# Observations 67, 129 and 168 are marked as outliers

# Final model

# Remove outliers
data2 <- data[-c(67, 129, 168),]

mm <- lm(log(price) ~ drivewheel + country + carlength + carwidth + 
           enginesize + log(horsepower), x=T, data = data2)
summary(mm)
xtable(summary(mm))

par(pty="s")
plot(mm$fitted.values, log(data2$price), 
     xlab=expression(paste("log(", hat(Y), ")")), 
     ylab="log(Y)")
abline(a=0, b=1)

########## ########## ########## ########## ##########

# Confidence intervals
mm <- lm(log(price) ~ drivewheel + country + carlength + carwidth + enginesize + log(horsepower), x=T, data = data2)
summary(mm)
confint(mm)

japan <- subset(data2, continent == "asia")

# Focus on data for Japan
mean_width <- mean(japan$carwidth)
mean_length <- mean(japan$carlength)
mean_engine <- mean(japan$enginesize)

N <- 100
loghp <- seq(from = 3.5, to = 5.5, length.out = N)
fit <- rep(0, N)
lwr <- rep(0, N)
upr <- rep(0, N)

pred_fit <- rep(0, N)
pred_lwr <- rep(0, N)
pred_upr <- rep(0, N)

for (i in 1:N) {
  temp_loghp <- loghp[i]
  newdata <- data.frame(drivewheel=factor("fwd", levels=c("fwd", "4wd", "rwd")),
                        country=factor("japan", levels=c("sweden", "france", "germany", "italy", "japan", "uk", "usa")),
                        carlength=mean_length, carwidth=mean_width, enginesize=mean_engine, horsepower=exp(temp_loghp))
  p1 <- predict(mm, newdata, interval="confidence")
  fit[i] <- p1[1]
  lwr[i] <- p1[2]
  upr[i] <- p1[3]
  
  p2 <- predict(mm, newdata, interval="prediction")
  pred_fit[i] <- p2[1]
  pred_lwr[i] <- p2[2]
  pred_upr[i] <- p2[3]
}

par(pty="s")
plot(log(japan$horsepower), log(japan$price), 
     main="Front-wheel drive Japanese cars", xlab="log(horsepower)", ylab="log(price)")
lines(loghp, fit, lty="solid")
lines(loghp, upr, lty="dashed")
lines(loghp, lwr, lty="dashed")
lines(loghp, pred_upr, lty="dotted", col="red")
lines(loghp, pred_lwr, lty="dotted", col="red")

# Ooops, it seems that enginesize and horsepower are related to each other

# Remove enginesize from the model
mm <- lm(log(price) ~ drivewheel + country + carlength + carwidth + log(horsepower), x=T, data = data2)

# Predict again
for (i in 1:N) {
  temp_loghp <- loghp[i]
  newdata <- data.frame(drivewheel=factor("fwd", levels=c("fwd", "4wd", "rwd")),
                        country=factor("japan", levels=c("sweden", "france", "germany", "italy", "japan", "uk", "usa")),
                        carlength=mean_length, carwidth=mean_width, horsepower=exp(temp_loghp))
  p1 <- predict(mm, newdata, interval="confidence")
  fit[i] <- p1[1]
  lwr[i] <- p1[2]
  upr[i] <- p1[3]
  
  p2 <- predict(mm, newdata, interval="prediction")
  pred_fit[i] <- p2[1]
  pred_lwr[i] <- p2[2]
  pred_upr[i] <- p2[3]
}

par(pty="s")
plot(log(japan$horsepower), log(japan$price), 
     main="Front-wheel drive Japanese cars", xlab="log(horsepower)", ylab="log(price)")
lines(loghp, fit, lty="solid")
lines(loghp, upr, lty="dashed")
lines(loghp, lwr, lty="dashed")
lines(loghp, pred_upr, lty="dotted", col="red")
lines(loghp, pred_lwr, lty="dotted", col="red")
