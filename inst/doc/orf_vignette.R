## ----orf intro, include = FALSE------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 7, fig.align = "center")
library(orf)
set.seed(123)

## ----data----------------------------------------------------------------
# load example data
data(odata)

# specify response and covariates
Y <- as.numeric(odata[, 1])
X <- as.matrix(odata[, -1])

## ----orf default---------------------------------------------------------
# estimate Ordered Forest with default settings
orf_model <- orf(X, Y)

# print output of the orf estimation
print(orf_model)

## ----orf custom----------------------------------------------------------
# estimate Ordered Forest with custom settings
orf_model <- orf(X, Y,
                       num.trees = 1000, mtry = 2, min.node.size = 5,
                       replace = FALSE, sample.fraction = 0.5,
                       honesty = TRUE, honesty.fraction = 0.5,
                       inference = FALSE, importance = FALSE)

# show summary of the orf estimation
summary(orf_model)

## ----orf plot------------------------------------------------------------
# plot the estimated probability distributions
plot(orf_model)

## ----orf predict---------------------------------------------------------
# get fitted values with the estimated orf
orf_fitted <- predict(orf_model)

# print orf fitted values
print(orf_fitted)

## ----orf predict test----------------------------------------------------
# specify response and covariates for train and test
idx <- sample(seq(1, nrow(odata), 1), 0.8*nrow(odata))

# train set
Y_train <- odata[idx, 1]
X_train <- odata[idx, -1]

# test set
Y_test <- odata[-idx, 1]
X_test <- odata[-idx, -1]

# estimate Ordered Forest
orf_train <- orf(X_train, Y_train)

# predict the probabilities with the estimated orf
orf_test <- predict(orf_train, newdata = X_test, type = "probs", inference = FALSE)

# summary of the orf predictions
summary(orf_test)

## ----orf margins---------------------------------------------------------
# estimate marginal effects of the orf
orf_margins <- margins(orf_model)

# print the results of the marginal effects estimation
print(orf_margins)

## ----orf margins custom--------------------------------------------------
# estimate marginal effects of the orf with inference
orf_margins <- margins(orf_model, eval = "mean", window = 0.1,
                                  inference = TRUE, newdata = NULL)

# summarize the results of the marginal effects estimation
summary(orf_margins)

