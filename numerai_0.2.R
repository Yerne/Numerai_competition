

rm(list = ls())


# if(!require(installr)) {
#   install.packages("installr"); require(installr)} #load / install+load installr
install.packages("afCEC")
library("afCEC")
afCEC (points= , maxClusters= )

library(afCEC)
data(fire)
plot(fire, asp=1, pch=20)

result <- afCEC(fire, 5,  numberOfStarts=10);
print(result)
plot(result)

# using the package:
# updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.
# XGBoost
setwd("D:\\GitHub\\20170605-Numerai_train\\1. Original Data\\87")
# if (!require(package)) install.packages('package')
# library(package)
# library(reticulate)
# py_discover_config()
# # use_condaenv("snakes")
# numpy <- import ("numpy")
# # C:\Users\TEMP.Main.001\Anaconda3\envs\snakes\Lib\site-packages\pandas
# pandas <- import ("pandas")
# main <- import_main()
# sys <- import("sys")
# os <- import("os")
# os$getcwd()

# Importing the dataset
# dataset = pandas.read_csv('numerai_training_data.csv')
#X = dataset.iloc[:, 3:24].values
# library(rattle)
# rattle()

# Importing the dataset
dataset <- read.csv('numerai_training_data.csv')
# dataset = dataset[4:25]
t_set= read.csv('numerai_tournament_data.csv')
# valid_set= valid_set[4:24]
# 
data2 = dataset[,4:53]

corMat <- cor(data2, use = "pairwise.complete.obs")
# View(round(corMat, 2))
library("qgraph")
corMat <- cor_auto(data2) # Correlate data
Graph_pcor <- qgraph(corMat, graph = "pcor", layout = "spring")
Graph_pcor <- qgraph(corMat, graph = "pcor", layout = "spring", threshold = "bonferroni",
                     sampleSize = nrow(data2), alpha = 0.05)
Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
                      sampleSize = nrow(data2))
# data(dataset)
# library("cluster")
# agn1 <- agnes(dataset, metric = "euclidian")
# agn2 <- agnes(dataset, diss = FALSE, method = "complete")
# plot(agn2)
# library(cluster)
# res.diana <- diana(dataset, stand = TRUE)

# Prepare Data
# mydata <- na.omit(dataset) # listwise deletion of missing
# # mydata <- scale(dataset) # standardize variables 
# # Ward Hierarchical Clustering
# library(bigmemory)
# 
# d <- big.matrix(dist(dataset, method = "euclidean")) # distance matrix
# fit <- hclust(d, method="ward")
# plot(fit) # display dendogram
# groups <- cutree(fit, k=5) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters
# rect.hclust(fit, k=5, border="red") 
df <- as.matrix(dataset[,4:53])
# Model Based Clustering
# library(mclust)
# fit <- Mclust(df)
# plot(fit) # plot results
# summary(fit) # display the best model 


y <- dataset[53]
#o_set   = pd.read_csv('numerai_tournament_data.csv')
# t_set = pd.read_csv('numerai_tournament_data.csv')
#t_X = t_set.iloc[:, 3:24].values

# Bind tset+dset for xmean total 
X_t_set <- rbind(dataset, t_set)


dataset$data_type <- NULL
X <- dataset
X.index <- X['id']
X$id <- NULL
X$target <- NULL

xmean <- aggregate(X_t_set, list(X_t_set$era), mean)

xmean <- aggregate(X, list(X$era), mean)
# calculate difference between X values and xmean values
xmean['era'] <- xmean['Group.1']
xmean$Group.1 <- NULL

# M <- merge(X,xmean,by="era")
# 
# S <- M[,grepl("*\\.x$",names(M))] - M[,grepl("*\\.y$",names(M))]
# 
# cbind(M[,1,drop=FALSE],S)

# X1 <- X - xmean
for (i in 1:nrow(X)) {
  X1['era', X1[i,1]] <- X['era', X[i,1]] - xmean['era', xmean[,1]] 
}

for (i in 1:nrow(X)) { 
  X1[i, ] <- X[i, ] - xmean[match(X$era, xmean$era)] 
  }


X-xmean[,1][col(X)]

X1 <- X$ - xmean[match(X$era, xmean$era)]
X1 <- X-xmean[match(colnames(X), rownames(DF2)),1][col(DF1)]

B$diff <- B$number - A$number[match(B$animal, A$animal)]
# Fitting XGBoost to the Training set
# install.packages('xgboost')

####################################### Gridsearch xgboost
data = dataset[,4:53]
library(xgboost)

####################################### Gridsearch xgboost

searchGridSubCol <- expand.grid(subsample = c(0.005, 0.1, 0.5), 
                                colsample_bytree = c(0.1, 0.3),
                                max_depth = c(3, 5, 10),
                                # min_child = seq(1), 
                                # nrounds = c(450, 1000),
                                lambda = c(.001, .01, .1),
                                alpha = c(.001, .01, .1),
                                eta = c(0.1, 0.01, 0.001)
)

# ntrees <- 5

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentlambda <- parameterList[["lambda"]]
    currentalpha <- parameterList[["alpha"]]
    # currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data = as.matrix(data), nrounds = 500, nfold = 2, showsd = TRUE, label = dataset$target, metrics = "error", verbose = TRUE, "eval_metric" = "error",
                             "objective" = "binary:logistic", "max.depth" = currentDepth, "eta" = currentEta, "lambda" = currentlambda, "alpha" = currentalpha,                            
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, booster = "gbtree",
                             early_stopping_rounds = 20)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    error <- tail(xvalidationScores$test_error_mean, 1)
    terror <- tail(xvalidationScores$train_error_mean,1)
    output <- return(c(error, terror, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentlambda, currentalpha))
    
  }))

output <- as.data.frame(t(rmseErrorsHyperparameters))
head(output)
varnames <- c("error", "terror","SubSampRate", "ColSampRate", "Depth", "eta", "currentlambda", "currentalpha")
names(output) <- varnames
head(output)
write.csv(output, "xgb_gridsearch.csv")





################################################################









library(xgboost)
classifier <- xgboost(data = as.matrix(dataset), label = dataset$target, max.depth = 3, eta = 0.1, nround = 800, eval.metric = "error", eval.metric = "logloss", subsample = 0.005,lambda=0.001, alpha = 0.001, colsample_bytree = 0.1, print_every_n = 50, objective = "binary:logistic")
# 
# 
# res0 <- xgb_cv_opt(data = as.matrix(dataset),
#                    label = dataset$target,
#                    objectfun = "binary:logistic",
#                    evalmetric = "error",
#                    n_folds = 3,
#                    init_points = 2,
#                    n_iter = 1
# )

# bstSparse <- xgboost(data = as.matrix(dataset), label = dataset$target, max.depth = 5, eta = 0.1, nthread = 2, nround = 200, objective = "binary:logistic")


# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(valid_set))
y_pred = predict(classifier, valid_set)





# y_pred = (y_pred >= 0.5)

# Making the Confusion Matrix
# cm = table(test_set, y_pred)

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$target, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set), label = training_set$target, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold))

})
# accuracy = mean(as.numeric(cv))
pred =y_pred


#__________________###############_###############################__________#########################################################################################################################################################################################
data(dataset.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
# the loaded data is stored in sparseMatrix, and label is a numeric vector in {0,1}
class(train$label)
class(train$data)

#-------------Basic Training using XGBoost-----------------
# this is the basic usage of xgboost you can put matrix in data field
# note: we are putting in sparse matrix here, xgboost naturally handles sparse input
# use sparse matrix when your feature is sparse(e.g. when you are using one-hot encoding vector)
print("Training xgboost with sparseMatrix")
bst <- xgboost(data = train$data, label = train$label, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic")
# alternatively, you can put in dense matrix, i.e. basic R-matrix
print("Training xgboost with Matrix")
bst <- xgboost(data = as.matrix(train$data), label = train$label, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic")

# you can also put in xgb.DMatrix object, which stores label, data and other meta datas needed for advanced features
print("Training xgboost with xgb.DMatrix")
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nrounds = 2, nthread = 2, 
               objective = "binary:logistic")

# Verbose = 0,1,2
print("Train xgboost with verbose 0, no message")
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic", verbose = 0)
print("Train xgboost with verbose 1, print evaluation metric")
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic", verbose = 1)
print("Train xgboost with verbose 2, also print information about tree")
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic", verbose = 2)

# you can also specify data as file path to a LibSVM format input
# since we do not have this file with us, the following line is just for illustration
# bst <- xgboost(data = 'agaricus.train.svm', max_depth = 2, eta = 1, nrounds = 2,objective = "binary:logistic")

#--------------------basic prediction using xgboost--------------
# you can do prediction using the following line
# you can put in Matrix, sparseMatrix, or xgb.DMatrix 
pred <- predict(bst, test$data)
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

#-------------------save and load models-------------------------
# save model to binary local file
xgb.save(bst, "xgboost.model")
# load binary model to R
bst2 <- xgb.load("xgboost.model")
pred2 <- predict(bst2, test$data)
# pred2 should be identical to pred
print(paste("sum(abs(pred2-pred))=", sum(abs(pred2-pred))))

# save model to R's raw vector
raw = xgb.save.raw(bst)
# load binary model to R
bst3 <- xgb.load(raw)
pred3 <- predict(bst3, test$data)
# pred3 should be identical to pred
print(paste("sum(abs(pred3-pred))=", sum(abs(pred3-pred))))

#----------------Advanced features --------------
# to use advanced features, we need to put data in xgb.DMatrix
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
#---------------Using watchlist----------------
# watchlist is a list of xgb.DMatrix, each of them is tagged with name
watchlist <- list(train=dtrain, test=dtest)
# to train with watchlist, use xgb.train, which contains more advanced features
# watchlist allows us to monitor the evaluation result on all data in the list 
print("Train xgboost using xgb.train with watchlist")
bst <- xgb.train(data=dtrain, max_depth=2, eta=1, nrounds=2, watchlist=watchlist,
                 nthread = 2, objective = "binary:logistic")
# we can change evaluation metrics, or use multiple evaluation metrics
print("train xgboost using xgb.train with watchlist, watch logloss and error")
bst <- xgb.train(data=dtrain, max_depth=2, eta=1, nrounds=2, watchlist=watchlist,
                 eval_metric = "error", eval_metric = "logloss",
                 nthread = 2, objective = "binary:logistic")

# xgb.DMatrix can also be saved using xgb.DMatrix.save
xgb.DMatrix.save(dtrain, "dtrain.buffer")
# to load it in, simply call xgb.DMatrix
dtrain2 <- xgb.DMatrix("dtrain.buffer")
bst <- xgb.train(data=dtrain2, max_depth=2, eta=1, nrounds=2, watchlist=watchlist,
                 nthread = 2, objective = "binary:logistic")
# information can be extracted from xgb.DMatrix using getinfo
label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

# You can dump the tree you learned using xgb.dump into a text file
xgb.dump(bst, "dump.raw.txt", with_stats = T)

# Finally, you can check which features are the most important.
print("Most important features (look at column Gain):")
imp_matrix <- xgb.importance(feature_names = colnames(train$data), model = bst)
print(imp_matrix)

# Feature importance bar plot by gain
print("Feature importance Plot : ")
print(xgb.plot.importance(importance_matrix = imp_matrix))











