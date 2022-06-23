# Examples and tests to perform on ciu package with mlr3.

# mlr3 package tests.
#
# See https://mlr3.mlr-org.com
#

#install.packages("mlr3")
#remotes::install_github("mlr-org/mlr3")
#install.packages("mlr3verse")

require(mlr3)
require(ciu)

# Penguins
# --------
# create learning task
task_penguins <- TaskClassif$new(id = "penguins", backend = palmerpenguins::penguins, target = "species")
# load learner and set hyperparameter
learner <- lrn("classif.rpart", cp = .01, predict_type = "prob")
# train/test split
train_set <- sample(task_penguins$nrow, 0.8 * task_penguins$nrow)
test_set <- setdiff(seq_len(task_penguins$nrow), train_set)
# train the model
model <- learner$train(task_penguins, row_ids = train_set)
# predict data
prediction <- learner$predict(task_penguins, row_ids = test_set)
# calculate performance
prediction$confusion
measure <- msr("classif.acc")
prediction$score(measure)
# CIU. Needs to clean away NA rows first.
dtrain <- task_penguins$data()[train_set,]; dtrain <- dtrain[complete.cases(dtrain),]
dtest <- task_penguins$data()[test_set,]; dtest <- dtest[complete.cases(dtest),]
ciu <- ciu.new(model, species~., dtrain)
cbind(colnames(dtrain)[-1], ciu.list.to.frame(ciu$meta.explain(dtest[1,-1])$ciuvals)) # Set "out.ind" for other outputs
# Gradient boosting instead Or nnet. NOPE, skip because they don't support factor/integer/etc types
#library(xgboost)
# library(mlr3learners)
# lrn = lrn("classif.xgboost", predict_type = "prob")
# model = lrn$train(task_penguins, row_ids = train_set)
# ciu <- ciu.new(model, species~., dtrain)
# cbind(colnames(dtrain)[-1], ciu.list.to.frame(ciu$meta.explain(dtest[1,-1])$ciuvals)) # Set "out.ind" for other outputs

# German Credit
# -------------
credit.task <- tsk("german_credit")
lrn <- lrn("classif.rpart", predict_type = "prob")
model <- lrn$train(credit.task)
data <- credit.task$data()
ciu <- ciu.new(model, credit_risk~., data)
cbind(colnames(data)[-1], ciu.list.to.frame(ciu$meta.explain(data[1,-1])$ciuvals))

