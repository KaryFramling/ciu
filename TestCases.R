# Tests to perform on ciu package. The "testthat" package doesn't quite
# offer the right functionality for this kind of packages. So this is the
# adopted way of performing tests until finding a better way to do it.

require(ggplot2)
require(MASS)
require(caret)
require(ciu)

# Iris data set, lda model.
test.iris.lda <- function() {
  iris_train <- iris[, 1:4]
  iris_lab <- iris$Species
  iris.lda <- lda(iris_train, iris_lab)
  instance <- iris[100,1:4]
  ciu <- ciu.new(iris.lda, Species~., iris)
  for ( i in 1:length(levels(iris$Species)))
    ciu$barplot.ciu(instance, ind.output = i)
  ciu$plot.ciu.3D(instance, ind.inputs = c(1,2), ind.output = 2)
  ciu$plot.ciu.3D(instance, ind.inputs = c(3,4), ind.output = 2)
  p <- ciu$ggplot.col.ciu(instance); print(p)
}

# Boston with GBM
test.boston.gbm <- function() {
  kfoldcv <- trainControl(method="cv", number=10)
  gbm <- train(medv ~ ., Boston, method="gbm", trControl=kfoldcv)
  instance <- Boston[370,1:13]
  ciu <- ciu.new(gbm, medv~., Boston)
  p <- ciu$ggplot.col.ciu(instance); print(p)
  ciu$barplot.ciu(instance, sort="CI")
  # See how lstat,rm,crim affect output.
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,3))
  ciu$plot.ciu(instance,13,main="BH: #370")
  ciu$plot.ciu(instance,6,main="BH: #370")
  ciu$plot.ciu(instance,1,main="BH: #370")
}

# Heart disease with RF
test.heart.disease.rf <- function() {
  orig.heart.data <- heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
  names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                          "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
  heart.data$num[heart.data$num > 0] <- 1
  heart.data$num <- as.factor(heart.data$num)
  heart.data <- na.omit(heart.data)
  # Random Forest with caret.
  kfoldcv <- trainControl(method="cv", number=10)
  performance_metric <- "Accuracy"
  rf.heartdisease <- train(num~., data=heart.data, method="rf", metric=performance_metric, trControl=kfoldcv,preProcess=c("center", "scale"))
  n.in <- ncol(heart.data) - 1
  instance <- heart.data[32,1:n.in]
  ciu <- ciu.new(rf.heartdisease, num~., heart.data, output.names=c("No Heart Disease","Heart Disease Present"))
  p <- ciu$ggplot.col.ciu(instance, c(1:n.in)); print(p)
  ciu$barplot.ciu(instance, ind.output=1, sort="CI")
  ciu$barplot.ciu(instance, ind.output=2, sort="CI")
  ciu$pie.ciu(instance, ind.output=1, sort="CI")
  ciu$pie.ciu(instance, ind.output=2, sort="CI")
  for ( i in 1:n.in ) {
    ciu$plot.ciu(instance, ind.input=i, ind.output=2)
  }
}

# UCI Cars with RF
test.cars.UCI.rf <- function() {
  car.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", header=FALSE)
  colnames(car.data) <- c("buying","maint","doors","persons","lug_boot","safety","result")
  price <- c(1,2)
  comfort <- c(3,4,5)
  tech <- c(comfort, 6)
  car <- c(price, tech)
  voc <- list("PRICE"=price, "COMFORT"=comfort, "TECH"=tech, "CAR"=car,
              "Buying Price"=c(1), "Maintenance Cost"=c(2), "#Doors"=c(3),
              "Person Capacity"=c(4), "#Luggage in Boot"=c(5), "Safety"=c(6))
  n.in <- ncol(car.data) - 1
  n.out <- 1
  # Random Forest with caret
  kfoldcv <- trainControl(method="cv", number=10)
  performance_metric <- "Accuracy"
  rf.carsUCI <- train(result~., data=car.data, method="rf", metric=performance_metric, trControl=kfoldcv)
  inst.ind <- 1098 # A very good one"
  instance <- car.data[inst.ind,1:6]
  ciu <- ciu.new(rf.carsUCI, formula=result~., data=car.data, vocabulary=voc)
  # Global plot, inputs
  p <- ciu$ggplot.col.ciu(instance, c(1:6)); print(p)
  # Plot according to PRICE, TECH
  p <- ciu$ggplot.col.ciu(instance, concepts.to.explain=c("PRICE", "TECH")); print(p)
  # Why is PRICE good?
  p <- ciu$ggplot.col.ciu(instance, ind.inputs = voc$PRICE, target.concept = "PRICE"); print(p)
  # Why is TECH good?
  p <- ciu$ggplot.col.ciu(instance, ind.inputs = voc$TECH, target.concept = "TECH"); print(p)
  # What happens if "safety" is "med" instead?
  instance$safety <- "med"
  p <- ciu$ggplot.col.ciu(instance, ind.inputs = voc$TECH, target.concept = "TECH"); print(p)
  # What happens if "persons" is "more" instead?
  instance <- car.data[inst.ind,];
  instance$persons <- "more"
  p <- ciu$ggplot.col.ciu(instance, ind.inputs = voc$TECH, target.concept = "TECH"); print(p)
}

# Diamonds with GBM. This is a mixed discrete/continuous input case. Takes
# a long time for GBM to train!
test.diamonds.gbm <- function() {
  kfoldcv <- trainControl(method="cv", number=10)
  diamonds.gbm <- train(price~., diamonds, method="gbm", trControl=kfoldcv)
  inst.ind <- 27750 # An expensive one!
  instance <- diamonds[inst.ind,-7]
  ciu <- ciu.new(diamonds.gbm, price~., diamonds)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  ciu$barplot.ciu(instance, sort="CI")
  p <- ciu$ggplot.col.ciu(instance); print(p)
}

# Titanic data set. Mixed discrete/continuous input case.
# Also tests plot.ciu with discrete inputs.
test.titanic.rf <- function() {
  require("DALEX")
  titanic_train <- titanic[,c("survived", "class", "gender", "age", "sibsp", "parch", "fare", "embarked")]
  titanic_train$survived <- factor(titanic_train$survived)
  titanic_train$gender <- factor(titanic_train$gender)
  titanic_train$embarked <- factor(titanic_train$embarked)
  titanic_train <- na.omit(titanic_train)

  # Using Random Forest here. Really bad results... Takes a while also (15 seconds?)
  kfoldcv <- trainControl(method="cv", number=10)
  model_rf <- train(survived ~ ., titanic_train, method="rf", trControl=kfoldcv)

  # Example instance
  new_passenger <- data.frame(
    class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew", "engineering crew", "restaurant staff", "victualling crew")),
    gender = factor("male", levels = c("female", "male")),
    age = 8,
    sibsp = 0,
    parch = 0,
    fare = 72,
    embarked = factor("Cherbourg", levels = c("Belfast", "Cherbourg", "Queenstown", "Southampton"))
  )
  ciu <- ciu.new(model_rf, survived~., titanic_train)
  p <- ciu$ggplot.col.ciu(new_passenger); print(p)
  for ( i in 1:ncol(new_passenger) )
    ciu$plot.ciu(new_passenger,i,1)
  for ( i in 1:ncol(new_passenger) )
    ciu$plot.ciu(new_passenger,i,2)
}

# Run only one at a time! Otherwise at least the ggplot figures do not show up.
# par(mai=c(0.8,1.2,0.4,0.2)) # Good parameters for barplot so that labels fit in.

#test.iris.lda()
#test.boston.gbm()
#test.heart.disease.rf()
#test.cars.UCI.rf() # Takes about 15 seconds for RF to train
#test.diamonds.gbm() # Takes something like 2-3 minutes to train but GBM seems to be best by far here.
#test.titanic.rf() # Takes maybe half minute.
