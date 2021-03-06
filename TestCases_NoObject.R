# Tests to perform on ciu package. The "testthat" package doesn't quite
# offer the right functionality for this kind of packages. So this is the
# adopted way of performing tests until finding a better way to do it.

require(ggplot2)
require(MASS)
require(caret)
require(ciu)

scaleFUN <- function(x) as.character(round(x, digits = 2))

# Simple weighted sum
test.ws <- function() {
  x <- y <- seq(0, 1, 0.05)
  pm <- as.matrix(expand.grid(x,y))
  pm_df <- data.frame(x1=pm[,1],x2=pm[,2])
  c <- data.frame(x1=0.7, x2=0.8)
  c1 <- data.frame(x1=0.5, x2=0.5)
  linfunc <- function(m, inp) { 0.3*inp[,1] + 0.7*inp[,2] }
  z <- linfunc(NULL, pm)
  ciu <- ciu(NULL, in.min.max.limits = matrix(c(0,0,1,1), ncol=2), abs.min.max=matrix(c(0,1), ncol=2),
                 input.names=c("x1","x2"), output.names = c("y"),
                 predict.function = linfunc)
  p <- ciu.ggplot.col(ciu, c1) + labs(title="", x ="", y="CI", fill="CU"); print(p)
  p <- ciu.ggplot.col(ciu, c1, use.influence=TRUE, low.color = "firebrick", high.color = "steelblue")
  p <- p + labs(title="", x ="", y = expression(phi)) + scale_y_continuous(labels=scaleFUN)
  print(p)
  print(ciu.explain(ciu, c1,1))
  print(ciu.explain(ciu, c1,2))
}

# Iris data set, lda model.
test.ciu.iris.lda <- function() {
  iris_train <- iris[, 1:4]
  iris_lab <- iris$Species
  iris.lda <- lda(iris_train, iris_lab)
  instance <- iris[100,1:4]
  ciu <- ciu(iris.lda, Species~., iris)
  for ( i in 1:length(levels(iris$Species)))
    ciu.barplot(ciu, instance, ind.output = i)
  ciu.plot.3D(ciu, instance, ind.inputs = c(1,2), ind.output = 2)
  ciu.plot.3D(ciu, instance, ind.inputs = c(3,4), ind.output = 2)
  p <- ciu.ggplot.col(ciu, instance); print(p)
}

# Boston with GBM
test.ciu.boston.gbm <- function() {
  kfoldcv <- trainControl(method="cv", number=10)
  gbm <- train(medv ~ ., Boston, method="gbm", trControl=kfoldcv)
  instance <- Boston[370,1:13]
  ciu <- ciu(gbm, medv~., Boston)
  p <- ciu.ggplot.col(ciu, instance); print(p)
  ciu.barplot(ciu, instance, sort="CI")
  # See how lstat,rm,crim affect output.
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,3))
  ciu.plot(ciu, instance,13,main="BH: #370")
  ciu.plot(ciu, instance,6,main="BH: #370")
  ciu.plot(ciu, instance,1,main="BH: #370")
}

# Heart disease with RF
test.ciu.heart.disease.rf <- function() {
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
  ciu <- ciu(rf.heartdisease, num~., heart.data, output.names=c("No Heart Disease","Heart Disease Present"))
  p <- ciu.ggplot.col(ciu, instance, c(1:n.in)); print(p)
  ciu.barplot(ciu, instance, ind.output=1, sort="CI")
  ciu.barplot(ciu, instance, ind.output=2, sort="CI")
  ciu.pie(ciu, instance, ind.output=1, sort="CI")
  ciu.pie(ciu, instance, ind.output=2, sort="CI")
  for ( i in 1:n.in ) {
    ciu.plot(ciu, instance, ind.input=i, ind.output=2)
  }
}

# UCI Cars with RF
test.ciu.cars.UCI.rf <- function() {
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
  ciu <- ciu(rf.carsUCI, formula=result~., data=car.data, vocabulary=voc)
  # Global plot, inputs
  p <- ciu.ggplot.col(ciu, instance, c(1:6)); print(p)
  # Plot according to PRICE, TECH
  for ( i in 1:4 )
    ciu.barplot(ciu, instance, ind.output=i, sort="CI", concepts.to.explain=c("PRICE", "TECH"))
  p <- ciu.ggplot.col(ciu, instance, concepts.to.explain=c("PRICE", "TECH")); print(p)
  # Why is PRICE good?
  p <- ciu.ggplot.col(ciu, instance, ind.inputs = voc$PRICE, target.concept = "PRICE"); print(p)
  # Why is TECH good?
  p <- ciu.ggplot.col(ciu, instance, ind.inputs = voc$TECH, target.concept = "TECH"); print(p)
  # What happens if "safety" is "med" instead?
  instance$safety <- "med"
  p <- ciu.ggplot.col(ciu, instance, ind.inputs = voc$TECH, target.concept = "TECH"); print(p)
  # What happens if "persons" is "more" instead?
  instance <- car.data[inst.ind,];
  instance$persons <- "more"
  p <- ciu.ggplot.col(ciu, instance, ind.inputs = voc$TECH, target.concept = "TECH"); print(p)
}

# Diamonds with GBM. This is a mixed discrete/continuous input case. Takes
# a long time for GBM to train!
test.ciu.diamonds.gbm <- function() {
  kfoldcv <- trainControl(method="cv", number=10)
  diamonds.gbm <- train(price~., diamonds, method="gbm", trControl=kfoldcv)
  inst.ind <- 27750 # An expensive one!
  instance <- diamonds[inst.ind,-7]
  ciu <- ciu(diamonds.gbm, price~., diamonds)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  ciu.barplot(ciu, instance, sort="CI")
  p <- ciu.ggplot.col(ciu, instance); print(p)
}

# Titanic data set. Mixed discrete/continuous input case.
# Also tests plot.ciu with discrete inputs.
test.ciu.titanic.rf <- function() {
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
  ciu <- ciu(model_rf, survived~., titanic_train)
  p <- ciu.ggplot.col(ciu, new_passenger); print(p)
  for ( i in 1:ncol(new_passenger) )
    ciu.plot(ciu, new_passenger,i,1)
  for ( i in 1:ncol(new_passenger) )
    ciu.plot(ciu, new_passenger,i,2)

  # Text-based.
  cat(ciu.textual(ciu, new_passenger[,-8], use.text.effects = TRUE, ind.output = 2))
  # Customized limits for CI.
  cat(ciu.textual(ciu, new_passenger[,-8], use.text.effects = TRUE, ind.output = 2,
                  CI.voc = data.frame(limits=c(0.05,0.15,0.25,0.5,1.0),
                                      texts=c("not important","little important", "important","very important",
                                              "extremely important"))))

  # Intermediate concepts
  wealth<-c(1,6); family<-c(4,5); gender<-c(2); age<-c(3); embarked <- c(7)
  Titanic.voc <- list("WEALTH"=wealth, "FAMILY"=family, "Gender"=gender,
                      "Age"=age, "Embarkment port"=embarked)
  ciu <- ciu.new(model_rf, survived~., titanic_train, vocabulary = Titanic.voc)
  # Need to use meta.explain here in order to guarantee same CIU values for
  # intermediate concepts when moving from one level to the other.
  meta.top <- ciu$meta.explain(new_passenger[,-8], concepts.to.explain=names(Titanic.voc), n.samples = 1000)
  cat(ciu.textual(ciu, new_passenger[,-8], use.text.effects = TRUE, ind.output = 2, ciu.meta = meta.top))

  # Explain intermediate concept utility, using input features (could also
  # be using other intermediate concepts).
  cat(ciu.textual(ciu, new_passenger[,-8], use.text.effects = TRUE, ind.output = 2, ind.inputs = Titanic.voc$FAMILY,
                  target.concept = "FAMILY", target.ciu = meta.top$ciuvals[["FAMILY"]], n.samples = 100))
  cat(ciu.textual(ciu, new_passenger[,-8], use.text.effects = TRUE, ind.output = 2, ind.inputs = Titanic.voc$WEALTH,
                  target.concept = "WEALTH", target.ciu = meta.top$ciuvals[["WEALTH"]], n.samples = 100))
}

# Adult dataset with Random Forest (not caret).
# This is a mixed discrete/continuous input case. For some reason caret RF takes
# very long to train, so we use randomForest library instead, which for the
# moment requires providing a predict.function.
# This data set also requires some pre-processing.
test.adult.rf <- function() {
  adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data',
                      sep = ',', fill = F, strip.white = T, na.strings = c("?","NA"))
  colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education',
                       'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex',
                       'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')

  # For simplicity of this analysis, the following variables are removed:
  # education.num, relationship, fnlwgt, capital.gain and capital.loss
  adult$capital_gain<-NULL
  adult$capital_loss<-NULL
  adult$fnlwgt<-NULL
  adult$education_num<-NULL
  adult$relationship<-NULL

  # Remove all rows with NAs
  adult1 <- stats::na.omit(adult)

  # Convert "chr" type columns into factor to avoid problems with RandomForest
  # classifier later.
  adult1$workclass <- factor(adult1$workclass)
  adult1$education <- factor(adult1$education)
  adult1$marital_status <- factor(adult1$marital_status)
  adult1$occupation <- factor(adult1$occupation)
  adult1$race <- factor(adult1$race)
  adult1$sex <- factor(adult1$sex)
  adult1$native_country <- factor(adult1$native_country)
  adult1$income <- factor(adult1$income)

  # Train with Random Forest, then do CIU.
  rf_adult_model<-randomForest(income~.,data = adult1)
  n.in <- ncol(adult1) - 1
  instance <- adult1[10,1:n.in]
  predict.function <- function(model, inputs) { as.matrix((predict(model, inputs, type = "prob"))) }
  ciu <- ciu(rf_adult_model, income~., adult1, predict.function=predict.function)
  p <- ciu.ggplot.col(ciu, instance, c(1:n.in)); print(p)

  # Textual explanation
  cat(ciu.textual(ciu, instance[,1:n.in], use.text.effects = TRUE, ind.output = 2))

  # Plot effect of every input on the output "2" ('>50K').
  par("cex.lab"=1.5)
  for ( i in 1:ncol(instance[,1:n.in]) )
    ciu.plot(ciu, instance[,1:n.in],i,2, n.points = 200, main = "", ylab="")
  par("cex.lab"=1)
}

# This can be useful at least for ordinary barplots.
# par(mai=c(0.8,1.2,0.4,0.2))

test.all<- function() {
  test.ws()
  test.ciu.iris.lda()
  test.ciu.boston.gbm()
  test.ciu.heart.disease.rf()
  test.ciu.cars.UCI.rf() # Takes about 15 seconds for RF to train
  test.ciu.diamonds.gbm() # Takes something like 2-3 minutes to train but GBM seems to be best by far here.
  test.ciu.titanic.rf() # Takes maybe half minute.
  test.adult.rf() # Takes about half minute.
}
