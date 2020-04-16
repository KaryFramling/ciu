library(MASS)
source("ContextualImportanceUtility.R")
iris_train <- iris[, 1:4]
iris_lab <- iris[[5]][]
model <- lda(iris_train, iris_lab)
# The hypothetical Iris that we have been using in CIU tests.
iris_test <- iris_train[1,] # Just to initialize
iris_test[1,]<-c(7, 3.2, 6, 1.8)
in.mins <- apply(iris_train, 2, min)
in.maxs <- apply(iris_train, 2, max)
c.minmax <- cbind(in.mins, in.maxs)
out.names <- levels(iris_lab)
abs.min.max <- matrix(c(0,1,0,1,0,1), ncol = 2, byrow = T)
ciu <- ciu.new(model, in.min.max.limits=c.minmax, abs.min.max=abs.min.max, output.names=out.names)
CI.CU <- ciu$explain(iris_test, ind.inputs.to.explain=c(1))
CI.CU
# Displays
# CI           CU
# setosa     3.301310e-33 6.419101e-05
# versicolor 2.436947e-02 5.974838e-02
# virginica  2.436947e-02 9.402516e-01
ciu$plot.CI.CU(iris_test, ind.input=3, ind.output=3)
ciu$plot.CI.CU.3D(iris_test, ind.inputs=c(3,4), ind.output=3)
ciu$barplot.CI.CU(inputs=iris_test, ind.output=3)

# Then same with Random Forest
library(caret)
model <- train(iris_train, iris_lab, method = 'rf')
ciu <- ciu.new(model, in.min.max.limits=c.minmax, abs.min.max=abs.min.max, output.names=out.names)
CI.CU <- ciu$explain(iris_test, ind.inputs.to.explain=c(1))
CI.CU
# CI    CU
# setosa     0.026 0 
# versicolor 0.194 0 
# virginica  0.218 1 
ciu$plot.CI.CU(iris_test, ind.input=3, ind.output=3)
ciu$plot.CI.CU.3D(iris_test, ind.inputs=c(3,4), ind.output=3)
ciu$barplot.CI.CU(inputs=iris_test, ind.output=3)

