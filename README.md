# ciu
R implementation of Contextual Importance and Utility for Explainable AI

<h1>Background</h1>

This is an R implementation of the Contextual Importance and Utility (CIU) concepts for Explainable AI (XAI). CIU was developed by Kary Främling in his PhD thesis *Learning and Explaining Preferences with Neural Networks for Multiple Criteria Decision Making*, (written in French, title *Modélisation et apprentissage des préférences par réseaux de neurones pour l'aide à la décision multicritère*. 

<h1>What is CIU?</h1>

According to current (2020) XAI vocabulary, CIU is a model-agnostic method for producing post-hoc explanations of results of any "black-box" model that takes a set of input variable values, does some processing and produces a set of output variable values. 
*There will be formulas or similar here!*

<h1>Running</h1>

After pulling the files to a directory, start R (I usually do it by double-clicking the file *StartR.RData*. Then execute the code below.

``` r
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

```
