# CIU with Boston Housing Dataset

The Boston Housing Dataset is a classical benchmark set used in Data Analysis and Machine learning. It is also available in R by default (or happened to be so in my R installation, alternatively in the MASS library. 

A gradient boosting model will be used here for learning the mapping from the 13 input variables to the Median value of owner-occupied homes in $1000's. This is a regression task (not a classification task), which means that the best explanation approach might be different. However, for CIU it doesn't really matter whether it is a classification or a regression task.

gbm is not (yet) supported directly by ``ciu``, so we will provide the ``predict``function to use as an argument to ``ciu.new``. This is a generic way for providing support for "any" regression/classification model. 

```r
require(MASS) # Just in case Boston is not already available
require(gbm)

# We don't care about train/test set in this case because it's not about evaluating training performance.
Boston.boost=gbm(medv ~ . ,data = Boston, distribution = "gaussian", n.trees=10000,
                 shrinkage = 0.01, interaction.depth = 4)
```
Gradient Boosting (gbm) is not yet supported by ``ciu`` implementation, so we need to tell ``ciu`` explicitly what it looks like. 

```r
predict.function <- function(model, inputs) { predict(model,inputs,n.trees=10000) }
```
We initialize and create CIU object and get CI and CU for row 370, which has a high median value (50).
```r
source("ContextualImportanceUtility.R") # Check that R's working directory is set correctly.
n.in <- ncol(Boston) - 1
in.mins <- apply(Boston[,1:n.in], 2, min)
in.maxs <- apply(Boston[,1:n.in], 2, max)
c.minmax <- cbind(in.mins, in.maxs)
out.range <- matrix(c(min(Boston$medv), max(Boston$medv)), ncol=2)
ciu <- ciu.new(Boston.boost, in.min.max.limits=c.minmax, abs.min.max=out.range, 
               input.names=names(Boston)[1:n.in], output.names=names(Boston)[n.in+1], 
               predict.function=predict.function)
inst.ind <- 370
CI.CU <- ciu$explain(Boston[inst.ind,], ind.inputs.to.explain=c(1), montecarlo.samples = 100)
CI.CU # Display result
# Gives CI and CU for the first input variable (crim) and looks something like this: 
#            CI        CU
#medv 0.1597509 0.7998012
```
Then we display the coloured bar plot for the same instance (looks nicer in RStudio than in plain R).

```r
ciu$barplot.CI.CU(Boston[inst.ind,1:n.in])

```
