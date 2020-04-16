# ciu
R implementation of Contextual Importance and Utility for Explainable AI

# Background

This is an R implementation of the Contextual Importance and Utility (CIU) concepts for Explainable AI (XAI). CIU was developed by Kary Främling in his PhD thesis *Learning and Explaining Preferences with Neural Networks for Multiple Criteria Decision Making*, (written in French, title *Modélisation et apprentissage des préférences par réseaux de neurones pour l'aide à la décision multicritère*), available online for instance here: https://tel.archives-ouvertes.fr/tel-00825854/document. It was originally implemented in Matlab. A new Matlab implementation can be found at https://github.com/shulemsi/CIU. A Python implementation can be found at https://github.com/TimKam/py-ciu. However, these three implementations have been developed quite independently from each other so their functionality and features may differ (and probably do differ). 

# What is CIU?

According to current (2020) XAI vocabulary, CIU is a model-agnostic method for producing post-hoc explanations of results of any "black-box" model that takes a set of input variable values, does some processing and produces a set of output variable values. 

* **Contextual Importance (CI)** is a measure of how much an output can change as a function of changes of one (or several) inputs.
* **Contextual Utility (CU)** indicates how favorable the current value of one (or several) inputs is for a high output value. 

The following figure illustrates the principle: 

![Illustration of CIU](/Figures/CIU_illustration.png)

``CI = (Cmax - Cmin)/(absmax - absmin)``

``CU = (out - Cmin)/(Cmax - Cmin)``

For the illustration in the figure, ``CI = 0.5`` for both inputs, whereas ``CU = 0.32`` for ``x`` and ``CU = 0.04 for ``y``. More formal and detailed descriptions can be found in articles on CIU. 

# Running

The example code shown here is also found in the file ciu_scripts.R and can be ran directly. However, the libraries MASS and caret need to be installed for the *lda* and *Random Forest* models. 

After pulling/cloning the files to a directory, start R (I usually do it by double-clicking the file *StartR.RData*). 

To begin with, we will start by training an lda model on the Iris data set. 

``` r
library(MASS)
source("ContextualImportanceUtility.R")
iris_train <- iris[, 1:4]
iris_lab <- iris[[5]][]
model <- lda(iris_train, iris_lab)
```

Then we test with a particular Iris flower instance and set up the CIU explainer. This Iris will clearly be a Virginica if the model learns correctly. What we want to know is why it is a Virginica. For that, we need 
* *in.min.max.limits*: range of possible input values, as a 4x2 matrix that has minimal input value in first column and maximal input value in second column, for all the four Iris inputs.
* *abs.min.max*: range of possible output values, as a 3x2 matrix that has minimal input value in first column and maximal input value in second column, for the three Iris classes. Since this is a classification task, the range is [0,1] for all three outputs. 
* *output.names*: names of the outputs, in this case the names of the three Iris classes.
* *input.names*: this is not necessary here because ciu$explain takes it automatically from the column names of iris_test if it hasn't been specified before (this would not work if iris_test would be a vector, though). 

``` r
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
# Displays something like:
#            CI           CU
# setosa     3.301310e-33 6.419101e-05
# versicolor 2.436947e-02 5.974838e-02
# virginica  2.436947e-02 9.402516e-01
```

To understand better what these values correspond to, we can for instance plot how the output 3 (Virginica) changes as a function of input 3 (Petal Length).

``` r
ciu$plot.CI.CU(iris_test, ind.input=3, ind.output=3)
```
The following figure shows how the outputs for Virginica changes as a function of the four different inputs. 

![Virginica output as a function of one input at a time](/Figures/lda_2D_virginica.png)

CIU can be calculated for any number of inputs (even all inputs). We will now calculate the joint CIU values for "Petal Size", which includes both Petal Length and Petal Width. We also produce a 3D-plot that plot that shows the joint effect on the value of Virginica.
``` r
CI.CU <- ciu$explain(iris_test, ind.inputs.to.explain=c(3,4))
CI.CU
# Displays something like:
#             CI           CU
# setosa      1 1.206792e-37
# versicolor  1 1.399856e-03
# virginica   1 9.986001e-01
ciu$plot.CI.CU.3D(iris_test, ind.inputs=c(3,4), ind.output=3)
```
The following figure shows how the outputs of all three Iris classes change as a function of "Petal Size". 

![Iris class outputs as a function of Petal Size](/Figures/lda_3D_PetalSize.png)

CIU values are just the "raw material" for producing actual explanations, which can be visual, textual, sounds or whatever means of presentation is the most appropriate to the user and the context. A bar plot of the same kind as what is often used for LIME and SHAP, for instance, is produced by the following method call. 
``` r
ciu$barplot.CI.CU(inputs=iris_test, ind.output=3)
```

When doing a barplot for all Iris classes with lda model, it looks like below. 
![Random Forest barplots for Iris](/Figures/lda_ciu_barplot.png)

The code for producing that plot is the following: 
``` r
# Do barplot for all three classes in same Figure
def.par <- par(no.readonly = TRUE); par(mfrow=c(1,3))
for ( iris.ind in 1:length(out.names) ) {
  ciu$barplot.CI.CU(inputs=iris_test, ind.output=iris.ind)
}
par(def.par)
```

A major difference in the plot above compared to those that are usually used with LIME, for instance, is that CIU has two dimensions (**importance** and **utility**) rather than only speaking about *importance* as most (all?) other methods. The length of the bar corresponds to the Contextual Importance (CI) value, while the color changes depending on the Contextual Utility (CU). A "low" CU (<0.5 by default) signifies that the value is defavorable for the output and gives a red bar. A "high" CU (>=0.5 by default) signifies that the value is favorable for the output and gives a green bar. The more (de/)favorable the value, the darker the colour. *lda* tends to produce very sharp class limits, so the color nuances are not well visible in this case. 

Next, we will do the same using a Random Forest model. 
``` r
# Then same with Random Forest
library(caret)
model <- train(iris_train, iris_lab, method = 'rf')
ciu <- ciu.new(model, in.min.max.limits=c.minmax, abs.min.max=abs.min.max, output.names=out.names)
CI.CU <- ciu$explain(iris_test, ind.inputs.to.explain=c(1))
CI.CU
#            CI    CU
# setosa     0.026 0 
# versicolor 0.194 0 
# virginica  0.218 1 
ciu$plot.CI.CU(iris_test, ind.input=3, ind.output=3)
ciu$plot.CI.CU.3D(iris_test, ind.inputs=c(3,4), ind.output=3)
ciu$barplot.CI.CU(inputs=iris_test, ind.output=3)

```
When doing a barplot for all Iris classes with Random Forest model, it looks like below. 
![Random Forest barplots for Iris](/Figures/rf_ciu_barplot.png)

The code for producing that plot is the following: 
``` r
# Do barplot for all three classes in same Figure
def.par <- par(no.readonly = TRUE); par(mfrow=c(1,3))
for ( iris.ind in 1:length(out.names) ) {
  ciu$barplot.CI.CU(inputs=iris_test, ind.output=iris.ind)
}
par(def.par)
```
The following figure shows how the outputs of all three Iris classes change as a function of "Petal Size" with Random Forest classifier. 

![Iris class outputs as a function of Petal Size](/Figures/rf_3D_PetalSize.png)

The following figure shows how the outputs for Virginica changes as a function of the four different inputs with Random Forest classifier. 

![Virginica output as a function of one input at a time with Random Forest](/Figures/rf_2D_virginica.png)


# Author

[Kary Främling](http://github.com/KaryFramling)

