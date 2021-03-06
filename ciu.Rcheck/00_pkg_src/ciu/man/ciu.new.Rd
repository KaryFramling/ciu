% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ContextualImportanceUtility.R
\name{ciu.new}
\alias{ciu.new}
\title{Create CIU object}
\usage{
ciu.new(
  bb,
  formula = NULL,
  data = NULL,
  in.min.max.limits = NULL,
  abs.min.max = NULL,
  input.names = NULL,
  output.names = NULL,
  predict.function = NULL,
  vocabulary = NULL
)
}
\arguments{
\item{bb}{Model/"black-box" object. At least all \code{caret} models, the
\code{lda} model from MASS, and the \code{lm} model are supported.
Otherwise, the prediction
function to be used can be gives as value of the \code{predict.function}
parameter.
A more powerful way is to inherit from FunctionApproximator class and
implement an "eval" method.}

\item{formula}{Formula that describes input versus output values. Only to
be used together with \code{data} parameter.}

\item{data}{The training data used for training the model. If this parameter
is provided, a \code{formula} MUST be given also.
\code{ciu.new} attempts to infer the other parameters from
\code{data} and \code{formula}. i.e. \code{in.min.max.limits},
\code{abs.min.max}, \code{input.names}
and \code{output.names}. If those parameters are provided, then they
override the inferred ones.}

\item{in.min.max.limits}{matrix with one row per output and two columns,
where the first column indicates the minimal value and the second column
the maximal value for that input.}

\item{abs.min.max}{\link{data.frame} or \link{matrix} of min-max values of
outputs, one row per output, two columns (min, max).}

\item{input.names}{labels of inputs.}

\item{output.names}{labels of outputs.}

\item{predict.function}{can be supplied if a model that is not supported by
ciu should be used. As an example, this is the function for lda:\preformatted{
o.predict.function <- function(model, inputs) \{
    pred <- predict(model,inputs)
        return(pred$posterior)
\}}}

\item{vocabulary}{list of labels/concepts to be used when producing
explanations and what combination of inputs they correspond to. Example of
two intermediate concepts and a higher-level one that combines them:
\code{list(intermediate.concept1=c(1,2,3), intermediate.concept2=c(4,5), higher.level.concept=c(1,2,3,4,5))}}
}
\value{
Object of class \code{CIU}.
}
\description{
Sets up a CIU object with the given parameters. CIU objects have "public"
and "private" methods. A CIU object is actually a \link{list} whose elements
are the public functions (methods).
}
\examples{
# Explaining the classification of an Iris instance with lda model.
# We use a versicolor (instance 100).
library(MASS)
test.ind <- 100
iris_test <- iris[test.ind, 1:4]
iris_train <- iris[-test.ind, 1:4]
iris_lab <- iris[[5]][-test.ind]
model <- lda(iris_train, iris_lab)

# Create CIU object
ciu <- ciu.new(model, Species~., iris)

# This can be used with explain method for getting CIU values
# of one or several inputs. Here we get CIU for all three outputs
# with input feature "Petal.Length" that happens to be the most important.
ciu$explain(iris_test, 1)

# It is, however, more convenient to use one of the graphical visualisations.
# Here's one using ggplot.
ciu$ggplot.col.ciu(iris_test)

# LDA creates very sharp class limits, which can also be seen in the CIU
# explanation. We can study what the underlying model looks like using
# plot.ciu and plot.ciu.3D methods. Here is a 3D plot for all three classes
# as a function of Petal Length&Width. Iris #100 (shown as the red dot)
# is on the ridge of the "versicolor" class, which is quite narrow for
# Petal Length&Width.
par(mfrow=c(1,3))
ciu$plot.ciu.3D(iris_test,c(3,4),1,main=levels(iris$Species)[1],)
ciu$plot.ciu.3D(iris_test,c(3,4),2,main=levels(iris$Species)[2])
ciu$plot.ciu.3D(iris_test,c(3,4),3,main=levels(iris$Species)[3])
par(mfrow=c(1,1))

# Same thing with a regression task, the Boston Housing data set. Instance
# #370 has the highest valuation (50k$). Model is gbm, which performs
# decently here. Plotting with "standard" bar plot this time.
library(caret)
gbm <- train(medv ~ ., Boston, method="gbm", trControl=trainControl(method="cv", number=10))
ciu <- ciu.new(gbm, medv~., Boston)
ciu$barplot.ciu(Boston[370,1:13])

# Same but sort by CI.
ciu$barplot.ciu(Boston[370,1:13], sort = "CI")

}
\seealso{
\code{\link{explain}} method

\code{\link{barplot.ciu}} method
}
\author{
Kary Främling
}
