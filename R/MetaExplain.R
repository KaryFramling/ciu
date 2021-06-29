#' CIU meta-result object
#'
#' Create object of class `ciu.meta.result`, which stores results of CIU
#' calculations together with their "meta-data".
#' The [ciu.meta.explain()] method returns a `ciu.meta.result` object.
#'
#' @inheritParams ciu.meta.explain
#' @param ciuvals List of `ciu.result` objects, one per input feature.
#' @param inp.names Names of the input features.
#'
#' @return An object of class ciu.meta.result, which is a [list] with
#' same elements as the given parameters.
#'
#' @export
#' @author Kary Främling
ciu.meta.result.new <- function(ciu, instance, ciuvals, ind.inputs=NULL, inp.names=NULL,
                                in.min.max.limits=NULL, n.samples=NULL,
                                target.concept=NULL, target.ciu=NULL) {
  ciu.meta.result <- list(ciu = ciu, instance = instance,
                          ciuvals = ciuvals, ind.inputs = ind.inputs, inp.names = inp.names,
                          in.min.max.limits = in.min.max.limits,
                          n.samples = n.samples,
                          target.concept = target.concept, target.ciu = target.ciu)
  class(ciu.meta.result)<-c("ciu.meta.result","list")
  return(ciu.meta.result)
}

#' ciu.meta.explain
#'
#' @inheritParams ciu.explain
#' @param ind.inputs Indices of input features to explain (the set {i} in CIU
#' formulae)
#' @param concepts.to.explain List of input feature concepts to explain, as defined
#' by vocabulary provided as argument to [ciu.new]. If `ind.inputs=NULL`,
#' then use `concepts.to.explain` instead. If both are `NULL`, then use all inputs.
#'
#' @return An object of class \code{ciu.meta.result}.
#' @export
#' @examples
#' # Explaining the classification of an Iris instance with lda model.
#' # We use a versicolor (instance 100).
#' library(MASS)
#' test.ind <- 100
#' iris_test <- iris[test.ind, 1:4]
#' iris_train <- iris[-test.ind, 1:4]
#' iris_lab <- iris[[5]][-test.ind]
#' model <- lda(iris_train, iris_lab)
#'
#' # Create CIU object
#' ciu <- ciu.new(model, Species~., iris)
#'
#' # Get ciu.meta.result. This can either be 'ciu$meta.explain(...)'
#' # or 'ciu.meta.explain(ciu, ...)'
#' ciu.meta <- ciu$meta.explain(iris_test)
#'
#' # Use same result for different visualisations.
#' ciu$ggplot.col.ciu(ciu.meta = ciu.meta)
#' ciu$barplot.ciu(ind.output = 2, ciu.meta = ciu.meta)
#' ciu$pie.ciu(ind.output = 2, ciu.meta = ciu.meta)
#'
#' # Same with Boston Housing data set.
#' library(caret)
#' gbm <- train(medv ~ ., Boston, method="gbm", trControl=trainControl(method="cv", number=10))
#' ciu <- ciu.new(gbm, medv~., Boston)
#' instance <- Boston[370,1:13]
#' ciu.meta <- ciu$meta.explain(instance)
#' ciu$barplot.ciu(ciu.meta = ciu.meta, sort = "CI")
#' ciu$pie.ciu(ciu.meta = ciu.meta)
#' ciu$ggplot.col.ciu(ciu.meta = ciu.meta)
#'
#' @author Kary Främling
ciu.meta.explain <- function(ciu, instance, ind.inputs=NULL, in.min.max.limits=NULL,
                             n.samples=100, concepts.to.explain=NULL,
                             target.concept=NULL, target.ciu=NULL) {
  # Little check here to support both ciu and CIU object as parameter
  CIU <- ciu
  if ( inherits(ciu, "ciu") ) {
    CIU <- ciu.to.CIU(ciu)
  }
  else {
    ciu <- ciu$as.ciu()
  }

  # Check if concepts are to be explained or pure inputs.
  # If no input indices are given, then use all inputs
  explain.concepts <- FALSE
  if ( !is.null(concepts.to.explain) ) {
    explain.concepts <- TRUE
    inp.names <- concepts.to.explain
  }
  else {
    if ( is.null(ind.inputs) ) {
      if ( is.null(dim(instance)) )
        ind.inputs <- 1:length(instance)
      else
        ind.inputs <- 1:ncol(instance)
    }
    inp.names <- names(instance)[ind.inputs]
  }
  n.inps <- length(inp.names)

  # Again, "instance" has to be a data.frame so this can't be NULL.
#  inst.name <- rownames(instance)

  # Create data frame for ggplot plotting
  ciuvals <- list()
  for ( i in 1:n.inps ) {
    if ( explain.concepts )
      expl.inps <- ciu$vocabulary[concepts.to.explain[i]][[1]]
    else
      expl.inps <- c(ind.inputs[i])
    ciuvals[[i]] <- CIU$explain(instance, ind.inputs.to.explain=expl.inps, in.min.max.limits=in.min.max.limits,
                           n.samples=n.samples, target.concept=target.concept, target.ciu=target.ciu)
  }
  names(ciuvals) <- inp.names

  ciu.meta.result <- ciu.meta.result.new(ciu = CIU$as.ciu(), instance = instance,
                                         ciuvals = ciuvals,
                                         ind.inputs = ind.inputs, inp.names = inp.names,
                                         in.min.max.limits = in.min.max.limits,
                                         n.samples = n.samples,
                                         target.concept = target.concept, target.ciu = target.ciu)
  return(ciu.meta.result)
}

#' ciu.list.to.frame
#'
#' Convert [list] of ciu.result objects into corresponding [data.frame] for
#' given output.
#'
#' @param ciu.list [list] of ciu.result objects.
#' @param out.ind Index of output to extract.
#'
#' @return [data.frame] with same columns as ciu.result object but with one row
#' per input feature.
#' @export
#'
#' @author Kary Främling
ciu.list.to.frame <- function(ciu.list, out.ind = 1) {
  dummy <- ciu.list[[1]]
  df <- data.frame(matrix(unlist(lapply(ciu.list, "[", out.ind, )), ncol=ncol(dummy), byrow = TRUE))
  names(df) <- names(dummy)
  return(df)
}

