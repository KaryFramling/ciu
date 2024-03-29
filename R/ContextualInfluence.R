# Functions related to Contextual infuence
#
#

#' Get Contextual influence values
#'
#' Contextual influence is calculated from CI and CU values, using a baseline
#' that is considered a "neutral" CU value.
#'
#' @param ciu.result [data.frame] with CI and CU columns. If NULL, then CI and CU values must be
#' provided.
#' @param CI CI value(s).
#' @param CU CU value(s).
#' @param neutral.CU Neutral CU value(s). Default is 0.5
#'
#' @return Contextual influence value(s). Value or vector.
#' @export
#' @author Kary Främling
ciu.contextual.influence <- function(ciu.result = NULL, CI=NULL, CU=NULL, neutral.CU=0.5) {
  if ( is.null(ciu.result) ) {
    ci <- CI
    cu <- CU
  }
  else {
    ci <- ciu.result$CI
    cu <- ciu.result$CU
  }
  return(ci*(cu - neutral.CU))
}

#' Create a contrastive explanation between two instances
#'
#' @param ciu.result1 [ciu.result] of first instance.
#' @param ciu.result2 [ciu.result] of second instance.
#' @param neutral.CU Baseline value(s) `phi0` to use for calculating Contextual
#' influence.
#'
#' @return Contrastive influence values, where CU values of second instance are used as
#' baseline for first instance.
#' @export
#' @examples
#' library(ciu)
#' library(MASS)
#' test.ind <- 100
#' iris_test <- iris[test.ind, 1:4]
#' iris_train <- iris[-test.ind, 1:4]
#' iris_lab <- iris[[5]][-test.ind]
#' model <- lda(iris_train, iris_lab)
#' # Create CIU object
#' ciu <- ciu.new(model, Species~., iris)
#' # First case: why is this a versicolor and not a virginica?
#' meta <- ciu$meta.explain(iris_test)
#' ciuvals.versicolor <- ciu.list.to.frame(meta$ciuvals, out.ind = 2)
#' ciuvals.virginica <- ciu.list.to.frame(meta$ciuvals, out.ind = 3)
#' # Show contextual influence values for both output classes.
#' cinfl.versicolor <- ciu.contextual.influence(ciuvals.versicolor) # Default neutral.CU=0.5
#' ciu.contextual.influence(CI = ciuvals.versicolor$CI, CU = ciuvals.versicolor$CU,
#' neutral.CU = 0)
#' cinfl.virginica <- ciu.contextual.influence(ciuvals.virginica)
#' # Now the contrastive part:
#' why.versicolor.not.virginica <- ciu.contextual.influence(ciuvals.versicolor, neutral.CU=(cinfl.virginica+0.5))
#' @author Kary Främling
ciu.contrastive <- function(ciu.result1, ciu.result2, neutral.CU=0.5) {
  infl2 <- ciu.contextual.influence(ciu.result2, neutral.CU=neutral.CU)
  contrastive.influences <- ciu.contextual.influence(ciu.result1, neutral.CU=(infl2 + neutral.CU))
  #res <- ciu.contrastive.result.new(contrastive.influences, ciu.result1,ciu.result2, neutral.CU)
  return(contrastive.influences)
}

# #' Return ciu.contrastive.result object
# #'
# #' @param contrastive.influences [data.frame] with one column for contrastive influence
# #' values.
# #' @param ciu.result1 ciu.result object as returned by [ciu.result.new] of explained instance.
# #' @param ciu.result2  ciu.result object as returned by [ciu.result.new] of instance
# #' compared with.
# #' @param neutral.CU neutral.CU value(s) used for calculating influence values of
# #' instance compared with.
# #'
# #' @return An object of class `ciu.contrastive.result` (which is of class [list])
# #' that has elements that correspond to the parameters.
# #' @export
# #' @author Kary Främling
# ciu.contrastive.result.new <- function(contrastive.influences, ciu.result1, ciu.result2, neutral.CU) {
#   ciu.contrastive.result <- list(contrastive.influences=contrastive.influences,
#                                  ciu.result1=ciu.result1, ciu.result2=ciu.result2,
#                                  neutral.CU=neutral.CU)
#   class(ciu.contrastive.result)<-c("ciu.contrastive.result","list")
#   return(ciu.contrastive.result)
# }

#' Create contrastive ggplot
#'
#' @param ciu.meta.result [ciu.meta.result] object for the instance to be explained,
#' i.e. the first instance parameter of [ciu.contrastive].
#' @param contrastive.influences Contrastive influence values, as normally returned
#' by [ciu.contrastive].
#' @param instance.names Vector with the labels to be used for the compared
#' classes/instances. If NULL, then we use `c("instance One", "instance Two")`.
#' @param question What kind of explanation do we answer. Can be "Why?" and
#' "Why not?". Default is "Why?".
#' @param negative.color Color to use for negative influence. Default is red.
#' @param positive.color Color to use for positive influence. Default is darkgreen.
#'
#' @return [ggplot] object.
#' @export
#' @author Kary Främling
#' @examples
#' library(ciu)
#' library(MASS)
#' test.ind <- 100
#' iris_test <- iris[test.ind, 1:4]
#' iris_train <- iris[-test.ind, 1:4]
#' iris_lab <- iris[[5]][-test.ind]
#' model <- lda(iris_train, iris_lab)
#' # Create CIU object
#' ciu <- ciu.new(model, Species~., iris)
#' # First case: why is this a versicolor and not a virginica?
#' meta <- ciu$meta.explain(iris_test)
#' ciuvals.versicolor <- ciu.list.to.frame(meta$ciuvals, out.ind = 2)
#' ciuvals.virginica <- ciu.list.to.frame(meta$ciuvals, out.ind = 3)
#' # Now the contrastive part:
#' contrastive <- ciu.contrastive(ciuvals.versicolor, ciuvals.virginica)
#' print(ciu.ggplot.contrastive(meta, contrastive, c("Versicolor", "Virginica")))
#' # Then a "Why not?" explanation
#' contrastive.neg <- ciu.contrastive(ciuvals.virginica, ciuvals.versicolor)
#' print(ciu.ggplot.contrastive(meta, contrastive.neg, question = "Why not?", c("Virginica", "Versicolor")))
ciu.ggplot.contrastive <- function(ciu.meta.result, contrastive.influences,
                                   instance.names = NULL, question="Why?", negative.color="red",
                                   positive.color="darkgreen") {
  if ( is.null(instance.names) ) {
    instance.names <- c("instance One", "instance Two")
  }
  Influence <- rep("Negative", length(contrastive.influences))
  Influence[contrastive.influences >= 0] <- "Positive"
  data <- data.frame(feature.name=ciu.meta.result$inp.names, influence=contrastive.influences)
  p <- ggplot(data)
  if ( question == "Why?" ) {
    main <- paste("Why", instance.names[1], "and not", instance.names[2], "?")
    p <- p + geom_col(aes(reorder(feature.name, influence), influence, fill=Influence))
  }
  else if ( question == "Why not?" ) {
    main <- paste("Why not", instance.names[1], "rather than", instance.names[2], "?")
    p <- p + geom_col(aes(reorder(feature.name, -influence), influence, fill=Influence))
  }
  else
    main <- "Contrastive explanation based on Contextual influence"
  p <- p +
    labs(x="Feature", y = expression(phi)) + #theme(legend.position="none") +
    scale_fill_manual(values = c("Negative"=negative.color, "Positive"=positive.color)) +
    coord_flip() +
    ggtitle(main)
  return(p)
}

