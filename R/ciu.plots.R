
#' Produce CIU explanations as "long" [data.frame]
#'
#' This function takes a CIU object and calculates CIU values for the whole data
#' set given as parameter to [ciu.new] or given as the `data` parameter.
#'
#' @param CIU CIU object, as created by [ciu.new].
#' @param data Data to use as [data.frame] (default: NULL). If NULL, then use the data of the
#' CIU object, if that exists. This [data.frame] must contain only feature (input)
#' values, not output value(s).
#' @param out.ind Index of output to explain. Default: 1.
#' @param neutral.CU Neutral CU value(s). Default is 0.5.
#'
#' @return [data.frame] of class "ciu.result.long.data.frame".
#' @export
#'
#' @examples
#' # Boston data set with GBM model.
#' library(MASS)
#' library(caret)
#' kfoldcv <- trainControl(method="cv", number=10)
#' gbm <- caret::train(medv ~ ., Boston, method="gbm", trControl=kfoldcv)
#' ciu <- ciu.new(gbm, medv~., Boston)
#' df <- ciu.explain.long.data.frame(ciu)
#' head(df)
#' # Only get results for a part of the data set.
#' ciu.explain.long.data.frame(ciu, data=subset(Boston[1:10,], select=-medv))
ciu.explain.long.data.frame <- function(CIU, data=NULL, out.ind=1, neutral.CU = 0.5) {
  # Get everything that we need from CIU object
  ciu <- CIU$as.ciu()
  model <- ciu$model
  if ( is.null(data) )
    data <- ciu$data.in
  mins <- apply(data, 2, min)
  maxs <- apply(data, 2, max)
  ranges <- maxs - mins
  m <- matrix(0, nrow=nrow(data)*ncol(data), ncol=8)
  vars <- rep(colnames(data), nrow(data))
  start.i <- 1
  end.i <- ncol(data)
  for ( i in 1:nrow(data) ) {
    meta <- CIU$meta.explain(data[i,])
    ciuvals <- ciu.list.to.frame(meta$ciuvals, out.ind)
    m[start.i:end.i,1] <- as.numeric(ciuvals$CI)
    m[start.i:end.i,2] <- as.numeric(ciuvals$CU)
    m[start.i:end.i,3] <- ciu.contextual.influence(ciuvals, neutral.CU = neutral.CU)
    m[start.i:end.i,4] <- as.numeric(data[i,])
    m[start.i:end.i,5] <- as.numeric((data[i,]-mins)/ranges)
    m[start.i:end.i,6] <- as.numeric(ciuvals$cmin)
    m[start.i:end.i,7] <- as.numeric(ciuvals$cmax)
    m[start.i:end.i,8] <- as.numeric(ciuvals$outval)
    start.i <- start.i + ncol(data); end.i <- end.i + ncol(data)
  }
  df <- data.frame(Feature=vars, CI=m[,1], CU=m[,2], Influence=m[,3], Value=m[,4],
                   Norm.Value=m[,5], Cmin=m[,6], Cmax=m[,7], Outvalue=m[,8])
  class(df)<-c("ciu.result.long.data.frame", class(df))
  return(df)
}

#' Create beeswarm-type visualisation.
#'
#' @param data A [data.frame] with CIU (or other) results that has to have
#' at least the columns:
#' - Feature: Feature name.
#' - The CI, CU, influence, whatever actual values to plot.
#' - Norm.Value: Normalized feature values. This can be omitted.
#' Such a [data.frame] is returned by [ciu.explain.long.data.frame], from which
#' the "non-relevant" columns have to be removed, however (see examples).
#' @param target.columns Character vector with names of the columns to use:
#' - Column with feature names.
#' - Column with actual importance/influence/whatever values to plot.
#' - Column with normalized values to use for determining color. If omitted, then
#'   the plot is produced without the colours.
#' Default: c("Feature", "CI", "Norm.Value").
#'
#' @return `ggplot` object
#' @export
#'
#' @examples
#' # Boston data set with GBM model.
#' library(MASS)
#' library(caret)
#' library(ggbeeswarm)
#' kfoldcv <- trainControl(method="cv", number=10)
#' gbm <- caret::train(medv ~ ., Boston, method="gbm", trControl=kfoldcv)
#' ciu <- ciu.new(gbm, medv~., Boston)
#' df <- ciu.explain.long.data.frame(ciu)
#' p <- ciu.plots.beeswarm(df); print(p)
#' p <- ciu.plots.beeswarm(df, c("Feature","CU","Norm.Value")); print(p)
#' p <- ciu.plots.beeswarm(df, c("Feature","Influence","Norm.Value")); print(p)
#'
#' # Plot without normalized values.
#' p <- ciu.plots.beeswarm(df, c("Feature","Influence")); print(p)
#'
#' # Shapley value-compatible reference value
#' mean.utility <- (mean(Boston$medv)-min(Boston$medv))/(max(Boston$medv)-min(Boston$medv))
#' df <- ciu.explain.long.data.frame(ciu, neutral.CU=mean.utility)
#' p <- ciu.plots.beeswarm(df, c("Feature","Influence","Norm.Value")); print(p)
ciu.plots.beeswarm <- function(data, target.columns=c("Feature", "CI", "Norm.Value")) {
  if ( length(target.columns) > 2 ) {
    p <- ggplot(data, aes(x=.data[[target.columns[1]]], y=.data[[target.columns[2]]],
                        color=.data[[target.columns[3]]])) +
      scale_color_gradient(low="blue", high="red", limits=c(0,1), breaks=c(0,1),
                           labels=c("Low", "High")) +
      labs(color = "Value") #+
      #theme(legend.position="top")
  }
  else{
    p <- ggplot(data, aes(x=.data[[target.columns[1]]], y=.data[[target.columns[2]]]))
  }
  p <- p +
    ggbeeswarm::geom_quasirandom() +
    coord_flip()
  return(p)
}

# For Boston, the Shapley-compatible neutral.CU value should be:
# (mean(Boston$medv)-min(Boston$medv))/(max(Boston$medv)-min(Boston$medv)) = 0.3896179

# library(MASS)
# kfoldcv <- trainControl(method="cv", number=10)
# gbm <- caret::train(medv ~ ., Boston, method="gbm", trControl=kfoldcv)
# indata <- subset(Boston, select=-medv)
# mins <- apply(indata, 2, min)
# maxs <- apply(indata, 2, max)
# ranges <- maxs - mins
#
# library(lime)
# explainer <- lime(Boston, gbm)
# # We have to do this in a loop for getting the values correctly, also because
# # for most instances "chas" doesn't get a value.
# #explanation <- lime::explain(indata, explainer, n_features=ncol(indata))
# #p <- lime::plot_features(explanation);print(p)
# #ldf <- data.frame(Feature=explanation$feature, Phi=explanation$feature_weight)
# #lp <- ciu.plots.beeswarm(ldf, "Phi"); print(lp)
# e <- lime::explain(indata[1,], explainer, n_features=ncol(indata))
# f <- e$feature
# result <- cbind(e, data.frame(Norm.Value=as.numeric((indata[1,f]-mins[f])/ranges[f])))
# for ( i in 2:nrow(indata) ) {
#   e <- lime::explain(indata[i,], explainer, n_features=ncol(indata))
#   f <- e$feature
#   r <- cbind(e, data.frame(Norm.Value=as.numeric((indata[i,f]-mins[f])/ranges[f])))
#   result <- rbind(result, r)
# }
# # We leave out "chas" completely because we don't get result for most instances
# # and it essentially destroys whole plot to include it.
# result <- result[result$feature!="chas",]
# lp <- ciu.plots.beeswarm(result, c("feature", "feature_weight", "Norm.Value")); print(lp)
#
# library(iml)
# predictor <- Predictor$new(gbm, data = indata, y = as.numeric(Boston$medv))
# shapley <- Shapley$new(predictor, x.interest = indata[1,])
# result <- cbind(shapley$results, data.frame(Norm.Value=as.numeric((indata[1,]-mins)/ranges)))
# for ( i in 2:nrow(indata) ) {
#   shapley <- Shapley$new(predictor, x.interest = indata[i,])
#   r <- cbind(shapley$results, data.frame(Norm.Value=as.numeric((indata[i,]-mins)/ranges)))
#   result <- rbind(result, r)
# }
# sp <- ciu.plots.beeswarm(result, c("feature", "phi", "Norm.Value")); print(sp)

