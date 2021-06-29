# Textual explanations for CIU.

#' Give textual CIU explanation
#'
#' Provide textual CIU explanations as those used in Kary Främling's
#' PhD thesis.
#'
#' @inheritParams ciu.meta.explain
#' @inheritParams ciu.barplot
#' @param n.features Maximal number of features to include in explanation.
#' @param use.text.effects Add bold/italics/colors effects?
#' @param CI.voc Limits and texts to use for CI values.
#' @param CU.voc Limits and texts to use for CU values.
#'
#' @return Text string with explanation.
#' @export
#' @importFrom crayon bold
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
#' # Give textual explanation. Use 'cat' for getting newlines to work.
#' cat(ciu.textual(ciu, iris_test, ind.output = 2))
#' cat(ciu.textual(ciu, iris_test, ind.output = 2, n.features = 2))
#'
#' # Boston housing, GBM model.
#' library(caret)
#' kfoldcv <- trainControl(method="cv", number=10)
#' gbm <- train(medv ~ ., Boston, method="gbm", trControl=kfoldcv)
#' boston.inst <- Boston[370,1:13]
#' ciu <- ciu.new(gbm, medv~., Boston)
#' cat(ciu.textual(ciu, boston.inst,use.text.effects = TRUE))
#' # Customized limits for CI.
#' cat(ciu.textual(ciu, boston.inst,use.text.effects = TRUE,
#'   CI.voc = data.frame(limits=c(0.05,0.1,0.3,0.5,1.0),
#' texts=c("not important","little important", "important","very important",
#'   "extremely important"))))
#'
#' # Intermediate concepts
#' social<-c(1,11,13); usage_type<-c(2,3); chas<-c(4); air_quality<-c(5)
#' housing<-c(6,7); transport<-c(8,9); blacks<-c(12); tax<-c(10)
#' Boston.voc <- list("SOCIAL"=social, "LAND USAGE"=usage_type, "Charles R. dummy"=chas,
#' "Air quality (Nox)"=air_quality, "HOUSING"=housing, "TRANSPORT"=transport,
#' "Prop. of black people"=blacks, "Tax"=tax)
#' ciu <- ciu.new(gbm, medv~., Boston, vocabulary = Boston.voc)
#'
#' # We use `meta.explain` here to avoid differences due to sampling.
#' meta.top <- ciu$meta.explain(boston.inst, concepts.to.explain=names(Boston.voc))
#' cat(ciu.textual(ciu, boston.inst, use.text.effects = TRUE, ciu.meta = meta.top))
#'
#' # Explain intermediate concept utility, using input features (could also
#' # be using other intermediate concepts).
#' cat(ciu.textual(ciu, boston.inst, use.text.effects = TRUE, ind.inputs = Boston.voc$SOCIAL,
#'   target.concept = "SOCIAL", target.ciu = meta.top$ciuvals[["SOCIAL"]]))
#' cat(ciu.textual(ciu, boston.inst, use.text.effects = TRUE, ind.inputs = Boston.voc$HOUSING,
#'   target.concept = "HOUSING", target.ciu = meta.top$ciuvals[["HOUSING"]]))
ciu.textual <- function(ciu, instance=NULL, ind.inputs=NULL, ind.output=1,
                        in.min.max.limits=NULL,
                        n.samples=100, neutral.CU=0.5,
                        show.input.values=TRUE, concepts.to.explain=NULL,
                        target.concept=NULL, target.ciu=NULL,
                        ciu.meta = NULL,
                        sort="CI", n.features = NULL,
                        use.text.effects = FALSE,
                        CI.voc = data.frame(limits=c(0.2,0.4,0.6,0.8,1.0),
                                            texts=c("not important","slightly important",
                                                    "important","very important","extremely important")),
                        CU.voc = data.frame(limits=c(0.2,0.4,0.6,0.8,1.0),
                                            texts=c("very bad","bad","average","good","very good"))) {
  # If there's a target concept, then we need CIU for that also. So best to do
  # it here.
  if ( !is.null(target.concept) ) {
    if ( is.null(target.ciu) ) {
      if ( inherits(ciu, "CIU") )
        ciu.tmp <- ciu$as.ciu()
      else
        ciu.tmp <- ciu
      ind.inps <- ciu.tmp$vocabulary[target.concept][[1]]
      target.ciu <- ciu.explain(ciu, instance, ind.inputs.to.explain=ind.inputs, in.min.max.limits=in.min.max.limits,
                                n.samples=n.samples*10)
    }
  }

  # Allow using already existing result.
  if ( is.null(ciu.meta) ) {
    ciu.meta <- ciu.meta.explain(ciu, instance, ind.inputs=ind.inputs, in.min.max.limits=in.min.max.limits,
                                 n.samples=n.samples, concepts.to.explain=concepts.to.explain,
                                 target.concept=target.concept, target.ciu=target.ciu)
  }
  else {
    instance <- ciu.meta$instance
  }

  # Make sure we have 'ciu' type object (as by definition for ciu.meta).
  ciu <- ciu.meta$ciu

  # "instance" has to be a data.frame so this can't be NULL.
  inst.name <- rownames(instance)

  # Initial setups.
  ind.inputs <- ciu.meta$ind.inputs
  inp.names <- ciu.meta$inp.names

  # Create data frame for textual explanation
  ciu.text <-ciu.list.to.frame(ciu.meta$ciuvals, ind.output)
  ciu.text$ind.inputs <- ind.inputs
  ciu.text$inp.names <- inp.names
  ciu.text$CI.text <- ciu.values.to.text(ciu.text$CI, CI.voc)
  ciu.text$CU.text <- ciu.values.to.text(ciu.text$CU, CU.voc)

  # Sorting?
  if ( sort == "CI")
    ciu.text <- ciu.text[order(ciu.text$CI, decreasing = TRUE),]
  else if ( sort == "CU" )
    ciu.text <- ciu.text[order(ciu.text$CU, decreasing = TRUE),]

  # Limit number of features?
  if ( is.null(n.features) )
    n.features <- nrow(ciu.text)
  else
    n.features <- min(n.features, nrow(ciu.text))

  # Output value utility (this should be improved...)
  if (is.null(target.concept) ) {
    outval <- ciu.text$outval[ind.output]
    abs.min.max <- ciu$abs.min.max;
    amin <- abs.min.max[ind.output,1]; amax <- abs.min.max[ind.output,2]
    out.CU <- (outval - amin)/(amax - amin)
    out.CU.text <- ciu.values.to.text(out.CU, CU.voc)
  }
  else {
    out.CU <- target.ciu[ind.output,"CU"]
    out.CU.text <- ciu.values.to.text(out.CU, CU.voc)
  }

  # Build the explanation.
  if (use.text.effects) out.CU.text <- crayon::bold(out.CU.text)
  if (is.null(target.concept) ) {
    t <- paste0("The value of output '", ciu$output.names[ind.output], "' for instance '", inst.name)
    t <- paste0(t, "' is ", round(outval,3), ", which is ", out.CU.text, " (CU=", round(out.CU,3), ").\n")
  }
  else {
    t <- paste0("The value of intermediate concept '", target.concept, "' for output '",
                ciu$output.names[ind.output], "', with instance '", inst.name,
                "' is ", out.CU.text, " (CU=", round(out.CU,3), ").\n")
  }
  for ( i in 1:n.features ) {
    ci.text <- ciu.text$CI.text[i]; if (use.text.effects) ci.text <- crayon::bold(ci.text)
    cu.text <- ciu.text$CU.text[i]; if (use.text.effects) cu.text <- crayon::bold(cu.text)
    fvalue <- instance[1, ciu.text$ind.inputs[i]]
    if ( is.null(ciu.meta$ind.inputs) ) {
      inds <- ciu$vocabulary[ciu.text$inp.names[i]][[1]]
      if ( length(inds) == 1 )
        fvalue <- instance[1, inds]
    }
    t <- paste0(t, "Feature '", ciu.text$inp.names[i], "' is ", ci.text,
                " (CI=", round(ciu.text$CI[i],3), ") and value ")
    # Different depending on if concept or input. Could still be improved (show
    # value if one-input concept).
    if ( length(fvalue) >= 1 )
      t <- paste0(t, "'", fvalue, "' ")
    t <- paste0(t, "is ", cu.text, " (CU=",
                round(ciu.text$CU[i],3), ").\n")
  }

  return(t)
}

ciu.values.to.text <- function(values, voc) {
  # Default value is last one.
  res <- as.character(matrix(voc$texts[1], nrow = length(values)))
  for ( limit.ind in 2:length(voc$limits) ) {
    res[values > voc$limits[limit.ind-1] & values <= voc$limits[limit.ind]] <- voc$texts[limit.ind]
  }
  return(res)
}

# https://www.r-bloggers.com/2018/07/coloured-output-in-the-r-console/
# library(crayon)
# t <- green('I am a green line ' %+% blue$underline$bold('with a blue substring') %+% yellow$italic(' that becomes yellow and italicised!\n'))
# cat(t)

