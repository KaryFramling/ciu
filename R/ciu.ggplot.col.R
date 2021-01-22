# CIU feature importance/utility plot using ggplot. All parameters are not
# implemented yet in this ggplot version.
# - output.names: vector with names of outputs to include. If NULL (default),
#   then include all.
# - discrete.inputs: temporary parameter for indicating if all inputs are
#   discrete. Should not be needed once "explain" has been updated to deal
#   with both discrete and continuous inputs.
# Also, not found out yet how to sort features according to CI (or CU)
# for all facets, now they are all sorted according to mean value of all CI
# (which might actually be a good choice).
#' ciu.ggplot.col
#'
#' @param ciu blabla
#' @param instance  blabla
#' @param ind.inputs  blabla
#' @param output.names  blabla
#' @param in.min.max.limits  blabla
#' @param n.samples  blabla
#' @param neutral.CU  blabla
#' @param show.input.values  blabla
#' @param concepts.to.explain  blabla
#' @param target.concept  blabla
#' @param target.ciu  blabla
#' @param low.color  blabla
#' @param mid.color  blabla
#' @param high.color  blabla
#' @param use.influence Plot "influence" instead of CI and CU.
#' @param sort  blabla
#' @param decreasing  blabla
#' @param main  blabla
#'
#' @return blabla
#' @export
#' @author Kary Främling
ciu.ggplot.col <- function(ciu, instance, ind.inputs=NULL, output.names=NULL,
                           in.min.max.limits=NULL,
                           n.samples=100, neutral.CU=0.5,
                           show.input.values=TRUE, concepts.to.explain=NULL,
                           target.concept=NULL, target.ciu=NULL,
                           low.color="red", mid.color="yellow",
                           high.color="darkgreen",
                           use.influence=FALSE,
                           sort=NULL, decreasing=FALSE, # These are not used yet.
                           main=NULL) {

  # Little check here to support both ciu and CIU object as parameter
  if ( inherits(ciu, "ciu") ) {
    CIU <- ciu.to.CIU(ciu)
  }
  else {
    CIU <- ciu
    ciu <- CIU$as.ciu()
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
  inst.name <- rownames(instance)

  # Create data frame for ggplot plotting
  ci.cu <- data.frame()
  for ( i in 1:n.inps ) {
    f.label <- inp.names[i]
    if ( explain.concepts )
      expl.inps <- ciu$vocabulary[concepts.to.explain[i]][[1]]
    else
      expl.inps <- c(ind.inputs[i])
    ciu.res <- CIU$explain(instance, ind.inputs.to.explain=expl.inps, in.min.max.limits=in.min.max.limits,
                           n.samples=n.samples,target.concept=target.concept, target.ciu=target.ciu)
    if ( show.input.values ) {
      # Don't really understand why [1,1] is necessary but otherwise factor values display numerically (not as strings).
      f.label <- paste0(f.label, " (", instance[ind.inputs[i]][1,1], ")")
    }

    # Some special treatment here for getting output names correct if result
    # variable is a factor, which leads to as many output classes as levels.
    if ( is.factor(ciu$data.out[,1]) && !is.null(ciu$output.names) ) {
      if ( length(levels(ciu$data.out[,1])) == length(ciu$output.names))
        rownames(ciu.res) <- ciu$output.names
    }

    # Only include the outputs that are indicated to be included,
    # otherwise include all
    if ( !is.null(output.names) ) {
      ciu.res <- ciu.res[row.names(ciu.res) %in% output.names,]
    }
    ci.cu <- rbind(ci.cu, data.frame(Label=rownames(ciu.res), Output.Value=ciu.res$outval,
                                     in.names=inp.names[i], CI=ciu.res$CI, CU=ciu.res$CU,
                                     Output=paste0(rownames(ciu.res), " (",
                                                   format(ciu.res$outval, digits=3), ")"),
                                     feature.labels=f.label)
    )
  }

  # Sort facets according to output value. Sorting factor levels correctly does the job.
  ci.cu$Output <- factor(ci.cu$Output, unique(ci.cu$Output[order(ci.cu$Output.Value, decreasing = TRUE)]))

  # Check if main plot title has been given as parameter, otherwise use default one
  if ( is.null(main) ) {
    main <- paste("Studied instance (context):", inst.name)
    if  ( !is.null(target.concept) )
      main <- paste0(main, "\nTarget concept is \"", target.concept, "\"")
  }

  # Influence plot requires small manipulations.
  ci <- ci.cu$CI; cu <- ci.cu$CU
  ymin <- 0; ymax <- 1
  if ( use.influence ) {
    ci <- ci*2*(cu - neutral.CU)
    cu <- sign(ci)/2 + 0.5
    #ymin <- -1;
    ymin <- min(ci); ymax <- max(ci)
  }

  # Create the plot. Have to use some tricks here for avoiding warnings either
  # by devtools.check or during execution. Apparently devtools.check
  # doesn't understand attach() explicitly nor done by ggplot
  fl <- ci.cu$feature.labels;
  p <- ggplot(ci.cu) +
    geom_col(aes(reorder(fl, ci), ci, fill=cu)) +
    coord_flip() +
    facet_wrap(~Output, labeller=label_both) + # Use scales="free_y" is different ordering for every facet
    ggtitle(main) +
    xlab("Feature") +
    ylim(ymin, ymax) +
    scale_fill_gradient2(low=low.color, mid=mid.color, high=high.color, midpoint=neutral.CU)
  if ( use.influence ) {
    p <- p + theme(legend.position="none")
  }
  return(p)
}
