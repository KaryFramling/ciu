# Not found out yet how to sort features according to CI (or CU)
# for all facets, now they are all sorted according to mean value of all CI
# (which might actually be a good choice).

#' CIU feature importance/utility plot using ggplot.
#'
#' Create a barplot showing CI as the length of the bar and CU on color scale from
#' red to green, via yellow, for the given inputs and the given output.
#'
#' @inheritParams ciu.meta.explain
#' @inheritParams ciu.barplot
#' @param output.names Vector with names of outputs to include.
#' If NULL (default), then include all.
#' @param low.color Colour to use for CU=0
#' @param mid.color Colour to use for CU=Neutral.CU
#' @param high.color Colour to use for CU=1
#'
#' @return ggplot object.
#' @export
#' @author Kary Främling
#'
ciu.ggplot.col <- function(ciu, instance=NULL, ind.inputs=NULL, output.names=NULL,
                           in.min.max.limits=NULL,
                           n.samples=100, neutral.CU=0.5,
                           show.input.values=TRUE, concepts.to.explain=NULL,
                           target.concept=NULL, target.ciu=NULL,
                           ciu.meta = NULL,
                           low.color="red", mid.color="yellow",
                           high.color="darkgreen",
                           use.influence=FALSE, influence.minmax = c(-1,1),
                           sort=NULL, decreasing=FALSE, # These are not used yet.
                           main=NULL) {
  # Allow using already existing result.
  if ( is.null(ciu.meta) ) {
    ciu.meta <- ciu.meta.explain(ciu, instance, ind.inputs=ind.inputs, in.min.max.limits=in.min.max.limits,
                                 n.samples=n.samples, concepts.to.explain=concepts.to.explain,
                                 target.concept=target.concept, target.ciu=target.ciu)
  }
  else {
    instance <- ciu.meta$instance
  }

  # Create data frame for ggplot plotting
  ind.inputs <- ciu.meta$ind.inputs
  inp.names <- ciu.meta$inp.names
  ci.cu <- data.frame()
  n.inps <- length(ciu.meta$ciuvals)
  for ( i in 1:n.inps ) {
    f.label <- inp.names[i]
    ciu.res <- ciu.meta$ciuvals[[i]]
    if ( show.input.values ) {
      # Don't really understand why [1,1] is necessary but otherwise factor values display numerically (not as strings).
      f.label <- paste0(f.label, " (", as.character(instance[1,ind.inputs[i]]), ")")
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

  # "instance" has to be a data.frame so this can't be NULL.
  inst.name <- rownames(instance)

  # Sort facets according to output value. Sorting factor levels correctly does the job.
  ci.cu$Output <- factor(ci.cu$Output, unique(ci.cu$Output[order(ci.cu$Output.Value, decreasing = TRUE)]))

  # Check if main plot title has been given as parameter, otherwise use default one
  if ( is.null(main) ) {
    main <- paste("Studied instance (context):", inst.name)
    if  ( !is.null(target.concept) )
      main <- paste0(main, "\nTarget concept is \"", target.concept, "\"")
  }

  # Create the plot. Have to use some tricks here for avoiding warnings either
  # by devtools.check or during execution. Apparently devtools.check
  # doesn't understand attach() explicitly nor done by ggplot
  fl <- ci.cu$feature.labels;
  ci <- ci.cu$CI; cu <- ci.cu$CU
  # Influence plot separated because needs more than trivial manipulations.
  p <- ggplot(ci.cu)
  if ( use.influence ) {
    ci <- (influence.minmax[2] - influence.minmax[1])*ci*(cu - neutral.CU)
    cu <- sign(ci)/2 + 0.5
    #ymin <- -1;
    ymin <- min(ci); ymax <- max(ci)
    # Have to do special treatment here for the case if we have only one "cu"
    # value for all features.
    cumin <- min(cu); cumax <- max(cu)
    # Have to do special treatment here for the case if we have only one "cu"
    # value for all features.
    cumin <- min(cu); cumax <- max(cu)
    if ( cumin == cumax ) {
      if ( cumin == 0 ) cu_col <- low.color
      else if ( cumin == 0.5 ) cu_col <- mid.color
      else cu_col <- high.color
      low.color <- mid.color <- high.color <- cu_col
    }
    else {
      p <- p + ylim(ymin, ymax)
    }
    p <- p + labs(y = expression(phi)) + theme(legend.position="none")
  }
  else {
    # # Have to do special treatment here for the case if all features have same
    # # cu value because ggplot doesn't use color palette correctly in that case.
    # # SKIPPED FOR THE moment because it still won't work if we have only one feature.
    # cumin <- min(cu); cumax <- max(cu)
    # if ( all.equal(cumin, cumax) ) {
    #   cu <- cu + runif(length(cu), -0.1, 0.1)
    #   print(cu)
    # }
    ymin <- 0; ymax <- 1
    p <- p + ylim(ymin, ymax) + labs(y="CI", fill="CU")
  }
  p <- p +
    geom_col(aes(reorder(fl, ci), ci, fill=cu)) +
    scale_fill_gradient2(low=low.color, mid=mid.color, high=high.color, midpoint=neutral.CU) +
    coord_flip() +
    facet_wrap(~Output, labeller=label_both) + # Use scales="free_y" is different ordering for every facet
    ggtitle(main) +
    xlab("Feature")
  return(p)
}
