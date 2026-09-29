# Not found out yet how to sort features according to CI (or CU)
# for all facets, now they are all sorted according to mean value of all CI
# (which might actually be a good choice).

# This is to get the CRAN check to go through. Thanks ChatGPT.
# These are variables used in ggplot internally that are apparently not
# understood correctly by rcheck.
utils::globalVariables(c("CI", "CU", "cu_scaled", "feature.labels", "phi",
                         "Positive.Phi", "feature.name"))

#' CIU feature importance/utility plot using ggplot.
#'
#' Create a barplot showing CI as the length of the bar and CU on color scale from
#' red to green, via yellow, for the given inputs and the given output.
#'
#' @inheritParams ciu.meta.explain
#' @inheritParams ciu.barplot
#' @param output.names Vector with names of outputs to include.
#' If NULL (default), then include all.
#' @param plot.mode "overlap" or "colour_cu". Default is "colour_cu".
#' @param ci.colours Colours to use for CI part in "overlap" mode. Three values
#' required: fill colour, border colour, alpha. Default is c("aquamarine", "aquamarine3", "0.3").
#' @param cu.colours Colours to use for CU part in "overlap" mode. Three values
#' required: fill colour, border colour, alpha. Default is c("darkgreen", "darkgreen", "0.8").
#' If it is set to NULL, then the same colour palette is used as for "colour_cu".
#' @param low.color Colour to use for CU=0
#' @param mid.color Colour to use for CU=Neutral.CU
#' @param high.color Colour to use for CU=1
#' @param scale.CI Scale x-axis according to maximal CI value.
#' @param sort Ordering of the feature rows. NULL (default) orders by CI for
#' CI/CU plots and by signed influence for influence plots, which reproduces the
#' historical behaviour. Otherwise one of "CI", "CU", "phi" (signed influence) or
#' "absphi" (absolute influence). Note that "phi" places a feature with large
#' negative influence at the opposite end from one with large positive influence,
#' regardless of importance; "absphi" orders by magnitude instead.
#' @param decreasing If FALSE (default), the largest value is at the top of the
#' plot. TRUE puts the smallest at the top.
#' @param row.order Explicit row order as a character vector of feature labels,
#' bottom to top. Overrides `sort` and `decreasing`. Useful for showing two
#' panels (e.g. CI/CU and influence) with rows aligned, so that a feature can be
#' tracked across them. Labels must match those used in the plot, i.e. they
#' include the input value when `show.input.values=TRUE`.
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
                           plot.mode = "colour_cu", # overlap or colour_cu
                           ci.colours = c("aquamarine", "aquamarine3", "0.3"),
                           cu.colours = c("darkgreen", "darkgreen", "0.8"),
                           low.color="red", mid.color="yellow",
                           high.color="darkgreen",
                           use.influence=FALSE,
                           scale.CI=FALSE,
                           sort=NULL, decreasing=FALSE,
                           row.order=NULL,
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

  # `neutral.CU` may be a single reference CU for all features, or one value per
  # feature. Influence is assembled feature by feature in the loop below, where
  # `ciu.res` has one row per OUTPUT, so a vector of any other length would
  # recycle over outputs instead of features and produce meaningless values.
  if ( length(neutral.CU) != 1 && length(neutral.CU) != n.inps )
    stop("neutral.CU must have length 1 or the number of features (", n.inps,
         "), not ", length(neutral.CU), ".")

  for ( i in 1:n.inps ) {
    # Reference CU for this feature.
    neutral.CU.i <- if ( length(neutral.CU) > 1 ) neutral.CU[i] else neutral.CU
    f.label <- inp.names[i]
    ciu.res <- ciu.meta$ciuvals[[i]]
    if ( show.input.values ) {
      # Didn't manage to get this done very elegantly...
      value <- instance[ind.inputs[i]]
      if ( is.data.frame(value) ) { # Crazy checks...
        if ( ncol(value) > 0 ) # For intermediate concepts that have no value.
          value <- value[[1]]
        else
          value <- ""
      }
      if ( is.numeric(value) )
        value <- format(value, digits=2)
      f.label <- paste(f.label, " (", value, ")", sep="")
      #f.label <- paste0(f.label, " (", as.character(instance[1,ind.inputs[i]]), ")")
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
                                     phi=ciu.contextual.influence(ciu.res, neutral.CU=neutral.CU.i),
                                     cu_scaled=ciu.res$CI*ciu.res$CU,
                                     Output=paste0(rownames(ciu.res), " (",
                                                   format(ciu.res$outval, digits=3), ")"),
                                     feature.labels=f.label)
    )
  }
  # Sort facets according to output value. Sorting factor levels correctly does the job.
  ci.cu$Output <- factor(ci.cu$Output, unique(ci.cu$Output[order(ci.cu$Output.Value, decreasing = TRUE)]))

  # Only needed for influence bar coloring.
  ci.cu$Positive.Phi <- ci.cu$phi >= 0

  # ---- Feature row order -----------------------------------------------------
  # With several outputs a feature label appears once per facet, so the order has
  # to be decided from an aggregate over facets. The mean is used, which is what
  # reorder() did implicitly before this was made explicit.
  if ( !is.null(row.order) ) {
    missing.labels <- setdiff(ci.cu$feature.labels, row.order)
    if ( length(missing.labels) > 0 )
      warning("row.order does not cover these feature labels, which will be ",
              "dropped from the plot: ", paste(missing.labels, collapse=", "))
    row.levels <- row.order
  }
  else {
    sort.key <- if ( is.null(sort) ) {
      # Historical default: CI for CI/CU plots, signed influence for influence plots.
      if ( use.influence ) ci.cu$phi else ci.cu$CI
    }
    else {
      switch(sort,
             "CI"     = ci.cu$CI,
             "CU"     = ci.cu$CU,
             "phi"    = ci.cu$phi,
             "absphi" = abs(ci.cu$phi),
             stop("sort must be NULL, \"CI\", \"CU\", \"phi\" or \"absphi\", not \"",
                  sort, "\"."))
    }
    aggregated <- tapply(sort.key, ci.cu$feature.labels, mean)
    row.levels <- names(sort(aggregated))
    # Levels run bottom to top after coord_flip(), so ascending levels put the
    # largest value at the top. `decreasing=TRUE` therefore reverses them.
    if ( decreasing ) row.levels <- rev(row.levels)
  }
  ci.cu$feature.labels <- factor(ci.cu$feature.labels, levels=row.levels)

  # "instance" has to be a data.frame so this can't be NULL.
  inst.name <- rownames(instance)

  # Check if main plot title has been given as parameter, otherwise use default one
  if ( is.null(main) ) {
    main <- paste("Studied instance (context):", inst.name)
    if  ( !is.null(target.concept) )
      main <- paste0(main, "\nTarget concept is \"", target.concept, "\"")
  }

  # Influence plot separated because needs more than trivial manipulations.
  p <- ggplot(ci.cu)
  if ( use.influence ) {
    p <- p +
      geom_bar(aes(x=feature.labels, y=phi, fill=Positive.Phi),
               stat="identity", position ="identity") +
      labs(y = expression(phi)) +
      scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue")) +
      theme(legend.position="none")
  }
  else {
    ymin <- 0
    ymax <- ifelse(scale.CI, max(ci.cu$CI), 1)
    p <- p + ylim(ymin, ymax)
    if ( plot.mode == "colour_cu" ) {
      p <- p +
        geom_col(aes(feature.labels, CI, fill=CU)) +
        labs(y="CI", fill="CU") +
        scale_fill_gradient2(low=low.color, mid=mid.color, high=high.color, limits=c(0,1), midpoint=neutral.CU)
    }
    else {
      p <- p +
        geom_bar(aes(x=feature.labels, y=CI), stat="identity", position ="identity",
                 alpha=as.numeric(ci.colours[3]), fill=ci.colours[1], color=ci.colours[2])
      if ( is.null(cu.colours) ) {
        p <- p +
          geom_bar(aes(x=feature.labels, y=cu_scaled, fill=CU), stat="identity", position="identity",
                   alpha=1.0, color='black') +
          scale_fill_gradient2(low=low.color, mid=mid.color, high=high.color, limits=c(0,1), midpoint=neutral.CU) +
          labs(y="CI and relative CU", fill="CU")
      }
      else {
        p <- p +
          geom_bar(aes(x=feature.labels, y=cu_scaled), stat="identity", position="identity",
                   alpha=as.numeric(cu.colours[3]), fill=cu.colours[1], color=cu.colours[2]) +
          labs(y="CI and relative CU")
      }
    }
  }
  p <- p + coord_flip() +
    facet_wrap(~Output, labeller=label_both) + # Use scales="free_y" is different ordering for every facet
    ggtitle(main) +
    xlab("Feature")
  return(p)
}
