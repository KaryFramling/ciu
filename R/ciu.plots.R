
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
#' \dontrun{
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
#' }
ciu.explain.long.data.frame <- function(CIU, data=NULL, out.ind=1, neutral.CU = NULL) {
  # Get everything that we need from CIU object
  ciu <- CIU$as.ciu()
  model <- ciu$model
  if ( is.null(data) )
    data <- ciu$data.in
  if ( is.null(neutral.CU) )
    neutral.CU <- ciu$neutral.CU

  # Convert all character columns to factor
  char_cols <- sapply(data, is.character)
  data[char_cols] <- lapply(data[char_cols], as.factor)

  # Deal with numeric columns
  num_cols <- which(sapply(data, is.numeric))
  num_mins <- apply(data[,num_cols], 2, min)
  num_maxs <- apply(data[,num_cols], 2, max)
  num_ranges <- num_maxs - num_mins

  # Deal with factor columns
  fac_cols <- which(sapply(data, is.factor))
  if ( length(fac_cols) > 1 )
    fac_maxs <- sapply(data[fac_cols], nlevels)

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
    if ( length(num_cols) > 1 ) {
      num_inds <- num_cols + start.i - 1
      numvals <- as.numeric((data[i,num_cols] - num_mins)/num_ranges)
      m[num_inds,5] <- numvals
    }
    if ( length(fac_cols) > 1 ) {
      fac_inds <- fac_cols + start.i - 1
      idx <- sapply(data[i, fac_cols, drop = FALSE], as.integer)
      fac_vals <- (idx - 1) / pmax(1, fac_maxs - 1)
      m[fac_inds,5] <- fac_vals
    }
    m[start.i:end.i,6] <- as.numeric(ciuvals$cmin)
    m[start.i:end.i,7] <- as.numeric(ciuvals$cmax)
    m[start.i:end.i,8] <- as.numeric(ciuvals$outval)
    start.i <- start.i + ncol(data); end.i <- end.i + ncol(data)
  }
  # The loop emits ncol(data) consecutive rows per instance, so the instance each
  # row belongs to is recoverable. Included explicitly because callers otherwise
  # have to reconstruct it, which matters when `data` is a subset of the full set.
  df <- data.frame(Instance=rep(rownames(data), each=ncol(data)),
                   Feature=vars, CI=m[,1], CU=m[,2], Influence=m[,3], Value=m[,4],
                   Norm.Value=m[,5], Cmin=m[,6], Cmax=m[,7], Outvalue=m[,8])

  # Eliminate the potential NaN values that might have come by zero division.
  df$Norm.Value[is.nan(df$Norm.Value)] <- 0

  # Set class attribute in case someone needs to know
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
#' \dontrun{
#' # Boston data set with GBM model.
#' library(MASS)
#' library(caret)
#' library(ggbeeswarm)
#' kfoldcv <- trainControl(method="cv", number=10)
#' gbm <- caret::train(medv ~ ., Boston, method="gbm", trControl=kfoldcv)
#' ciu <- ciu.new(gbm, medv~., Boston)
#' df <- ciu.explain.long.data.frame(ciu)
#' # The quasirandom layout of beeswarm may cause warnings that don't matter.
#' # You can use "suppressWarnings" to just not see them, if you get some.
#' p <- ciu.plots.beeswarm(df); suppressWarnings(print(p))
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
#' }
ciu.plots.beeswarm <- function(data, target.columns=c("Feature", "CI", "Norm.Value")) {
  # Check that the necessary packages are installed.
  if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
    stop("Package 'ggbeeswarm' is required for ciu.plots.beeswarm. Install it with: install.packages('ggbeeswarm')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install it with: install.packages('ggplot2')")
  }

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

#' Make a visNetwork for the igraph using CIU attributes
#'
#' Mainly a CIU shortcut function for [ciu::ciu.plots.visNetwork], which
#' allows for switching between CI&CU versus Contextual influence visualisation.
#'
#' @param graph CIU-initialized [igraph::igraph].
#' @param ... Parameters to pass to [ciu::ciu.plots.visNetwork].
#' @param use.influence Use CI&CU values or influence?
#'
#' @returns A [visNetwork::visNetwork].
#' @export
#'
#' @examples
#' library(ciu)
#' library(igraph)
#' library(visNetwork)
#'
#' # Create a graph with 10 nodes using random graph structure (Erdos-Renyi model)
#' g <- sample_gnp(10, 0.3)
#' # Add node attributes (CI, CU, influence)
#' V(g)$CI <- runif(10, min = 0, max = 1)
#' V(g)$CU <- runif(10, min = 0, max = 1)
#' V(g)$influence <- runif(10, min = -1, max = 1)
#' ciu.plots.visNetwork_CIU(g)
#'
ciu.plots.visNetwork_CIU <- function(graph, use.influence = FALSE, ...) {
  if ( !use.influence )
    visnet <- ciu.plots.visNetwork(graph, node_area_attribute = "CI", node_color_attribute = "CU", ...)
  else {
    igraph::V(graph)$absinfluence <- abs(igraph::V(graph)$influence)
    visnet <- ciu.plots.visNetwork(graph, node_area_attribute = "absinfluence",
                                   node_colors = ifelse(igraph::V(graph)$influence > 0, "steelblue", "firebrick"))
  }
  return(visnet)
}

#' Plot igraph with XAI visualisation
#'
#' Plots the given [igraph::igraph] by using the attributes with the given names
#' for determining node sizes and colors. This function is not specific for CIU
#' or any other method.
#'
#' @param graph [igraph::igraph] object.
#' @param node_area_attribute Name of node attribute to use for node area.
#' @param node_color_attribute Name of node attribute to use for node color.
#' @param node_colors Array of colors to use directly as node colors.
#' @param default_radius Default radius for nodes.
#' @param min_radius Minimal radius allowed for nodes.
#' @param max_radius Maximal radius allowed for nodes.
#' @param default_node_color Default color for nodes.
#' @param node_color_palette Color palette to use for node color.
#' @param default_node_border_color Default border color for nodes.
#' @param ... Parameters passed to [igraph::plot.igraph].
#'
#' @returns Returns `NULL`, invisibly..
#' @export
#'
#' @examples
#' library(igraph)
#' library(ciu)
#'
#' # Create a graph with 10 nodes
#' # Using a random graph structure (Erdos-Renyi model)
#' g <- sample_gnp(10, 0.3)
#' # Add node attributes (CI, CU, influence)
#' V(g)$CI <- runif(10, min = 0, max = 1)
#' V(g)$CU <- runif(10, min = 0, max = 1)
#' V(g)$influence <- runif(10, min = -1, max = 1)
#' ciu.plots.plot_igraph(g) # No sizing, colors
#' node_color_attribute = "CU"
#' ciu.plots.plot_igraph(g, node_area_attribute = "CI", node_color_attribute = node_color_attribute)
#'
#' # You can add a legend for the color like this:
#' # Add a color scale legend using plotrix
#' if (require(plotrix, quietly = TRUE)) {
#'   color.legend(xl = 0.8, yb = -0.3, xr = 1.0, yt = 0.3,
#'                legend = c("0.0", "0.5", "1.0"),
#'                rect.col = colorRampPalette(c("red", "yellow", "darkgreen"))(100),
#'                gradient = "y",
#'                align = "rb",
#'                cex = 0.8)
#'   text(0.9, 0.4, node_color_attribute, cex = 0.9, font = 2)
#' } else {
#'   # Fallback to simple legend if plotrix not available
#'   legend("topright",
#'          legend = c("CU = 1.0", "CU = 0.5", "CU = 0.0"),
#'          fill = colorRampPalette(c("red", "yellow", "darkgreen"))(100)[c(100, 50, 1)],
#'          title = "CU Values",
#'          cex = 0.8)
#' }
#'
#' # Influence plot
#' V(g)$absinfluence <- abs(V(g)$influence)
#' ciu.plots.plot_igraph(g, node_area_attribute = "absinfluence",
#'   node_colors = ifelse(V(g)$influence > 0, "steelblue", "firebrick"))
#' legend("topright", legend = c("Positive", "Negative"),
#'   fill = c("steelblue", "firebrick"), cex = 0.8)
ciu.plots.plot_igraph <- function(graph,
                                  node_area_attribute = NULL,
                                  node_color_attribute = NULL,
                                  node_colors = NULL,
                                  default_radius = 20, min_radius = 0, max_radius = 50,
                                  default_node_color = "lightblue", default_node_border_color = "darkblue",
                                  node_color_palette = colorRampPalette(c("red", "yellow", "darkgreen"))(100),
                                  ...
) {

  # Check that the necessary packages are installed.
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for ciu.plots.plot_igraph_xai. Install it with: install.packages('igraph')")
  }

  # Scale node area (CI) -> Convert to radius
  if ( is.null(node_area_attribute) || is.null(igraph::vertex_attr(g, node_area_attribute)) )
    diam_value <- node_radius <- default_radius
  else {
    node_radius <- sqrt(scales::rescale(igraph::vertex_attr(g, node_area_attribute), to = c(min_radius^2, max_radius^2)))  # Apply sqrt for area-based scaling
    diam_value <- node_radius * 2
    # # We skip this possibility: modify diameter rather than area:
    # diam_value <- scales::rescale(igraph::V(graph)$CI, to = c(min_radius, max_radius))*2
  }

  # Set node colors
  node_border_colors <- default_node_border_color
  if ( is.null(node_colors) ) {
    if ( !is.null(node_color_attribute) && !is.null(igraph::vertex_attr(g, node_color_attribute)) ) {
      # Create a color gradient for CU using red-yellow-darkgreen
      node_colors <- node_color_palette[ceiling(igraph::vertex_attr(g, node_color_attribute) * 99) + 1]
    }
    else {
      node_colors <- default_node_color
    }
  }

  # Do the actual plot
  plot(g,
       vertex.size = node_radius,
       vertex.color = node_colors,
       frame.color = node_border_colors,
       ...)

  # Return invisible NULL
  invisible(NULL)
}

#' Make a visNetwork for the graph
#'
#' Creates a visNetwork from an igraph and sets the visual attributes according
#' to the specified attribute.
#'
#' @inheritParams ciu.plots.plot_igraph
#' @param node_tooltips Array of tooltips to use for nodes instead of the
#' default ones.
#' @param hierarchical_layout Use hierarchical layout instead of default one?
#'
#' @returns A [visNetwork::visNetwork].
#' @export
#'
#' @examples
#' library(igraph)
#' library(visNetwork)
#' library(ciu)
#'
#' # Create a graph with 10 nodes
#' # Using a random graph structure (Erdos-Renyi model)
#' g <- sample_gnp(10, 0.3)
#' # Add node attributes (CI, CU, influence)
#' V(g)$CI <- runif(10, min = 0, max = 1)
#' V(g)$CU <- runif(10, min = 0, max = 1)
#' V(g)$influence <- runif(10, min = -1, max = 1)
#' ciu.plots.plot_igraph(g) # No sizing, colors
#' ciu.plots.visNetwork(g, node_area_attribute = "CI", node_color_attribute = "CU")
#'
#' # Influence plot
#' V(g)$absinfluence <- abs(V(g)$influence)
#' ciu.plots.visNetwork(g, node_area_attribute = "absinfluence",
#'   node_colors = ifelse(V(g)$influence > 0, "steelblue", "firebrick"))
#'
#' # Add navigation buttons, just as an example
#' vn <- ciu.plots.visNetwork(g, node_area_attribute = "CI", node_color_attribute = "CU")
#' vn %>% visInteraction(navigationButtons = TRUE)
#'
#' # Adding a legend color bar is more demanding and maybe not such a great
#' # idea. But it can be done through raw HTML:
#' library(htmltools)
#' ciu.plots.visNetwork_with_legend(vn)
#' # Legend and navigation buttons:
#' ciu.plots.visNetwork_with_legend(vn %>% visInteraction(navigationButtons = TRUE))
ciu.plots.visNetwork <- function(graph,
                                 node_area_attribute = NULL,
                                 node_color_attribute = NULL,
                                 node_colors = NULL,
                                 node_tooltips = NULL,
                                 hierarchical_layout = FALSE,
                                 default_radius = 20, min_radius = 0, max_radius = 50,
                                 default_node_color = "lightblue", default_node_border_color = "darkblue",
                                 node_color_palette = colorRampPalette(c("red", "yellow", "darkgreen"))(100)
) {

  # Check that the necessary packages are installed.
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for ciu.plots.visNetwork. Install it with: install.packages('igraph')")
  }
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' is required for ciu.plots.visNetwork. Install it with: install.packages('visNetwork')")
  }

  # Node size. Scale node area -> Convert to radius
  if ( is.null(node_area_attribute) || is.null(igraph::vertex_attr(graph, node_area_attribute)) )
    diam_value <- node_radius <- default_radius
  else {
    node_radius <- sqrt(scales::rescale(igraph::vertex_attr(graph, node_area_attribute), to = c(min_radius^2, max_radius^2)))  # Apply sqrt for area-based scaling
    diam_value <- node_radius * 2
  }
  igraph::V(graph)$value <- diam_value

  # Set node colors
  igraph::V(graph)$color.border <- default_node_border_color
  if ( is.null(node_colors) ) {
    if ( !is.null(node_color_attribute) && !is.null(igraph::vertex_attr(graph, node_color_attribute)) ) {
      # Create a color gradient for CU using red-yellow-darkgreen
      col_inds <- ceiling(igraph::vertex_attr(graph, node_color_attribute) * 99) + 1
      # Sometimes we might actually get CU values > 1 (and maybe smaller than 0?)
      # We force them to be inside the range, even though it could be useful to
      # detect them for "debugging".
      col_inds <- pmax(pmin(col_inds, 100), 1)
      node_colors <- node_color_palette[col_inds]
    }
    else {
      node_colors <- default_node_color
    }
  }
  igraph::V(graph)$color <- node_colors

  # Make visNetwork tooltips
  if ( is.null(node_tooltips) ) {
    if ( !is.null(igraph::V(graph)$label) && !is.null(igraph::vertex_attr(graph, node_area_attribute)) ) {
      t <- paste0(igraph::V(graph)$label, "<br>", node_area_attribute,
                  ": ", sprintf("%.3f", igraph::vertex_attr(graph, node_area_attribute)))
      if ( !is.null(node_color_attribute) && !is.null(igraph::vertex_attr(graph, node_color_attribute)) )
        t <- paste0(t, "<br>", node_color_attribute, ": ",
                    format(igraph::vertex_attr(graph, node_color_attribute), digits = 3))
      igraph::V(graph)$title <- t
    }
  }
  else {
    igraph::V(graph)$title <- node_tooltips
  }
  if ( !is.null(igraph::E(graph)$relation) )
    igraph::E(graph)$title <- E(graph)$relation

  # Convert igraph into visNetwork data structure
  vis <- visNetwork::toVisNetworkData(graph)

  # Create interactive visNetwork graph with directed edges
  visnet <- visNetwork::visNetwork(vis$nodes, vis$edges) %>%
    visNetwork::visNodes(font = list(multi = TRUE), scaling = list(min = min_radius, max = max_radius)) %>%
    visNetwork::visEdges(arrows = "to", smooth = FALSE) %>%  # Ensure directed edges
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visNetwork::visPhysics(stabilization = TRUE)

  # Options for the network
  if ( hierarchical_layout ) {
    visnet <- visnet %>%
      visNetwork::visHierarchicalLayout()  # Apply hierarchical (tree) layout
  }
  return(visnet)
}

#' Create HTML plot with visNetwork and legend
#'
#' For the moment, this function just adds a legend with the default CU colors.
#'
#' @param vn [visNetwork::visNetwork] object.
#' @param legend_title Title for the legend.
#'
#' @returns A "browsable" object.
#' @export
ciu.plots.visNetwork_with_legend <- function(vn, legend_title = "") {
  # Check that the necessary packages are installed.
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' is required for ciu.plots.visNetwork_with_legend. Install it with: install.packages('visNetwork')")
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required for ciu.plots.visNetwork_with_legend. Install it with: install.packages('htmltools')")
  }

  # Create a custom gradient legend with HTML
  gradient_legend <- htmltools::tags$div(
    style = "position: absolute; right: 20px; top: 100px; width: 30px; height: 200px;",
    htmltools::tags$div(
      style = paste0(
        "width: 100%; height: 100%; ",
        "background: linear-gradient(to top, red, yellow, darkgreen); ",
        "border: 1px solid black;"
      )
    ),
    htmltools::tags$div(style = "position: absolute; left: 35px; top: -5px;", "1.0"),
    htmltools::tags$div(style = "position: absolute; left: 35px; top: 95px;", "0.5"),
    htmltools::tags$div(style = "position: absolute; left: 35px; bottom: -5px;", "0.0"),
    htmltools::tags$div(
      style = "position: absolute; left: 5px; top: -25px; font-weight: bold;",
      legend_title
    )
  )

  # Combine network and legend
  htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$div(
        style = "position: relative; width: 100%; height: 600px;",
        vn,
        gradient_legend
      )
    )
  )
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

