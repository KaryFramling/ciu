# Knowledge Graph related functionality for CIU.
#
# Kary Främling, created in 2025
#

# This is to get the CRAN check to go through. Thanks ChatGPT.
utils::globalVariables(c("V", "E", "bfs", "name"))

#' Set CIU values for graph nodes
#'
#' Sets CI and CU values for all "Intermediate Concept" nodes in the passed
#' graph object, starting from the given root concept. This is a
#' recursively used/usable function for going through the entire hierarchy down
#' to the basic features.
#'
#' @param instance The explained instance (data.frame with one row).
#' @param CIU_object A CIU explainer object.
#' @param graph [igraph::igraph] object with the Knowledge Graph.
#' @param concept Name of the root concept.
#' @param out_index Which output index of the model to use for CIU.
#' @param scale_to_IC Scale CI values to the CI of the parent Intermediate
#' Concept or use the "global" CI value, i.e. the one that is directly
#' relative to the output value.
#' @param IC_samples Number of samples to use for estimating CIU values
#' @param level The level in the hierarchy. This starts with one and is used
#' internally for the recursive traversal.
#'
#' @returns An [igraph::igraph] object
#' @export
ciu.igraph.explain <- function(instance, CIU_object, graph, concept,
                               out_index = 1, scale_to_IC = TRUE, IC_samples = 500,
                               level = 1) {
  # Check that the necessary packages are installed.
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for ciu.igraph.explain. Install it with: install.packages('igraph')")
  }

  g <- graph # Make code shorter

  # Use CIU objects' neutral CU value
  neutral.CU <- CIU_object$as.ciu()$neutral.CU

  # Check if this is a topmost concept.
  if ( length(E(g)[.to(concept) & relation=="feature-of"]) == 0 ) {
    igraph::V(g)[concept]$CI <- 1.0 # Must be so by definition
    # Temporary fix: we assume that output utility function is linear
    ciu <- CIU_object$as.ciu()
    min <- ciu$abs.min.max[out_index,1]
    max <- ciu$abs.min.max[out_index,2]
    outval <- ciu$predict.function(ciu$model, instance)
    if ( !inherits(outval, "numeric")) # Difference between single- and multiple output
      outval <- outval[1,out_index]
    igraph::V(g)[concept]$CU <- (outval - min)/(max - min)
    igraph::V(g)[concept]$influence <- igraph::V(g)[concept]$CI*(igraph::V(g)[concept]$CU - neutral.CU)
    target_concept <- NULL # Expected by "meta.explain" for root concept.
    igraph::V(g)[concept]$level <- level
  }
  else
    target_concept <- concept

  # Then go for the child concepts.
  if ( igraph::V(g)[concept]$type == "Intermediate concept") {
    target_CI <- igraph::V(g)[concept]$CI
    child_concepts <- ciu.kg.get.child.features(g, concept)
    meta_ciu <- CIU_object$meta.explain(instance,
                                        target.concept = target_concept,
                                        concepts.to.explain = child_concepts,
                                        n.samples = IC_samples)
    # Need to deal with potential multiple outputs here
    ciuvals <- lapply(meta_ciu$ciuvals, function(df) df[out_index, ])
    CIval <- as.numeric(lapply(ciuvals, function(df) df$CI))
    if ( scale_to_IC )
      CIval <- target_CI*CIval
    igraph::V(g)[child_concepts]$CI <- CIval
    igraph::V(g)[child_concepts]$CU <- as.numeric(lapply(ciuvals, function(df) df$CU))
    igraph::V(g)[child_concepts]$influence <- igraph::V(g)[child_concepts]$CI*(igraph::V(g)[child_concepts]$CU - neutral.CU)
    igraph::V(g)[child_concepts]$level <- level + 1
  }

  # Call function recursively for all child concepts that are ICs
  for ( c in child_concepts ) {
    if ( igraph::V(g)[c]$type == "Intermediate concept")
      g <- ciu.igraph.explain(instance, CIU_object, g, c,
                              out_index, scale_to_IC, IC_samples,
                              level = level + 1)
  }

  # We are ready, return final graph
  return(g)
}

#' Additive propagation of attributions to ICs
#'
#' This function is for making it possible to create Intermediate Concept (IC)
#' explanations/visualisations also with other methods than with CIU. This is
#' notably for Shapley values, where the influence value of an IC is the sum
#' of its constituents (which should add up to the difference between the
#' current output value and the baseline). In practice, it can of course be used
#' for any values but then there is no guarantee that there's a correspondence
#' between the additive result and the actual output value. Even for Shapley
#' values, there's no guarantee that it will be so due to approximated values
#' as produced by the `iml` package for instance. With SHAP, this should however
#' be guaranteed.
#'
#' @param graph [igraph::igraph] with the ICs to use. This should preferably
#' be a graph that only contains the actual explanation nodes, as extracted
#' e.g. by [ciu.get.sub.graph].
#' @param feature_attributions Feature attribution values as an array that has
#' to include column names accessible by `names`.
#' @param attribute_name Node attribute name to use for the values.
#' @param normalize_to Array with minimal and maximal values to normalize the
#' values to. If set to NULL, then there won't be any normalisation.
#'
#' @returns Updated [igraph::igraph].
#' @export
#'
#' @examples
#' \dontrun{
#' # This should not be run because it requires the use of one of the example
#' # files that are not a part of the package itself.
#' library(AmesHousing)
#' library(igraph)
#' library(ciu)
#' library(iml)
#' source("TestAmesKnowledgeGraph.R")
#' ames_pms <- create.CIU_Ames_for_Shiny()
#' kg <- ames_pms$partner_models$Default$graph
#' # Extract IC hierarchy from it
#' kg <- ciu.get.sub.graph(kg, "SalesPrice_def_voc")
#' # Get Shapley values with iml
#' inst_ind <- 16
#' ames_instance <- ames_pms$data[inst_ind,]
#' predict(ames_pms$model, ames_instance)
#' iml_model <- Predictor$new(ames_pms$model, data = ames_pms$data)
#' shapley_vals <- Shapley$new(iml_model, x.interest = ames_instance)
#' shapley_vals_vec <- setNames(shapley_vals$results$phi, shapley_vals$results$feature)
#' kg <- ciu.igraph.additive_attribution_to_ICs(kg, shapley_vals_vec)
#'
#' # Create and show visNetwork for it
#' igraph::V(kg)$absinfluence <- abs(igraph::V(kg)$influence)
#' visnet <- ciu.plots.visNetwork(kg, node_area_attribute = "absinfluence",
#'   node_colors = ifelse(igraph::V(kg)$influence > 0, "steelblue", "firebrick"))
#' visnet
#'
#' # With more appropriate tooltips:
#' tooltips <- paste0(V(kg)$name, "<br>&phi;: ", format(V(kg)$influence, digits = 3))
#' visnet <- ciu.plots.visNetwork(kg, node_area_attribute = "absinfluence",
#'   node_colors = ifelse(igraph::V(kg)$influence > 0, "steelblue", "firebrick"),
#'   node_tooltips = tooltips)
#' visnet
#'
#' # Influence values for intermediate concepts can then be retrieved liek this:
#' fnames <- ciu.kg.get.child.features(kg, "Basement")
#' inf_values <- V(kg)[name %in% fnames]$influence
#' df <- data.frame(feature=fnames, phi=inf_values, sign=(inf_values>=0))
#' p <- ggplot(df) + geom_col(aes(x=reorder(feature, phi), y=phi, fill=sign)) +
#'   coord_flip() +
#'   labs(x ="", y = expression(phi)) + theme(legend.position = "none") +
#'   scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue"))
#' print(p)
#' }
ciu.igraph.additive_attribution_to_ICs <- function(graph, feature_attributions,
                                                   attribute_name = "influence",
                                                   normalize_to = NULL) {
  # Normalize the values to [-1,1] by default, otherwise graph node diameters
  # and other things will not like it.
  if ( !is.null(normalize_to) )
    feature_attributions <- scales::rescale(feature_attributions, to = normalize_to)

  # Get correct mapping between graph and column names.
  voc <- ciu.voc.from.graph(graph, names(feature_attributions))

  # Go through all names in the vocabulary, get their attribution value and
  # set it as the corresponding attribute of the corresponding node.
  # 1) Compute the sums for all voc entries at once (type-safe with vapply)
  influence_by_name <- vapply(voc, function(ix) sum(feature_attributions[ix]), numeric(1))
  # 2) Align to vertex order and assign in one go
  vn <- V(kg)$name
  vals <- influence_by_name[match(vn, names(influence_by_name))]  # NA for names not in voc
  # Replace NAs with 0
  vals[is.na(vals)] <- 0
  # 3) Single assignment (no loop, no repeated graph copies)
  kg <- igraph::set_vertex_attr(kg, attribute_name, value = vals)

  return(kg)
}

#' Plot concept tree using basic [igraph::igraph] plotting
#'
#' This is mainly a convenience function for plotting trees "sideways", which
#' tends to be more readable. Also, nodes of type "Feature" are plotted in red,
#' all others in blue. The [igraph::igraph] should be a tree structure for this
#' to really make sense.
#'
#' @param g The [igraph::igraph].
#' @param root_concept Name of the node to use as the top level.
#' @param main Plot title.
#' @param circular Plot with circular layout or not?
#' @param ... Remaining parameters are passed to `plot` function.
#'
#' @returns Void
#' @export
#'
#' @examples
#' library(ciu)
#' library(igraph)
#'
#' # Create a graph with 10 nodes using random graph structure (Erdos-Renyi model)
#' g <- make_tree(7, children = 2, mode = "out")
#' V(g)[1:3]$type <- "Intermediate concept"
#' V(g)[4:7]$type <- "Feature"
#' ciu.plot.knowledgegraph(g, "1")
ciu.plot.knowledgegraph <- function(g, root_concept, main = "Concept hierarchy",
                                    circular = FALSE, ...) {
  layout <- igraph::layout_as_tree(g, root = root_concept, circular = circular)
  layout <- layout[, c(2, 1)]  # Swap columns (flip x and y axes)
  layout[, 1] <- -layout[, 1]  # Reverse the x-axis to go right
  plot(g, layout = layout, vertex.label = V(g)$label, #edge.label = E(g)$relation,
       vertex.size = 0,
       vertex.color = "white",
       vertex.label.color = ifelse(V(g)$type == "Feature", "red", "blue"),
       main = main,
       ...)
}

#
#' Get CIU vocabulary from Knowledge Graph
#'
#' Make a CIU-compatible vocabulary based on the graph and data column names
#' Remember to remove the target value column(s) from the passed data!
#'
#' @param g [igraph::igraph] object. The names of the leaves have to match
#' with the column names in `column_names`
#' @param column_names Array of column names in the data.
#'
#' @returns A CIU vocabulary (a [list])
#' @export
ciu.voc.from.graph <- function(g, column_names) {
  voc <- list()
  for ( v in igraph::V(g)$name ) {
    voc[[v]] <- recursive.get.col.names.from.graph(g, v, column_names)
  }
  return(voc)
}

# Extract subgraph that starts from given node and (recursively) includes all
# "feature-of"-connected vertices. root_concept_names can be a single value or
# an array.
#' Extract subgraph from igraph
#'
#' This function is mainly for extracting the concept tree for explanations,
#' starting from a root concept. If there are more than one root concepts, then
#' the corresponding subgraphs are extracted too.
#'
#' @param g [igraph::igraph] object.
#' @param root_concept_names A root concept name, or an array of names.
#' @param relation The relation name to use for extraction.
#' @param mode The direction of the relation from the upper node to use for
#' extraction.
#'
#' @returns An [igraph::igraph].
#' @export
ciu.get.sub.graph <- function(g, root_concept_names, relation = "feature-of", mode = "out") {
  # BFS with a filter for edges with name = "feature_of"
  reachable_vertices <- igraph::bfs(
    g,
    root = root_concept_names,
    mode = mode,
    unreachable = FALSE,
    order = TRUE,
    dist = FALSE,
    extra = igraph::E(g)$name == relation
  )$order

  # Extract only the non-NA reachable vertices
  reachable_vertices <- reachable_vertices[!is.na(reachable_vertices)]

  # Get the names of reachable vertices
  reachable_vertex_names <- igraph::V(g)$name[reachable_vertices]

  # Create the subgraph
  subgraph <- igraph::induced_subgraph(g, vids = reachable_vertex_names)
  return(subgraph)
}

#' Get child nodes based on specific relation
#'
#' Get all child nodes of the node with name "node_name" and whose edges
#' have a "relation" attribute with the given name.
#'
#' @param g [igraph::igraph] graph.
#' @param node_name Name of root node.
#' @param relation Relation name to use.
#'
#' @returns An array of child node names.
#' @export
ciu.kg.get.child.features <- function(g, node_name, relation = "feature-of") {
  filtered_edges <- igraph::E(g)[.from(node_name) & relation == relation]
  igraph::ends(g, filtered_edges)[,2]
}

# Find all leaf column names for the "concept" in the graph.
recursive.get.col.names.from.graph <- function(g, concept, column_names) {
  v <- igraph::V(g)[name == concept]
  if ( v$type == "Intermediate concept") {
    children <- ciu.kg.get.child.features(g, concept)
    vc <- igraph::V(g)[name %in% children]
    leaf_concepts <- c()
    for ( n in children )
      leaf_concepts <- c(leaf_concepts, recursive.get.col.names.from.graph(g, n, column_names))
  }
  else
    return(which(column_names == concept))
  return(leaf_concepts)
}
