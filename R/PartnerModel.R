# Partner Model related functionality for CIU.
#
# Kary Främling, created in 2025
#

#' Create a new partner model
#'
#' @param name Name of the model.
#' @param root_concepts Names of root concepts/nodes in the graph.
#' @param graph Knowledge Graph [igraph::igraph] of concepts and relations
#' to use for explanations.
#'
#' @returns A [list] that is also of class "PartnerModel".
#' @export
ciu.partnermodel.new <- function(name, root_concepts, graph) {
  # Default vocabulary
  pmodel <- list(
    name = name,
    root_concepts = root_concepts,
    graph = graph
  )
  class(pmodel) <- c("PartnerModel", "list")
  return(pmodel)
}
