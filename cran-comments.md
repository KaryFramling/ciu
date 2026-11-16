## Test environments

* local macOS, R 4.6.0
* win-builder, R-devel
* R-hub: Ubuntu Linux (R-devel), macOS, Windows, and a build without Suggests packages

## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

This is an update to an existing CRAN package (0.8.0 -> 0.9.0). It adds
knowledge-graph based explanations, Intermediate Concept vocabularies built from
igraph objects, and an interactive Shiny application, and tidies parts of the
plotting API. See NEWS.md for details.

All packages required by the new graph and Shiny functionality (igraph,
visNetwork, shiny, shinyBS, htmltools) are in Suggests, and their use is
conditional on requireNamespace().
