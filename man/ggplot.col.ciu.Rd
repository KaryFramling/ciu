\name{ggplot.col.ciu}
\alias{ggplot.col.ciu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ggplot2 "col" plot CIU explanation for specific instance}
\description{Create a barplot showing CI as the length of the bar and CU
on color scale from red to green, via yellow, for the given inputs and
the given output. First get a \code{CIU} object by calling \link{ciu.new}
as e.g. \code{ciu <- ciu.new(...)},
then call \code{ciu.res <- ciu$barplot.ciu(...)}.
\emph{"Usage" section is in
"Details" section because Roxygen etc. don't support documentation of functions
within functions.}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{instance}{Instance to explain. See \code{\link{explain}}.}
  \item{ind.inputs}{\code{\link{vector}} of indices for the
  inputs to be included in the plot.
  If NULL then all inputs will be included.}
  \item{output.names}{Vector with names of outputs to include. If
  \code{NULL} (default), then include all.}
  \item{in.min.max.limits}{See \code{\link{explain}}.}
  \item{n.samples}{See \code{\link{explain}}.}
  \item{neutral.CU}{Indicates when the Contextual Utility is considered
to be "negative". The default value of 0.5 seems quite logical for most cases.}
  \item{show.input.values}{Include input values after input labels or not.}
  \item{concepts.to.explain}{List of concepts to use in the plot, as defined
  by vocabulary provided as argument to \code{\link{ciu.new}}.
  If \code{ind.inputs=NULL}, then use \code{concepts.to.explain} instead.
  If both are NULL, then use all inputs.}
  \item{target.concept}{See \code{\link{explain}}.}
  \item{target.ciu}{See \code{\link{explain}}.}
  \item{low.color}{Color to use for CU=0. Default is red.}
  \item{mid.color}{Color to use for \code{CU=neutral.CU}. Default is yellow.}
  \item{high.color}{Color to use for CU=1. Default is darkgreen.}
  \item{sort}{NOT USED FOR THE MOMENT! Features are in the same order for
  all facets, sorted by mean importance over all facets, which feels like
  a decent behaviour.
  NULL, "CI" or "CU". No sorting by default, other options are
  sorting by CI or CU.}
  \item{decreasing}{NOT USED FOR THE MOMENT. Set to TRUE for decreasing sort.}
  \item{main}{Replace default main title of plot.}
}
\details{
First get a
\code{CIU} object by calling \link{ciu.new} as e.g. \code{ciu <- ciu.new(...)},
then call \code{ciu.res <- ciu$ggplot.col.ciu(...)}.
\emph{"Usage" section is here in
"Details" section because Roxygen etc. don't support documentation of functions
within functions.}

\bold{Usage}
\preformatted{
ggplot.col.ciu(
  instance,
  ind.inputs=NULL,
  output.names=NULL,
  in.min.max.limits=NULL,
  n.samples=100,
  neutral.CU=0.5,
  show.input.values=TRUE,
  concepts.to.explain=NULL,
  target.concept=NULL,
  target.ciu=NULL,
  low.color="red",
  mid.color="yellow",
  high.color="darkgreen",
  sort=NULL,
  decreasing=FALSE,
  main=NULL)
}
}
\value{Created ggplot object.
}
\seealso{
  \code{\link{barplot.ciu}}
  \code{\link{ciu.new}}
  \code{\link{explain}}
}
\author{
Kary Främling
}
