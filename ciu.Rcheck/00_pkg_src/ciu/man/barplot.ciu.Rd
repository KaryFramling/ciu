\name{barplot.ciu}
\alias{barplot.ciu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Barplot CIU explanation for specific instance}
\description{Create a barplot showing CI as the length of the bar and CU on color scale from
red to green, via yellow, for the given inputs and the given output. First get a
\code{CIU} object by calling \link{ciu.new} as e.g. \code{ciu <- ciu.new(...)},
then call as \code{ciu.res <- ciu$barplot.ciu(...)}.
}
%\usage{
%barplot.ciu(
%instance,
%ind.inputs=NULL,
%ind.output=1,
%in.min.max.limits=NULL,
%n.samples=100,
%neutral.CU=0.5,
%show.input.values=TRUE,
%concepts.to.explain=NULL,
%target.concept=NULL,
%target.ciu=NULL,
%color.ramp.below.neutral=NULL,
%color.ramp.above.neutral=NULL,
%sort=NULL,
%decreasing=FALSE,
%main=NULL,
%xlab=NULL,
%xlim=NULL,
%...)
%}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{instance}{the current input values for the instance to explain/plot.}
  \item{ind.inputs}{vector of indices for the inputs to be included in the plot. If NULL then all inputs will be included.}
  \item{ind.output}{index of output to be explained.}
  \item{in.min.max.limits}{matrix with one row per output and two columns, where the first column indicates
the minimal value and the second column the maximal value for that output. ONLY NEEDED
if they were not provided to "new" or if they need to be different for this instance.}
  \item{n.samples}{how many random instances to use for estimating CI and CU.}
  \item{neutral.CU}{indicates when the Contextual Utility is considered
to be "negative". The default value of 0.5 seems quite logical for most cases.}
  \item{show.input.values}{include input values after input labels or not. Default is TRUE.}
  \item{concepts.to.explain}{list of concepts to use in the plot, as defined by vocabulary provided
as argument to ciu.new. if "ind.inputs=NULL", then use "concepts.to.explain" instead. If both
are NULL, then use all inputs.}
  \item{target.concept}{if provided, then calculate CIU of inputs
  \code{ind.inputs.to.explain} relative to the given concept rather than
  relative to the actual output(s). See \code{explain()}.}
  \item{target.ciu}{See \code{explain()}.}
  \item{color.ramp.below.neutral}{color ramp function as returned by function colorRamp(). Default color
ramp is from red3 to yellow.}
  \item{color.ramp.above.neutral}{color ramp function as returned by function colorRamp(). Default color
ramp is from yellow to darkgreen.}
  \item{sort}{NULL, "CI" or "CU". No sorting by default, other options are sorting by CI or CU.}
  \item{decreasing}{set to TRUE for decreasing sort (see arguments of \code{sort()}).}
  \item{main, xlab, xlim, ...}{usual plot parameters, possible to override the default ones provided here if needed.}
}

\value{"void", i.e. whatever happens to be result of last instruction.
}
\author{
  Kary Främling
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
