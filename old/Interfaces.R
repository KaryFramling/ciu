# "Interface classes" are collected into this source file. Because
# they only contain method declarations and class names, they are usually
# very compact. 
#
# Kary Fr?mling, created 21 mar 2006
#

# "Interface class" for all kinds of function approximator
# classes. This "class" contains no functionality, its sole
# "raison-d'?tre" is to define a set of methods that need to
# be implemented by any objects of type "FunctionApproximator".
function.approximator.new <- function() {

  m <- list (
             get.inputs = function() { NULL },
             get.outputs = function() { NULL },
             eval = function(invals) { NULL }
             )
  class(m) <- c("FunctionApproximator")
  return(m)
}

# "Interface class" for "trainable" function approximators.
trainable.approximator.new <- function() {

  m <- list (
             # Training methods
             train.with.delta = function(delta) { NULL },
             train = function(t) { NULL },

             # Learning rate setting for generality. May not be used
             # for all methods but we define it anyway.
             get.lr = function() { 1 },
             set.lr = function(lrate) { NULL },

             # Trace methods for generality 
             get.trace = function() { NULL },
             set.trace = function(tr) { trace <<- tr }
             )

  class(m) <- c("TrainableApproximator")
  return(m)
}

# "Interface class" for all kinds of "controllers", i.e. objects
# that take an input value vector and deside on a control action to take
# based on the input values.
# Public methods are:
# - reset: resets controller for new episode
# - get.state: returns current state/input value vector
# - set.state: move into new state, decide on action(s) and return "output"
#   values. Output values may be discrete or continuous.
# - get.actions: returns last action(s)
# - has.discrete.actions: TRUE if actions are discrete values, false if
#   continuous (in that case the action value(s) is typically directly
#   the control value).
controller.new <- function() {

  m <- list (
             reset = function() { NULL },
             get.state = function() { NULL },
             set.state.vector = function(inputs, reward=0) { NULL },
             get.actions = function() { NULL },
             has.discrete.actions = function() { TRUE },
             get.action.descriptions = function() { NULL },
             get.task = function() { NULL },
             set.task = function(task) { NULL }
             )
  class(m) <- c("Controller")
  return(m)
}

predict.FunctionApproximator <- function(fa, data) {
  return(fa$eval(data))
}

