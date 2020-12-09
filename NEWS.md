# ciu 0.1.0.9000

* Added parameter "use.influence" to "ggplot.col" and "barplot.ciu" 
functions/methods, which produces a LIME/SHAP/etc-like plot where 
"influence = ci*2*(cu-0.5)", bars 
go either right or left of zero and there are only two colours. 
* Added support for factor-type inputs to method "$plot.ciu". 
* Created new "class" called "ciu", which is just a list object with the 
"instance variables" of a CIU object. This makes it possible to create 
plot functions that are not methods of CIU but rather take a "ciu" object 
as their first parameter. The main reason for doing this modification is that 
CIU objects seem to use much more memory than a simple data-based "ciu" object. 
The documentation and package tools in R are also not too aware about 
"inner functions" (methods), which is an inconvenient. Finally, the plan was 
indeed to go for this approach in any case because increasing the code length
of CIU was not desirable in the long run. The way in which it is implemented now
allows functions and methods to be used interchangeably in any case. 

# ciu 0.1.0

* First version of ciu published at CRAN
