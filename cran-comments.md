# Version 0.5.0 comments

## Test environments

* local OS X install, R 4.0.2, platform x86_64-apple-darwin17.0 
* devtools::check_win_devel()
* devtools::check_rhub()
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran

## R CMD check results

OK in all environments.

# Comments from version 0.1.0 submission

Kept here mainly for logging purposes.

## Test environments
* local OS X install, R 4.0.2
* win-builder (devel) 
* devtools::check_rhub()
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Kary Främling <Kary.Framling@umu.se>’

New submission
```

Since this is the first version/submission to CRAN of the `ciu` package, 
there's still this one NOTE from `R CMD check` (which is apparently normal).

## Re-submission 

The feedback from CRAN is indicated in italics, responses as normal font. 

### Remark 1 

*Please change the start of your description from "R implementation..." to "Implementation...".*

Done.

### Remark 2 

*You have examples for unexported functions.*

```
barplot.ciu() in: 
	ciu.blackbox.new.Rd 
	ciu.new.Rd
ciu.result.new() in:
      ciu.new.Rd
explain() in:
      ciu.blackbox.new.Rd
      ciu.new.Rd
ggplot.col.ciu() in:
      ciu.new.Rd
pie.ciu() in:
      ciu.new.Rd
plot.ciu.3D() in:
      ciu.new.Rd
```

*Please either omit these examples or export the functions.*

For `ciu.result.new`, it **is** exported. Unless there's something strange that I didn't see. You might mean the function `plot.ciu`?

<<<<<<< HEAD
The other functions (`barplot, explain, ...`) are "inner functions" of the `ciu.new` function. `ciu.new` returns a `list` object (that additionally is of class `CIU`. That CIU/list object's elements are a list of the "public methods" that can be called for that object (the ones that you mention above). This was the recommended way of doing "object-oriented programming" in R, as described in an R document from around 2005. The later arrived R6 classes use the same principle but they require importing the R6 library and doesn't offer any additional, useful functionality. 

Unfortunately, Roxygen doesn't take "inner functions" into account in any way (and apparently doesn't really support R6 neither). So exporting those methods is not really possible (unless there's a solution that I am unaware of). 

However, all those functions do have a corresponding .Rd file so their documentation is accessible through the standard R "help" functionality, e.g. `?pie.ciu` will give the expected help text. `R CMD CHECK` checks these `.Rd` files too and the only problem is that there can't be a standard "Usage" section because `R CMD CHECK` doesn't know how to do the check for this kind of "inner functions". 

This kind of object-oriented encapsulation gives advantages for avoiding name conflicts, as well as unintentional modification of variables between different functions. The use of `<<-` in `ciu` is related to this and only modifies "instance variables" of CIU objects (see reply about this later). 

One inconventient with this object-oriented approach in R is that all the "inner functions" seem to be copied into all CIU objects (which is not so smart). However, CIU objects are typically not created in great numbers, nor for long reuse so this should not be a problem. 

**Conclusion**: It is not possible to export these functions just by adding `#' @export` because Roxygen and devtools will not handle it properly, at least for the moment. The other option is to remove the examples but that will reduce the quality of the documentation and usability of the `ciu` package. Still, if that is the only possibility, then I will of course remove those examples. Which would be a pity also in the sense that `R CMD check` runs all those examples and makes sure that everything runs successfully. 
=======
Actual reply: `ciu` uses the recommended way of doing "object-oriented programming" in R from around 2005, as described in an R document from back then. It is very similar to the newer "Reference Classes" or "R5" way of doing object-oriented programming. For instance, methods are called using <object>$<method> and assignment of values to instance variables is done using `<<-`. 

This signifies that the functions `barplot, explain, ...` are "inner functions" of the `ciu.new` function. Expressed in another way, they are methods associated with `CIU` objects. `ciu.new` returns an object of class `CIU`, which is actually a `list` object. That CIU/list object's elements are the "public methods" that can be called for that object (the ones that you mention above). 

Unfortunately, Roxygen doesn't take "inner functions" into account in any way. So exporting those functions is not really possible (unless there's a solution that I am unaware of). I do not have the impression that using Reference Classes would help there. 

However, all those functions do have a corresponding .Rd file so their documentation is accessible through the standard R "help" functionality, e.g. `?barplot.ciu` will give the expected help text. `R CMD CHECK` checks these `.Rd` files too and the only problem is that there can't be a standard "Usage" section because `R CMD CHECK` doesn't know how to do the check for this kind of "inner functions". 

This kind of object-oriented encapsulation gives advantages for avoiding name conflicts, as well as unintentional modification of variables between different functions.  

One inconventient with this object-oriented approach in R is that all the "inner functions" seem to be copied into all CIU objects (which is not so smart). However, CIU objects are typically not created in great numbers, nor for long reuse so this should not be a problem. 

Finally - it's mainly a design choice. S3 functions might be good also. But then for instance the `explain` function would clash with several `explain` functions in other packages, notably the `lime` package that implements another method but in the same domain as `ciu`. Difficult to know what is the best design choice. 

**Conclusion**: It is not possible to export these functions just by adding `#' @export` because Roxygen and devtools will not handle them properly, at least for the moment. The other option is to remove the examples but that will reduce the quality of the documentation and usability of the `ciu` package. Still, if that is the only possibility, then I will of course remove those examples. However, that would be a pity also because `R CMD check` runs all those examples and makes sure that everything runs successfully. 
>>>>>>> 090516da01ac245ccb64fd670afbd70c6305775b

### Remark 3 

*\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ('# Not run:') as a warning for the user.
Does not seem necessary.*

*Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.*

<<<<<<< HEAD
Thanks, I did this change. The only reason for this is that, for one example, the test sometimes takes some tenths of a second longer than 5 seconds to run (due to the machine learning algorithm), giving a "NOTE" when doing `R CMD check`. 
=======
Thanks, I did this change. The only reason for having \donttest{} is that, for one example, the test sometimes takes some tenths of a second longer than 5 seconds to run (due to the machine learning algorithm), giving a "NOTE" when doing `R CMD check`. 
>>>>>>> 090516da01ac245ccb64fd670afbd70c6305775b

### Remark 4 

*Please delete the standard text from man/plot.ciu.Rd*

```
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
```
Oups, I don't know where that came from. I removed it, sorry. 

### Remark 5 

*Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an* immediate *call of on.exit() that the settings are reset when the function is exited. e.g.:*

```
...
oldpar <- par(no.readonly = TRUE)    # code line i
on.exit(par(oldpar))            # code line i + 1
...
par(mfrow=c(2,2))            # somewhere after
...
```

*e.g.: ContextualImportanceUtility.R*
*If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.*

<<<<<<< HEAD
Thanks, I didn't know about `on.exit`. Anyways, I removed all `par()` call everywhere, better not to have them there at all. 
=======
Thanks, I didn't know about `on.exit`. Anyways, I removed all `par()` calls everywhere, better not to have them there at all. 
>>>>>>> 090516da01ac245ccb64fd670afbd70c6305775b

### Remark 6 

*Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos.*

```
e.g.:
oldpar <- par(no.readonly = TRUE)
...
par(oldpar)
e.g. ciu.new.Rd
```
Yes, that's what I try to do systematically. Usually exactly in the way you indicate. But with `par(mfrow())` I think I had some strange behaviour, that's the reason why I did it differently. 

<<<<<<< HEAD
Anyways, as mentioned, removed all `par()` calls everywhere. Should have thought about that before.  
=======
Anyways, as mentioned, I removed all `par()` calls everywhere. Should have thought about that before.  
>>>>>>> 090516da01ac245ccb64fd670afbd70c6305775b

### Remark 7 

*Please do not modify the global environment (e.g. by using <<-) in your functions. This is not allowed by the CRAN policies.*

The use of ``<<-`` in the package only modifies CIU object's own "instance variables", it **never** affects the global environment. `R CMD check` complains if there's a risk of such things happening, which actually allowed me to notice and correct that I wasn't initializing some instance variables. 

*Please fix and resubmit.*

*Best,*
*Julia Haider*

*

Thanks, 

   Kary

