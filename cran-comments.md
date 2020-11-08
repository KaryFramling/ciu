## Test environments
* local OS X install, R 4.0.2
* win-builder (devel) 
* devtools::check_rhub()
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran

## Re-submission 


*Remark about initial submission was the following:*

Thanks, we see:

  License components with restrictions and base license permitting such:
    MIT + file LICENSE
  File 'LICENSE':
    # MIT License

    Copyright (c) 2020 Kary FrÃ¤mling

    Permission i

Please only ship the CRAN template for the MIT license.

Please fix and resubmit.

Best,
Uwe Ligges


*That has been fixed. Sorry for misunderstanding the instructions.* 

SInce this is the first version/submission to CRAN of the `ciu` package, 
there's still this one NOTE from `R CMD check` (which is apparently normal).

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Kary Främling <Kary.Framling@umu.se>’

New submission
