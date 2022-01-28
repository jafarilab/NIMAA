## Resubmission
This is a resubmission. In this version I have done the changes in the following three aspects which I got from CRAN staff:

1. `If there are references describing the methods in your package, please 
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for 
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")`

* Added the reference paper with the format above in DESCRIPTION file.

2. `\dontrun{} should only be used if the example really cannot be executed 
(e.g. because of missing additional software, missing API keys, ...) by 
the user. That's why wrapping examples in \dontrun{} adds the comment 
('# Not run:') as a warning for the user.
Does not seem necessary. Please unwrap the examples if they are executable in < 5 sec, or create 
additionally small toy examples to allow automatic testing.
(You could also replace \dontrun{} with \donttest, if it takes longer 
than 5 sec to be executed, but it would be preferable to have automatic 
checks for functions. Otherwise, you can also write some tests.)`

*  Changed all the examples, there is no \dontrun{} in examples' scripts, all examples can be run.

3. `Please ensure that your functions do not write by default or in your 
examples/vignettes/tests in the user's home filespace (including the 
package directory and getwd()). This is not allowed by CRAN policies. In 
your examples/vignettes/tests you can write to tempdir().`

*  Changed the vignette file, won't write anything in local.

## Test environments
* local Ubuntu 20.04.1, R 4.1.0
* win-builder (release, devel)

## local R CMD check results

0 errors | 0 warnings | 0 note

## win-builder (release, devel) R CMD check results

0 errors | 0 warnings | 1 note
* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Cheng Chen <cheng.chen@helsinki.fi>'

  New submission


* This is a new release.
