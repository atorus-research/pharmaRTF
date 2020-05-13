## Resubmission 3
A note regarding the validation vignette: In certain places in the validation vignette there are helper functions that appear to write to the users default directory. This is to provide an executable functional testing document of the package with the package source code. This document is specified in the .Rbuildignore file so it does not execute during package build, but is available to the user to generate.
In this version I have:

* Added a \value tag to the write_rtf function documentation
* Added 'Atorus Research LLC' to the DESCRIPTION as the copyright holder.
* Removed the \dontrun from the write_rtf function example.
* Uncommented the write_rtf line in the rtf_doc.R function
* Added helper files for the validation vignette to the .Rbuildignore to remove any portion that writes to the users default directory.

## Resubmission 2
In this version I have:

* Updated the LICENSE file per the CRAN template.
* DESCRIPTION title was updated to be less than 65 characters.
* DESCRIPTION description was updated to include single quotes around package names.

## Resubmission
This is a resubmission. In this version I have:

* Updated the links to vignettes in the readme and vignettes.
* Updated title field to title case.
* Reference to the Huxtable package was capitalized.
* Validate vignette title corrected.
* Ran the win-builder test on the package before submission.

## Test environments
* local Ubuntu 18.04.4 LTS, R 3.6.2
* ubuntu-16.04 docker(azure ci)

## R CMD check results
There were no ERRORs or WARNINGs
There is one NOTE. This is a new submission and there is one false positive for mis-spelled words in the DESCRIPTION.
