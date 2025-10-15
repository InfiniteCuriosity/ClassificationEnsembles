## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
The one note states, " Adding so many packages to the search path is excessive and importing
  selectively is preferable."
All of the packages are used each time ClassificationEnsembles is run. It automatically builds individual and ensembles of classification data, and uses all the packages. Removing any of the installed packages will reduce the accuracy of the results, and/or the documents to the user (such as summary graphs or reports). ClassificationEnsembles has been tested on several platforms, and consistently returns excellent results without any warnings or errors.

* Comment from CRAN to reduce the size of the package to less than 5MB (done)
* Comment from CRAN to add detail to the description about the method that is used (done)
* Please explain all acronyms (VIF) - the acronym was spelled out
* We still see: Size of tarball: 5173241 bytes Could you reduce the size a bit more? - Done
* Please add small executable examples in your Rd-files to illustrate the use of the exported function but also enable automatic testing. - This package automatically performs the entire analysis.
All my attempts to get anything to run in less than five seconds have failed. I can put executable examples in the Rd files, but they will take more than 5 seconds, which is why I removed them.
*You write information messages to the console that cannot be easily suppressed. All print commands were changed to message commands
* Please do not modify the global environment (e.g. by using <<-) in your functions. This is not allowed by the CRAN policies. This is complete. Took a lot of work, but the trained models and graphics are saved to a temp directory

* 0.7.1 fixed a ggplot2 issue that was crashing the system. Issue is resolved, it runs without any errors or warnings now.
