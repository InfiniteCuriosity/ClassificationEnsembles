## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
The one note states, " Adding so many packages to the search path is excessive and importing
  selectively is preferable."
All of the packages are used each time ClassificationEnsembles is run. It automatically builds individual and ensembles of classification data, and uses all the packages. Removing any of the installed packages will reduce the accuracy of the results, and/or the documents to the user (such as summary graphs or reports). ClassificationEnsembles has been tested on several platforms, and consistently returns excellent results without any warnings or errors.
