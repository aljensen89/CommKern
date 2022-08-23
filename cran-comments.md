## R CMD check results

0 errors | 0 warnings | 1 note

* Two potential spelling error notes were found in the DESCRIPTION file.
    *  neuroimaging: this is the standard spelling in the field, both in research and clinical fields (see https://medicine.utah.edu/psychiatry/research/labs/diagnostic-neuroimaging/neuroimaging as an example of the spelling).
    *  spinglass: there is no consensus on how to spell this in the literature; some use spin glass, others use spin-glass, and some use spinglass.

* The following note was also found during the R hub checks: "Skipping checking HTML validation: no command ‘tidy’ found"
   * Based on the following conversation (https://groups.google.com/g/r-sig-mac/c/7u_ivEj4zhM), we believe this note can be ignored and is not an issue with the package but the testing environment.
