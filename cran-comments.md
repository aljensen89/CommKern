# Version 1.0.0

## CRAN Submission Comments

```
If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <[https:...]https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")
```

The methods in this package are part of a PhD dissertation. Currently, these methods have not been published but, in the coming months, manuscripts will be submitted for publication. Once this has been done, the DESCRIPTION file will be updated accordingly.

```
\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Please replace \dontrun with \donttest.
```

This has been updated within the `community_plot()` function.

## R CMD check results

0 errors | 0 warnings | 3 notes

1) There is a NOTE that is only found on Windows (Server 2022, R-devel 64-bit): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.


2) The following NOTE was also found on Fedora Linux, R-devel, clang, gfortran: 

```
checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
```

Based on the following [conversation](https://groups.google.com/g/r-sig-mac/c/7u_ivEj4zhM), we believe this note can be ignored and is not an issue with the package but the testing environment.


3) Across all the checks, the following potential spelling error NOTE was found:

```
Possibly misspelled words in DESCRIPTION:
  multimodal (8:169)
  neuroimaging (8:73)
  semiparametric (8:348)
  spinglass (8:180)
```

None are actually a spelling errors. Multimodal, neuroimaging, and semiparametric are all correct spellings, with numerous examples across the literature. Spinglass has no consensus spelling, with some using spin glass, others using spin-glass, and yet others using spinglass.
