# Hierarchical_Spinglass

[![R-CMD-check](https://github.com/aljensen89/Hierarchical_Spinglass/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aljensen89/Hierarchical_Spinglass/actions/workflows/R-CMD-check.yaml)

An R package for implementing the hierarchical multimodal spinglass (HMS) algorithm and semiparametric kernel machine methods, with specific applications to neuroimaging data. The HMS algorithm allows for multimodal inputs and creates a hierarchical structure of nested communities through modification of the spinglass algorithm first proposed in a 2006 paper by Reichardt and Bornholdt. The combined flexibility of specifying the maximum possible number of communities a priori (as opposed to an exact number), allowing for more than one source of information in the algorithm, and creation of a nested, hierarchical structure of communities addresses many of the limitations that exist within other community detection algorithms when applied to neuroimaging data. The semiparametric kernel machine methods can then be used to conduct statistical inference to understand if the paritioning of the network nodes to communities is associated with an outcome (whether binary or continuous) while controlling for potential confounders. Extrinsic or intrinsic cluster evaluation metrics have been provided in the package but the kernel approach is flexible enough for the end user to specify their own distance-based metric.

# Learn more about CommKern.
The community detection algorithm and semiparametric kernel machine methods were developed as part of Alexandria Jensen's PhD dissertation work.  

## Vignettes

There is one vignette included with the package, for now.  Additional details will be added to this vignette.

```r
vignette('CommKern-pkg', package = 'CommKern')
```

Additional vignettes may also be authored soon.

## Installation
You can install CommKern from GitHub. This will require you to have [devtools](https://github.com/hadley/devtools) installed, and, if you are using Windows, you'll need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed as well.

```
library(devtools)
install_github("aljensen/CommKern", build_vignettes = TRUE)
```

## Hierarchical Multimodal Spinglass (HMS) Algorithm

