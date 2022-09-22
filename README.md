CommKern: Analysis of Network Community Structures <img src="CommKern_hexsticker.png" width="120" height="120" align="right"/>
=====================================================================

[![R-CMD-check](https://github.com/aljensen89/CommKern/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aljensen89/CommKern/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/aljensen89/CommKern/branch/master/graph/badge.svg)](https://app.codecov.io/gh/aljensen89/CommKern)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/CommKern)](https://cran.r-project.org/package=CommKern)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/CommKern)](https://www.r-pkg.org/pkg/CommKern)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/CommKern)](https://www.r-pkg.org/pkg/CommKern)



An R package for implementing the hierarchical multimodal spinglass (HMS) algorithm and semiparametric kernel machine methods, with specific applications to neuroimaging data. The HMS algorithm allows for multimodal inputs and creates a hierarchical structure of nested communities through modification of the spinglass algorithm first proposed in a 2006 paper by Reichardt and Bornholdt. The combined flexibility of specifying the maximum possible number of communities a priori (as opposed to an exact number), allowing for more than one source of information in the algorithm, and creation of a nested, hierarchical structure of communities addresses many of the limitations that exist within other community detection algorithms when applied to neuroimaging data. The semiparametric kernel machine methods can then be used to conduct statistical inference to understand if the partitioning of the network nodes to communities is associated with an outcome (whether binary or continuous) while controlling for potential confounders. Extrinsic or intrinsic cluster evaluation metrics have been provided in the package but the kernel approach is flexible enough for the end user to specify their own distance-based metric.

# Learn more about CommKern
The community detection algorithm and semiparametric kernel machine methods were developed as part of Alexandria Jensen's PhD dissertation work.  

## Vignettes

There is one vignette included with the package, for now.  Additional details will be added to this vignette.

```r
vignette('CommKern', package = 'CommKern')
```

Additional vignettes may also be authored soon.

## Installation
You can install CommKern from GitHub. This will require you to have [devtools](https://github.com/r-lib/devtools) installed, and, if you are using Windows, you'll need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed as well.

```
library(devtools)
install_github("aljensen89/CommKern", build_vignettes = TRUE)
```

## Hierarchical Multimodal Spinglass (HMS) Algorithm
Details of the HMS algorithm can be found in the vignette. Specific application to brain connectivity data will be briefly summarized here. Starting from a common brain atlas (e.g., Automated Anatomical Labeling (AAL), Harvard-Oxford (HO), Craddock 200 (CC200), etc.), adjacency matrices representing functional and structural connectivity are derived from the correlation between resting state functional MRI (rs-FMRI) blood oxygen level dependent (BOLD) time series or the number of white matter fiber tracks computed from diffusion tensor MRI, respectively. Previous studies have observed that functional connectivity can be detected between brain regions that lack a direct structural connection. However, the propensity for two areas to interact on a functional level has been shown to vary in proportion to their structural connectivity, leading to the widely held assumption that functional configurations are assumed to be a reflection of underlying structural linkages. As such, we have utilized the multimodal aspect to the HMS algorithm to create structurally-informed functional connectivity communities. 

<img src="HMS_Algorithm_Figure.png" align="left"/>

However, the flexibility of the HMS algorithm does not require either brain network data or multiple inputs. In his 2010 paper, Fortunato showed that the spinglass algorithm is both precise and accurate at recovering community structures but, due its computational complexity, should not be used on networks that exceed several thousand nodes. The HMS algorithm is flexible enough so that its hierarchical and/or multimodal aspects can be suppressed. We recommend that the end user find a balance between computation time and algorithmic specifications in all applications, paying special attention when applying to large networks. 

## Semiparametric Kernel Machine Methods
Details of the semiparametric kernel machine methods can be found in the vignette. Bringing the partitioning of nodes to communities (via the HMS algorithm or another community detection algorithm, if desired) into a format for analysis using a distance-based kernel will be briefly overviewed here. For group-level network data, a community detection algorithm is run, providing the clustering of nodes to communities. From there, a variety of cluster evaluation metrics can be chosen. Extrinsic metrics require some form of ground truth labeling; however, in neuroimaging, a ground truth is not available. For the appropriate extrinsic metrics, a harmonic mean is calculated such that for each pair of partitions A and B, each takes a turn being the ground truth label. Intrinsic metrics, on the other hand, do not require a ground truth labeling. Within this package, we utilize the Hamiltonian value from the HMS algorithm. Once the metric has been applied to the set of network community data, it is ready for input into the semiparametric kernel machine methods. Options are available for both a binary and continuous outcome of interest and for a fully nonparametric or semiparametric approach, depending on whether the end user wishes to include other covariates of interest.

<img src="Kernel_Machine_Methods.png" align="left"/>
