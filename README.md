[![CRAN Version](http://www.r-pkg.org/badges/version/TPMplt)](https://cran.r-project.org/package=TPMplt)
[![Total RStudio Cloud Downloads](http://cranlogs.r-pkg.org/badges/grand-total/TPMplt?color=brightgreen)](https://cran.r-project.org/package=TPMplt)
[![RStudio Cloud Downloads](http://cranlogs.r-pkg.org/badges/TPMplt?color=brightgreen)](https://cran.r-project.org/package=TPMplts)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
# TPMplt package

**TPMplt**, short for thermal processing-map plot, is a tool-kit for constructing dynamic material model (DMM) and corresponding visualization

## Installation

### Installation from github:

```
if(! "devtools" %in% installed.packages()) install.packages("devtools")
devtools::install_github("CubicZebra/TPMplt")
```

### Installation from CRAN:

```
install.packages("TPMplt")
```

## Main functions

TPMplt is a tool-kit for building and visualizing the dynmaic materials model (DMM), suggested by [Prasad and Gegel](https://link.springer.com/article/10.1007/BF02664902). It provides an easy approach to calculate constructive functions and other related material constants based on a given strain condiiton. 2D and 3D processing-maps with temperature as its x axis, while logarithm strain rate as its y axis are also available.

## Workflow

![Workflow Overview](./vignettes/img/demo_figs1.png)

## Processing Map Preview

![Preview of 2d processing map](./vignettes/img/demo_figs2.png)

## Attention

Some bugs occur in data extraction which results incorrect output of ```epsExtract()``` function (in the CRAN version). These problems were fixed in Git version. New submission for CRAN is coming soon.

## Contact

Author: ZHANG Chen

Mail: 447974102@qq.com
