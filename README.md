# 📦 `{maths.genealogy}` R package <img src="man/figures/logo.png" align="right" height="278" alt="" />

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![license](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/gpl-2.0.html)
[![metacran version](https://www.r-pkg.org/badges/version/maths.genealogy)](https://cran.r-project.org/package=maths.genealogy)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/maths.genealogy)](https://cran.r-project.org/package=maths.genealogy)
[![mlmc status badge](https://louisaslett.r-universe.dev/badges/maths.genealogy)](https://louisaslett.r-universe.dev/maths.genealogy)

Search for mathematicians on the [Mathematics Genealogy Project](https://mathgenealogy.org/) and pull full ancestor and/or descendent academic genealogical data.
The latter functionality is possible thanks to the WebSocket server run by [@davidalber](https://github.com/davidalber), which we access directly [with kind permission](https://github.com/davidalber/geneagrapher/issues/38).
The package also provides functionality to export these genealogical datasets to different plotting environments.

## Installation

The stable binary release can be install from CRAN:

``` r
install.packages("maths.genealogy")
```

The latest development binary can be installed from R-universe:

``` r
install.packages("maths.genealogy", repos = c("https://louisaslett.r-universe.dev", "https://cloud.r-project.org"))
```

Alternatively, you can install the development version from source on [GitHub](https://github.com/):

``` r
# install.packages("pak")
pak::pak("louisaslett/maths.genealogy")
```

## Example

The first task is to identify the ID of the mathematician(s) you would like to build the genealogical data for.
For the author of this package, that would be:

``` r
library("maths.genealogy")

search_id("Aslett", "Louis")
```

Then, one would retrieve the full genealogical tree, using the id identified in the search:

``` r
g <- get_genealogy(171971)
```

The simplest thing would then be to plot the whole genealogical tree:

``` r
plot_grviz(g)
```

Note you can also plot a shared genealogical tree by passing a vector of ids to `get_genealogy()`.
So to see the shared genealogy of the package author and his former postdoc supervisor:

``` r
g <- get_genealogy(c(96119, 171971))
plot_grviz(g)
```

This can be interesting to see where the trees share commonality and link together.
To just hone in on the shortest path:

``` r
plot_gg_path(g)
```

All the above functions take various options (see their documentation), so that for example more than two ids can be passed to `get_genealogy()` and then selective shortest path pairs be plotted with `plot_gg_path()` using appropriate function arguments.

For a slightly longer introduction, please see the [Getting Started vignette](https://genealogy.louisaslett.com/articles/getting-started.html)
