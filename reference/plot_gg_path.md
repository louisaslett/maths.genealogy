# Plot shortest path in genealogical tree with `ggenealogy`

Plots a shortest path between two mathematicians in a genealogical tree
using the `ggenealogy` layout engine.

## Usage

``` r
plot_gg_path(g, id1 = NULL, id2 = NULL, expand = 0.15)
```

## Arguments

- g:

  an object of class `genealogy`, as returned by
  [`get_genealogy()`](https://genealogy.louisaslett.com/reference/get_genealogy.md).

- id1:

  an `integer(1)` or `character(1)` with the ID of the first
  mathematician of interest.

- id2:

  an `integer(1)` or `character(1)` with the ID of the second
  mathematician of interest.

- expand:

  a `numeric(1)` with the expansion factor for the graph. This defaults
  to `0.15`, with larger values causing the `x` axis to expand, smaller
  values for it to shrink. This is useful if the nearest common ancestor
  has a long name, which may cause it to be clipped when plotting:
  increase this expansion factor to rectify this.

## Value

An object of class `("gg", "ggplot")` which can be displayed, or further
manipulated using additional layers or aesthetic modifications from the
[`ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
package.

## Details

This function requires the `ggenealogy` package to be installed. It is
only a "Suggests" dependency because this package supports multiple
plotting approaches. The presence of this package will be verified when
the function is actually called, providing an opportunity to install
automatically if needed.

The shortest path between the two mathematician IDs provided is plotted,
with the `x` position of each label determined by the year of PhD award.

**NOTE:** if the name of the nearest common ancestor is long, it can be
clipped by `ggplot2`. If this occurs, increase the `expand` argument
greater than the default of `0.15`.

## References

Rutter, L., VanderPlas, S., Cook, D. and Graham, M.A. (2019).
“ggenealogy: An R Package for Visualizing Genealogical Data”, *Journal
of Statistical Software*, **89**(13), 1-31.
[doi:10.18637/jss.v089.i13](https://doi.org/10.18637/jss.v089.i13) .

Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York.

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
# First, you need to use search_id() to find the mathematician ID for the
# individual(s) you wish to plot, or visit https://mathgenealogy.org/ to look
# up in the browser.

# For example, to find the shortest genealogical path between the package
# author and my former postdoc supervisor, I would start by querying using
# both mathematician IDs
g <- get_genealogy(c(96119, 171971))

# Then use the plot_gg_path() function to use the underlying ggenealogy package
plot_gg_path(g)
}
```
