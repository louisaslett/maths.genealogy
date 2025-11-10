# Plot genealogical tree with Graphviz

Plots a genealogical tree either interactively or to PDF using the
Graphviz layout engine.

## Usage

``` r
plot_grviz(g, file = "", max_zoom = 200)
```

## Arguments

- g:

  an object of class `genealogy`, as returned by
  [`get_genealogy()`](https://genealogy.louisaslett.com/reference/get_genealogy.md).

- file:

  an optional file name. If the file name is specified, then Graphviz
  will render the genealogical tree to PDF and save in this file. If the
  file name is not specified, then the plot will be rendered
  interactively in the RStudio Viewer panel.

- max_zoom:

  a `numeric(1)` with the maximum zoom factor when plotting in the
  Viewer. If trees are particularly deep or wide, the default maximum
  zoom of 200x may be insufficient, in which case a value larger than
  200 should be supplied. This option has no effect when plotting to a
  file.

## Value

If a filename was specified, the full path of the saved file is returned
as a `character(1)` string. If no filename was specified, then an
`htmlwidget` suitable for display in the RStudio Viewer is returned.

## Details

This function requires the `DiagrammeR`, `DiagrammeRsvg` and either
`svgPanZoom` (interactive) or `rsvg` (pdf output) packages to be
installed. They are only "Suggests" dependencies as this package
supports multiple plotting options. The presence of these packages will
be verified when the function is actually called, providing an
opportunity to install them automatically if needed.

## References

Ellson, J., Gansner, E.R., Koutsofios, E., North, S.C. and Woodhull, G.
(2004). “Graphviz and Dynagraph — Static and Dynamic Graph Drawing
Tools”. In: Jünger, M., Mutzel, P. (eds) *Graph Drawing Software,
Mathematics and Visualization*, 127-148.
[doi:10.1007/978-3-642-18638-7_6](https://doi.org/10.1007/978-3-642-18638-7_6)
.

Iannone, R. and Roy, O. (2024). *DiagrammeR: Graph/Network
Visualization*. R package,
<https://CRAN.R-project.org/package=DiagrammeR>.

Iannone, R. (2016). *DiagrammeRsvg: Export DiagrammeR Graphviz Graphs as
SVG*. R package, <https://CRAN.R-project.org/package=DiagrammeRsvg>.

Ooms, J. (2024). *rsvg: Render SVG Images into PDF, PNG, (Encapsulated)
PostScript, or Bitmap Arrays*. R package,
<https://CRAN.R-project.org/package=rsvg>.

Riutta, A., Tangelder, J., Russell, K., et al. (2020). *svgPanZoom: R
'Htmlwidget' to Add Pan and Zoom to Almost any R Graphic*. R package,
<https://CRAN.R-project.org/package=svgPanZoom>.

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
# First, you need to use search_id() to find the mathematician ID for the
# individual(s) you wish to plot, or visit https://mathgenealogy.org/ to look
# up in the browser.

# For example, the package author would get their own tree using
g <- get_genealogy(171971)

# Then use the plot_grviz() function to produce a full genealogical tree
plot_grviz(g)
}
```
