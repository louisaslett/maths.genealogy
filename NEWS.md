# maths.genealogy 0.1.3

* Catch stray example that could fail due to unavailable internet resources in `\donttest{}`.

# maths.genealogy 0.1.2

* Correctly quote 'WebSocket' in `DESCRIPTION`.
* Wrap examples that could fail due to unavailable internet resources in `\donttest{}`.

# maths.genealogy 0.1.1

* Add `max_zoom` argument to `plot_grviz()` to enable increasing default maximum zoom when plotting particularly deep or wide trees. Fixes issue <https://github.com/louisaslett/maths.genealogy/issues/1>.
* Additional tidying following final checks before first CRAN submission.

# maths.genealogy 0.1.0

* Add `plot_gg_path()` to create shortest path plots via `ggenealogy`.
* Add `plot_gg()` to enable `ggenealogy` plots of ancestry local to a mathematician.
* Create a "Getting Started" vignette.
* Extra error handling in `get_genealogy()` for premature WebSocket closure.
* Add {pkgdown} site.
* Create warning for vignette viewed on R-universe. For some reason the embedded graphviz does not work, see <https://github.com/r-universe-org/help/discussions/554>.
* Changes to satisfy new lintr rules.
* `README` finished up.

# maths.genealogy 0.0.1

* Add `plot_grviz()` to enable Graphviz plotting to both RStudio Viewer and PDF file.
* Add `get_genealogy()` to query the genealogical tree from the 'geneagrapher-core' WebSocket server.
* Add `disciplines()` to enable lookup of subject IDs to supply to `search_id()`.
* Add `search_id()` to enable searching for mathematicians at The Mathematics Genealogy project: <https://mathgenealogy.org/>.
* Create the all important hex logo!
* Initial development.
