# Changelog

## maths.genealogy 0.1.4

CRAN release: 2025-07-24

- Revamp all examples using recommendations at
  <https://blog.thecoatlessprofessor.com/programming/r/api-packages-and-cran-requirements/>
  to avoid problems with CRAN which apparently will still occasionally
  run `\donttest{}` code.

## maths.genealogy 0.1.3

CRAN release: 2025-07-07

- Catch stray example that could fail due to unavailable internet
  resources in `\donttest{}`.

## maths.genealogy 0.1.2

CRAN release: 2025-03-06

- Correctly quote ‘WebSocket’ in `DESCRIPTION`.
- Wrap examples that could fail due to unavailable internet resources in
  `\donttest{}`.

## maths.genealogy 0.1.1

CRAN release: 2025-03-03

- Add `max_zoom` argument to
  [`plot_grviz()`](https://genealogy.louisaslett.com/reference/plot_grviz.md)
  to enable increasing default maximum zoom when plotting particularly
  deep or wide trees. Fixes issue
  <https://github.com/louisaslett/maths.genealogy/issues/1>.
- Additional tidying following final checks before first CRAN
  submission.

## maths.genealogy 0.1.0

- Add
  [`plot_gg_path()`](https://genealogy.louisaslett.com/reference/plot_gg_path.md)
  to create shortest path plots via `ggenealogy`.
- Add
  [`plot_gg()`](https://genealogy.louisaslett.com/reference/plot_gg.md)
  to enable `ggenealogy` plots of ancestry local to a mathematician.
- Create a “Getting Started” vignette.
- Extra error handling in
  [`get_genealogy()`](https://genealogy.louisaslett.com/reference/get_genealogy.md)
  for premature WebSocket closure.
- Add {pkgdown} site.
- Create warning for vignette viewed on R-universe. For some reason the
  embedded graphviz does not work, see
  <https://github.com/r-universe-org/help/discussions/554>.
- Changes to satisfy new lintr rules.
- `README` finished up.

## maths.genealogy 0.0.1

- Add
  [`plot_grviz()`](https://genealogy.louisaslett.com/reference/plot_grviz.md)
  to enable Graphviz plotting to both RStudio Viewer and PDF file.
- Add
  [`get_genealogy()`](https://genealogy.louisaslett.com/reference/get_genealogy.md)
  to query the genealogical tree from the ‘geneagrapher-core’ WebSocket
  server.
- Add
  [`disciplines()`](https://genealogy.louisaslett.com/reference/disciplines.md)
  to enable lookup of subject IDs to supply to
  [`search_id()`](https://genealogy.louisaslett.com/reference/search_id.md).
- Add
  [`search_id()`](https://genealogy.louisaslett.com/reference/search_id.md)
  to enable searching for mathematicians at The Mathematics Genealogy
  project: <https://mathgenealogy.org/>.
- Create the all important hex logo!
- Initial development.
