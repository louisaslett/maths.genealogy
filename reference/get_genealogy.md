# Retrieve genealogy tree by mathematician ID

Queries the genealogy of a single or set of mathematicians by their ID
in the [Mathematics Genealogy Project](https://mathgenealogy.org/).

## Usage

``` r
get_genealogy(id, ancestors = TRUE, descendants = TRUE)
```

## Arguments

- id:

  `integer` vector of IDs of mathematicians for whom the genealogy
  should be retrieved

- ancestors:

  `logical` indicating whether to include the genealogy backward to
  include all ancestors, defaults to `TRUE`. This can be a single
  `logical(1)` which then applies to all mathematicians referenced in
  the `id` argument, or it can be a vector of the same length as `id`
  providing different selection for each individual.

- descendants:

  `logical` indicating whether to include the genealogy forward to
  include all descendants, defaults to `TRUE`. This can be a single
  `logical(1)` which then applies to all mathematicians referenced in
  the `id` argument, or it can be a vector of the same length as `id`
  providing different selection for each individual.

## Value

A list object of class `genealogy`. Each element of the list represents
a mathematician in the genealogical tree. The name of the element is the
mathematician's ID in the [Mathematics Genealogy
Project](https://mathgenealogy.org/). Each element of the object is list
with containing:

- `id`:

  `integer(1)` with Mathematician's ID;

- `name`:

  `character(1)` containing the full name of the mathematician;

- `institution`:

  `character(1)` containing the institution at which PhD was obtained;

- `year`:

  `integer(1)` with the year their PhD was completed;

- `descendants`:

  `integer` vector of IDs of any mathematicians who were supervised by
  this individual for their PhD;

- `advisors`:

  `integer` vector of IDs of any mathematicians who were supervisors of
  this individual for their PhD.

In addition, there is an attribute named `start_nodes` which contains an
`integer` vector of IDs indicating the origin nodes used in the
genealogical tree search that produced this object. In other words, the
`id` argument as passed to this function.

## References

Alber, D. (2024). “'geneagrapher-core' package”,
<https://github.com/davidalber/geneagrapher-core>

Jackson, A. (2007). “A Labor of Love: The Mathematics Genealogy
Project”, *Notices of the AMS*, **54**(8), 1002-1003.
<https://www.ams.org/notices/200708/tx070801002p.pdf>

Mulcahy, C. (2017). “The Mathematics Genealogy Project Comes of Age at
Twenty-one”, *Notices of the AMS*, **64**(5), 466-470.
<https://www.ams.org/journals/notices/201705/rnoti-p466.pdf>

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
# First, you need to use search_id() to find the mathematician ID for the
# individual(s) you wish to plot, or visit https://mathgenealogy.org/ to look
# up in the browser. Once you have these IDs the get_genealogy() function will
# retrieve the genealogical tree.

# For example, to find the package author would search for themselves using
search_id("Aslett", "Louis")

# Then, use the id to retrieve the genealogy
g <- get_genealogy(171971)

# With that genealogy, you can then plot using plot_grviz() or other plotting
# functions.
}
```
