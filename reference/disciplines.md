# Mathematical discipline IDs

Map mathematical disciplines to IDs for use in searching for
mathematicians.

## Usage

``` r
disciplines(search = NULL)
```

## Arguments

- search:

  a character(1) string which will search within disciplines. This can
  be a regular expression search term if desired.

## Value

Data frame, with columns:

- `id`:

  the discipline ID, as required by
  [`search_id()`](https://genealogy.louisaslett.com/reference/search_id.md)
  when searching for a mathematician within a specific mathematical
  discipline;

- `discipline`:

  the name of the discipline classification, per the Mathematics
  Genealogy Project.

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
# Lookup the ID of any discipline involving the partial word "stat"
disciplines("stat")

# Use a regular expression to only exactly match the whole word Statistics and nothing else
disciplines("^statistics$")

# Use the above to search only for statisticians with the first name Louis
search_id(given = "Louis", discipline = disciplines("^statistics$")$id)
}
```
