# Search for mathematician in Mathematics Genealogy Project

Perform an online search using information about an individual
mathematician to find their ID in the [Mathematics Genealogy
Project](https://mathgenealogy.org/).

## Usage

``` r
search_id(
  family = NULL,
  given = NULL,
  middle = NULL,
  university = NULL,
  year = NULL,
  thesis_keyword = NULL,
  country = NULL,
  discipline = NULL
)
```

## Arguments

- family:

  a `character(1)` string with the family names.

- given:

  a `character(1)` string with the given names.

- middle:

  a `character(1)` string with the collapsed middle name(s).

- university:

  a `character(1)` string with the University at which PhD studied.

- year:

  a `character(1)` string or `integer(1)` with the year of completion.

- thesis_keyword:

  a `character(1)` string with keyword(s) in the PhD thesis title.

- country:

  a `character(1)` string with the country of study.

- discipline:

  an `integer(1)` with the mathematical sub-discipline code.

## Value

Data frame containing all matches against the provided search terms,
with columns:

- `id`:

  Mathematician ID (as required by
  [`get_genealogy()`](https://genealogy.louisaslett.com/reference/get_genealogy.md));

- `name`:

  The full name (surname first) of the mathematician;

- `university`:

  The institution at which PhD was obtained;

- `year`:

  The year PhD was completed.

## Details

Any one or more of the listed arguments can be provided. This will
trigger an online search against the live [Mathematics Genealogy
Project](https://mathgenealogy.org/) database, so please be considerate
and do not spam queries. All the information returned by a standard
search on the website is gathered into a data frame and returned,
enabling programmatic access to the data.

If you cannot find the individual you are looking for, it could be that
they are not in the [Mathematics Genealogy
Project](https://mathgenealogy.org/) database. New data can be submitted
by following the instructions in the "How to submit updates" section at
<https://mathgenealogy.org/submit.php>.

## References

Jackson, A. (2007). “A Labor of Love: The Mathematics Genealogy
Project”, *Notices of the AMS*, **54**(8), 1002-1003.
<https://www.ams.org/notices/200708/tx070801002p.pdf>

Mulcahy, C. (2017). “The Mathematics Genealogy Project Comes of Age at
Twenty-one”, *Notices of the AMS*, **64**(5), 466-470.
<https://www.ams.org/journals/notices/201705/rnoti-p466.pdf>

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
# Search for the package author
search_id("Aslett", "Louis")

# You may find it easier to directly use the https://mathgenealogy.org/
# website, and extract the "id" from the URL on the page for the mathematician
# of interest.
}
```
