## R CMD check results

Duration: 22.3s

0 errors | 0 warnings | 0 notes

This submission is to address the following request from CRAN:

> It seems we need to remind you of the CRAN policy:
> 
> 'Packages which use Internet resources should fail gracefully with an informative message
> if the resource is not available or has changed (and not give a check warning nor error).'
>
> This needs correction whether or not the resource recovers.

I apologise because I have tried to fix this problem before using `\donttest{}`
although it appears this is insufficient as CRAN will run `\donttest{}` code
sometimes.

After researching I have found what appears to be the latest advice on handling
this CRAN request at the following URL:

https://blog.thecoatlessprofessor.com/programming/r/api-packages-and-cran-requirements/

I have followed this advice within my package (option 2, since no API key) and
so all examples are now guarded with

#' @examplesIf interactive() && curl::has_internet()

I hope this will be sufficient to satisfy this request.
