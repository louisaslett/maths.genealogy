## R CMD check results

Duration: 1m 0.2s

0 errors | 0 warnings | 0 notes

This submission is to address the following request from CRAN:

> It seems we need to remind you of the CRAN policy:
> 
> 'Packages which use Internet resources should fail gracefully with an informative message
> if the resource is not available or has changed (and not give a check warning nor error).'
>
> This needs correction whether or not the resource recovers.

I have double checked that all package functions do indeed fail gracefully with
informative error messages when the internet resources are not available.

To address checks not giving a warning or error I previously added `\donttest{}`
to examples which could fail due to unavailable internet resources. I decided
this based on the CRAN Writing R Extensions manual which states:

> 'Finally, there is \donttest, [...]. This should be needed only occasionally 
> but can be used for code which might fail in circumstances that are hard to
> test for [...]'

I believe the functions should trigger an error if the internet resource is
unavailable, not merely a message. Hence `\donttest{}` seems appropriate as
full tests of resource availability and correct response would clutter the
examples.

However, I discovered there was a single example where I failed to implement the
`\donttest{}` guard, which I have not rectified.
