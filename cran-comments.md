# Version 1.0.1

We are aware that this submission is very soon after the initial submission, and
acceptance of version 1.0.0.   This submission is required to address an issue
found Professor Ripely 11 hours after CRAN accepted v1.0.0.   In the CRAN
checks one of our tests, using `identical` fails on the no long-double system.
We have relaxed the testing to use `all.equal` instead of `identical`.

We hope that this change will also address the issue on the M1 Mac.  We do not,
ourselves, have access to a M1 Mac for testing at this time.  But given that the
CRAN check errors were on the same test script on the M1 and the noLD system we
expect that version 1.0.1 should pass on the M1 as well.

We have also updated the http links for badges in the README file to https per a request from Uwe Ligges on 22 Sep 2022.
