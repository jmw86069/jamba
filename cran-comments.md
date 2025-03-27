## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a re-submission to correct problems in r-oldrel-macos-arm64 with
jamba version 1.0.2.

## Resubmission

Thank you Dr. Benjamin Altmann and Dr. Brian Ripley for your patience!

* One test failed with version 1.0.3 only on R-devel. The error was
caused by slightly different hex color, as if some minor rounding error
upstream from the jamba package. The jamba test was adjusted to accomodate
a small potential change. I could not reproduce the error on R-4.5.0 on MacOS.

Changes in jamba version 1.0.3 to fix one error in version 1.0.2:

* The vignette now uses
`requireNamespace("kableExtra", quietly=TRUE)`
* The '@examplesIf' for kable_coloring() now uses
`requireNamespace("kableExtra", quietly=TRUE)`
* Both changes should fix the error seen only on r-oldrel-macos-arm64.
However these errors were not reproducible on my test system running
R-4.3.3 on M1 MacOS, nor in the available r-builder tests.
The change relies upon `requireNamespace()` as a proper test
whether an installed R package is usable.
