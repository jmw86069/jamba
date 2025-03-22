## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a re-submission to correct problems in r-oldrel-macos-arm64.

## Resubmission

Thank you Dr. Benjamin Altmann and Dr. Brian Ripley for your patience!

* The vignette has been updated to use the following:
`requireNamespace("kableExtra", quietly=TRUE)`
* The '@examplesIf' for kable_coloring() also uses the same check:
`requireNamespace("kableExtra", quietly=TRUE)`
