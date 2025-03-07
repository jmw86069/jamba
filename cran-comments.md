## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

The recent two points were addressed: Use single quotes for software terms,
and do not modify `.GlobalEnv`. Both were addressed.
For brevity I removed previous comments, and added a brief recap
hoping to save you time. Maybe I made it worse, I'm so sorry! Haha.

1. Single quotes were used for software terms in DESCRIPTION,
function titles, and function text where relevant.
2. All instances of `.GlobalEnv` were removed, replaced with `new.env()`
or alternatives consistent with `ls()` and `get()`.

 Brief recap of previous updates:

* Verbose output is FALSE by default. All `print()`, `cat()` uses
`if(verbose){}` except `printDebug()` which itself is called using
`if(verbose){}`.
* `jargs()` now uses `message()`.
* Functions no longer modify `par()` or `options()` except
'jam' options which are intended but only for aesthetics.
* Other calls to `par()` or `options()` use `withr` to revert changes.
* `setPrompt()` modifies `options()` by intent, however it can be
turned off with `updateOptions=FALSE`.

Thank you!
