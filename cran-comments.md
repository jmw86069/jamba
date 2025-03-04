## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

This is a resubmission, Benjamin Altmann's comments were helpful!
All points were addressed, and improved the code.

1. No method references are required in the DESCRIPTION
2. All .Rd files now contain \value
3. Examples no longer contain commented code.
4. Examples no longer use \dontrun{}.
5. Information messages to console are now suppressed and controlled
by `if(verbose)` logic.

   * All `print()`, `printDebug()`, `cat()` are controlled by `if(verbose)`.
   * One exception is `printDebug()`, which itself is only called
   within `if(verbose)` logic.
   * `jargs()` now uses `message()`

* Use of `par()` and `options()` no longer changes the user environment.

   * All instances now use `withr` to apply and revert changes.
   * The only exceptions are `options()` specific to the jamba package,
   which are intended for this purpose. By design, these options only modify
   aesthetics and do not affect analytical results.

Thank you!
