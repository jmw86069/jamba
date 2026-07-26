# provigrep: progressive case-insensitive value-grep

case-insensitive value-grep for a vector of patterns

case-insensitive grep for a vector of patterns

## Usage

``` r
provigrep(
  patterns,
  x,
  maxValues = NULL,
  sortFunc = c,
  rev = FALSE,
  returnType = c("vector", "list"),
  ignore.case = TRUE,
  value = TRUE,
  ...
)

proigrep(..., value = FALSE)
```

## Arguments

- patterns:

  `character` vector of regular expression patterns, ultimately passed
  to [`base::grep()`](https://rdrr.io/r/base/grep.html).

- x:

  `character` vector that is the subject of
  [`base::grep()`](https://rdrr.io/r/base/grep.html).

- maxValues:

  `integer` or NULL, the maximum matching entries to return per grep
  pattern. Note that each grep pattern may match multiple values, and
  values are only returned at most once each, so restricting items
  returned by one grep pattern may allow an item to be matched by
  subsequent patterns, see examples. This argument is most commonly used
  with `maxValues=1` which returns only the first matching entry per
  pattern.

- sortFunc:

  `function` or NULL, used to sort entries within each set of matching
  entries. Use NULL to avoid sorting entries.

- rev:

  `logical` whether to reverse the order of matching entries. Use TRUE
  if you would like entries matching the patterns to be placed last, and
  entries not matching the grep patterns to be placed first. This
  technique is effective at placing "noise names" at the end of a long
  vector, for example.

- returnType:

  `character` indicating whether to return a vector or list. A list will
  be in order of the grep patterns, using empty elements to indicate
  when no entries matched each pattern. This output is useful when you
  would like to know which patterns matched specific entries.

- ignore.case:

  `logical` parameter sent to
  [`base::grep()`](https://rdrr.io/r/base/grep.html), TRUE runs in
  case-insensitive mode, as by default.

- value:

  `logical` indicating whether to return the matched value, or when
  `value=FALSE` the index position is returned.

- ...:

  additional arguments are passed to
  [`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md).

## Value

`character` vector with entries in `x` reordered to match the order of
`patterns` provided, or `list` when `returnType="list"` named by
`patterns` in the order provided. When `value=FALSE` then it returns
`integer` index values of `x`.

## Details

Purpose is to provide "progressive vigrep()",which is value-returning,
case-insensitive grep, starting with an ordered vector of grep patterns.
For example, it returns entries in the order they are matched, by the
progressive use of grep patterns.

It is particularly good when using multiple grep patterns, since
[`grep()`](https://rdrr.io/r/base/grep.html) does not accept multiple
patterns as input. This function also only returns the unique matches in
the order they were matched, which alleviates the need to run a series
of [`grep()`](https://rdrr.io/r/base/grep.html) functions and collating
their results.

It is mainly to allow for prioritized ordering of matching entries,
where one would like certain matching entries first, followed by another
set of matching entries, without duplication. For example, one might
grep for a few patterns, but want certain pattern hits to be listed
first.

## See also

Other jam grep functions:
[`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md),
[`igrep()`](https://jmw86069.github.io/jamba/reference/igrep.md),
[`igrepHas()`](https://jmw86069.github.io/jamba/reference/igrepHas.md),
[`igrepl()`](https://jmw86069.github.io/jamba/reference/igrepl.md),
[`unigrep()`](https://jmw86069.github.io/jamba/reference/unigrep.md),
[`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md),
[`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md),
[`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md)

## Examples

``` r
# a rather comical example
# set up a test set with labels containing several substrings
set.seed(1);
testTerms <- c("robot","tree","dog","mailbox","pizza","noob");
testWords <- pasteByRow(t(combn(testTerms,3)));

# now pull out entries matching substrings in order
provigrep(c("pizza", "dog", "noob", "."), testWords);
#>                     3                     6                     8 
#>    "robot_tree_pizza"     "robot_dog_pizza" "robot_mailbox_pizza" 
#>                    10                    12                    14 
#>    "robot_pizza_noob"      "tree_dog_pizza"  "tree_mailbox_pizza" 
#>                    16                    17                    19 
#>     "tree_pizza_noob"   "dog_mailbox_pizza"      "dog_pizza_noob" 
#>                    20                     1                     5 
#>  "mailbox_pizza_noob"      "robot_tree_dog"   "robot_dog_mailbox" 
#>                     7                    11                    13 
#>      "robot_dog_noob"    "tree_dog_mailbox"       "tree_dog_noob" 
#>                    18                     4                     9 
#>    "dog_mailbox_noob"     "robot_tree_noob"  "robot_mailbox_noob" 
#>                    15                     2 
#>   "tree_mailbox_noob"  "robot_tree_mailbox" 
# more detail about the sort order is shown with returnType="list"
provigrep(c("pizza", "dog", "noob", "."), testWords, returnType="list");
#> $pizza
#>                     3                     6                     8 
#>    "robot_tree_pizza"     "robot_dog_pizza" "robot_mailbox_pizza" 
#>                    10                    12                    14 
#>    "robot_pizza_noob"      "tree_dog_pizza"  "tree_mailbox_pizza" 
#>                    16                    17                    19 
#>     "tree_pizza_noob"   "dog_mailbox_pizza"      "dog_pizza_noob" 
#>                    20 
#>  "mailbox_pizza_noob" 
#> 
#> $dog
#>                   1                   5                   7                  11 
#>    "robot_tree_dog" "robot_dog_mailbox"    "robot_dog_noob"  "tree_dog_mailbox" 
#>                  13                  18 
#>     "tree_dog_noob"  "dog_mailbox_noob" 
#> 
#> $noob
#>                    4                    9                   15 
#>    "robot_tree_noob" "robot_mailbox_noob"  "tree_mailbox_noob" 
#> 
#> $.
#>                    2 
#> "robot_tree_mailbox" 
#> 
# rev=TRUE will reverse the order of the list
provigrep(c("pizza", "dog", "noob", "."), testWords, returnType="list", rev=TRUE);
#> $.
#>                    2 
#> "robot_tree_mailbox" 
#> 
#> $noob
#>                    4                    9                   15 
#>    "robot_tree_noob" "robot_mailbox_noob"  "tree_mailbox_noob" 
#> 
#> $dog
#>                   1                   5                   7                  11 
#>    "robot_tree_dog" "robot_dog_mailbox"    "robot_dog_noob"  "tree_dog_mailbox" 
#>                  13                  18 
#>     "tree_dog_noob"  "dog_mailbox_noob" 
#> 
#> $pizza
#>                     3                     6                     8 
#>    "robot_tree_pizza"     "robot_dog_pizza" "robot_mailbox_pizza" 
#>                    10                    12                    14 
#>    "robot_pizza_noob"      "tree_dog_pizza"  "tree_mailbox_pizza" 
#>                    16                    17                    19 
#>     "tree_pizza_noob"   "dog_mailbox_pizza"      "dog_pizza_noob" 
#>                    20 
#>  "mailbox_pizza_noob" 
#> 
provigrep(c("pizza", "dog", "noob", "."), testWords, rev=TRUE);
#>                     2                     4                     9 
#>  "robot_tree_mailbox"     "robot_tree_noob"  "robot_mailbox_noob" 
#>                    15                     1                     5 
#>   "tree_mailbox_noob"      "robot_tree_dog"   "robot_dog_mailbox" 
#>                     7                    11                    13 
#>      "robot_dog_noob"    "tree_dog_mailbox"       "tree_dog_noob" 
#>                    18                     3                     6 
#>    "dog_mailbox_noob"    "robot_tree_pizza"     "robot_dog_pizza" 
#>                     8                    10                    12 
#> "robot_mailbox_pizza"    "robot_pizza_noob"      "tree_dog_pizza" 
#>                    14                    16                    17 
#>  "tree_mailbox_pizza"     "tree_pizza_noob"   "dog_mailbox_pizza" 
#>                    19                    20 
#>      "dog_pizza_noob"  "mailbox_pizza_noob" 

# another example showing ordering of duplicated entries
set.seed(1);
x <- paste0(
   sample(letters[c(1,2,2,3,3,3,4,4,4,4)]),
   sample(1:5));
x;
#>  [1] "d3" "c5" "d1" "a4" "b2" "c3" "b5" "d1" "c4" "d2"
# sort by letter
provigrep(letters[1:4], x)
#>  [1] "a4" "b2" "b5" "c5" "c3" "c4" "d3" "d1" "d1" "d2"

# show more detail about how the sort is performed
provigrep(letters[1:4], x, returnType="list")
#> $a
#> [1] "a4"
#> 
#> $b
#> [1] "b2" "b5"
#> 
#> $c
#> [1] "c5" "c3" "c4"
#> 
#> $d
#> [1] "d3" "d1" "d1" "d2"
#> 

# rev=TRUE will reverse the order of pattern matching
# which is most useful when "." is the last pattern:
provigrep(c(letters[1:3], "."), x, returnType="list")
#> $a
#> [1] "a4"
#> 
#> $b
#> [1] "b2" "b5"
#> 
#> $c
#> [1] "c5" "c3" "c4"
#> 
#> $.
#> [1] "d3" "d1" "d1" "d2"
#> 
provigrep(c(letters[1:3], "."), x, returnType="list", rev=TRUE)
#> $.
#> [1] "d3" "d1" "d1" "d2"
#> 
#> $c
#> [1] "c5" "c3" "c4"
#> 
#> $b
#> [1] "b2" "b5"
#> 
#> $a
#> [1] "a4"
#> 

# example demonstrating maxValues
# return in list format
provigrep(c("[ABCD]", "[CDEF]", "[FGHI]"), LETTERS, returnType="list")
#> $`[ABCD]`
#> [1] "A" "B" "C" "D"
#> 
#> $`[CDEF]`
#> [1] "E" "F"
#> 
#> $`[FGHI]`
#> [1] "G" "H" "I"
#> 

# maxValues=1
provigrep(c("[ABCD]", "[CDEF]", "[FGHI]"), LETTERS, returnType="list", maxValues=1)
#> $`[ABCD]`
#> [1] "A"
#> 
#> $`[CDEF]`
#> [1] "C"
#> 
#> $`[FGHI]`
#> [1] "F"
#> 
provigrep(c("[ABCD]", "[CDEF]", "[FGHI]"), LETTERS, returnType="list", maxValues=1, value=FALSE)
#> $`[ABCD]`
#> [1] 1
#> 
#> $`[CDEF]`
#> [1] 3
#> 
#> $`[FGHI]`
#> [1] 6
#> 
proigrep(c("[ABCD]", "[CDEF]", "[FGHI]"), LETTERS, maxValues=1)
#> [1] 1 3 6
```
