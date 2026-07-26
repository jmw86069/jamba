# set R prompt with project name and R version

set R prompt with project name and R version

## Usage

``` r
setPrompt(
  projectName = "unnamed",
  useColor = TRUE,
  projectColor = "yellow",
  bracketColor = "white",
  Rcolors = c("white", "white", "white"),
  PIDcolor = NA,
  promptColor = "white",
  usePid = TRUE,
  resetPrompt = FALSE,
  addEscape = NULL,
  updateOptions = TRUE,
  debug = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- projectName:

  `character` string, default "unnamed", used as a label to represent
  the project work.

- useColor:

  `logical` whether to define a color prompt if the `crayon` package is
  installed.

- projectColor, bracketColor, Rcolors, PIDcolor, promptColor:

  `character` colors used when `useColor==TRUE` and the `crayon` package
  is installed:

  - `projectColor` colors the project name;

  - `bracketColor` colors the curly brackets around the project;

  - `Rcolors` can be a vector of 3 colors, colorizing "R", the "-"
    divider, and the R version;

  - `PIDcolor` colors the PID when `usePid=TRUE`; and

  - `promptColor` colors the `">"` at the end of the prompt.

- usePid:

  `logical` whether to include the process ID in the prompt. Including
  the PID is helpful for the rare occasion when a process is hung and
  needs to be stopped directly.

- resetPrompt:

  `logical` whether to revert all changes to the prompt back to the
  default R prompt, that is, no color and no projectName.

- addEscape:

  `logical` or 'NULL' indicating whether to wrap color encoding ANSI
  inside additional escape sequences. This change is helpful for
  linux-based (readline-based) R consoles, by telling the console not to
  count ANSI color control characters as visible characters when
  determining word wrapping on the console. Note that RStudio does not
  work well with this setting. If you find that the word-wrap is
  incorrect in the R console, try `addEscape=TRUE`. Apparently most
  versions of RStudio will already adjust (and prevent) colorizing the
  prompt during editing, presumably to sidestep the problem of
  calculating the correct character length. By default when `addEscape`
  is 'NULL', it checks whether environmental variable `RSTUDIO` equals
  `"1"` (running inside RStudio) then sets `addEscape=FALSE`; otherwise
  it defines `addEscape=TRUE`. In most cases for commandline prompts,
  `addEscape=TRUE` is helpful and not problematic.

- updateOptions:

  `logical` whether to update the user
  [`options()`](https://rdrr.io/r/base/options.html) with
  `options(prompt="...")`, default TRUE.

- debug:

  `logical` indicating whether to print the ANSI control character
  output for the full prompt, for visual review.

- verbose:

  `logical` whether to print verbose output.

- ...:

  additional parameters are passed to
  [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  which is only relevant with the argument `useColor=TRUE`.

## Value

`list` named `"prompt"`, suitable to use in
[`options()`](https://rdrr.io/r/base/options.html) with the recommended
prompt. When `updateOptions=FALSE` use:
`options(setPrompt("projectName"))`

## Details

This function sets a simple, colorized R prompt with useful information:

- `projectName`

- R version, major and minor included

- Process ID (PID)

The prompt is defined in `options("prompt")`.

### Where Am I?

It is useful for the question: "What version of R?" In rare cases,
multiple R versions can be active at once (!), see the `rig` package for
this exciting capability.

### What Am I Doing?

The core question addressed is : "What am I working on?" The project
name is especially useful when working with multiple active R sessions.

### How Do I Stop This Thing?

It may also be useful for the question "How do I stop this thing", by
returning the Process ID to be used to kill a long-running process
without fear of killing the **wrong** long-running process.

### Can It Have Color?

Then of course, meeting the above requirements, at least make it pretty.

### Word-Wrap Gone Awry

A color-encoded prompt may sometimes interfere with word-wrapping on the
R console. A long line may wrap prematurely before reaching the right
edge of the screen. There are two frequent causes of this issue:

1.  `options("width")` is sometimes defined too narrow for the screen.
    When resizing the console, this option should be updated, and
    sometimes this update fails. To fix, either resize the window
    briefly again, or define `options("width")` manually. (Or debug the
    reason that this option is not being updated.)

2.  The terminal `locale` is sometimes mismatched with the terminal,
    usually caused by a layer of terminal emulation which is not
    compatible with ANSI color codes, or ANSI escape codes.

    - Some examples: 'PuTTY' on 'Windows', GNU 'screen', 'tmux'.

    - Check `Sys.env("LC_ALL")`. The most common results are `"C"` for
      generic C-type output, or a Unicode/UTF-8 locale such as
      `"en_US.UTF-8"` ('enUS' is English-USA in this context). In
      general, Unicode/UTF-8 is recommended, with benefit that it more
      readily displays other Unicode characters. However, sometimes the
      terminal environment (PuTTY or iTerm) is expecting one locale, but
      is receiving another. Either switching the terminal expected
      locale, or the R console locale, may resolve the mismatch.

R uses 'readline' for unix-like systems by default, and issues related
to using color prompt are handled at that level.

The 'readline' library allows escaping ANSI color characters so they do
not contribute to the estimated line width, and these codes are used in
`setPrompt()`.

The final workaround is `useColor=FALSE`, but that would be a sad
outcome.

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
[`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md),
[`getAxisLabel()`](https://jmw86069.github.io/jamba/reference/getAxisLabel.md),
[`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md),
[`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md),
[`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md),
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
[`middle()`](https://jmw86069.github.io/jamba/reference/middle.md),
[`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
[`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md),
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
[`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md),
[`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md),
[`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md)

## Examples

``` r
setPrompt("jamba")

setPrompt("jamba", projectColor="purple");

setPrompt("jamba", usePid=FALSE);

setPrompt(resetPrompt=TRUE);
```
