# Merge list of data.frames retaining all rows

Merge list of data.frames retaining all rows

## Usage

``` r
mergeAllXY(...)
```

## Arguments

- ...:

  arguments are handled as described:

  - named arguments are passed through to
    [`base::merge.data.frame()`](https://rdrr.io/r/base/merge.html),
    with the exception of `all.x` and `all.y` which are both defined
    `all.x=TRUE` and `all.y=TRUE`. and all other arguments are assumed
    to be `data.frame` or equivalent, and are merged in order they
    appear as arguments. The order of these `data.frame` objects should
    not affect the output content, but will affect the row and column
    order of the resulting `data.frame`.

## Value

`data.frame` after iterative calls to
[`base::merge.data.frame()`](https://rdrr.io/r/base/merge.html).

## Details

This function is a wrapper around
[`base::merge.data.frame()`](https://rdrr.io/r/base/merge.html) except
that it allows more than two data.frame objects, and applies default
arguments `all.x=TRUE` and `all.y=TRUE` for each merge operation to
ensure that all rows are kept.

## See also

Other jam list functions:
[`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
[`heads()`](https://jmw86069.github.io/jamba/reference/heads.md),
[`jam_rapply()`](https://jmw86069.github.io/jamba/reference/jam_rapply.md),
[`list2df()`](https://jmw86069.github.io/jamba/reference/list2df.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
[`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md),
[`relist_named()`](https://jmw86069.github.io/jamba/reference/relist_named.md),
[`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md),
[`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md),
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
df1 <- data.frame(City=c("New York", "Los Angeles", "San Francisco"),
   State=c("New York", "California", "California"))
df2 <- data.frame(Team=c("Yankees", "Mets", "Giants", "Dodgers"),
   City=c("New York", "New York", "San Francisco", "Los Angeles"))
df3 <- data.frame(State=c("New York", "California"),
   `State Population`=c(39.24e9, 8.468e9),
   check.names=FALSE)
mergeAllXY(df1, df3, df2)
#>            City      State State Population    Team
#> 1   Los Angeles California        8.468e+09 Dodgers
#> 2      New York   New York        3.924e+10 Yankees
#> 3      New York   New York        3.924e+10    Mets
#> 4 San Francisco California        8.468e+09  Giants

df4 <- data.frame(check.names=FALSE,
   CellLine=rep(c("ul3", "dH1A", "dH1B"), each=2),
   Treatment=c("Vehicle", "Dex"))
df4$CellLine <- factor(df4$CellLine,
   levels=c("ul3", "dH1A", "dH1B"))
df4$Treatment <- factor(df4$Treatment,
   levels=c("Vehicle", "Dex"))
df5 <- data.frame(
   Treatment=rep(c("Vehicle", "Dex"), each=3),
   Time=c("0h", "12h", "24h"))
df6 <- data.frame(check.names=FALSE,
   CellLine=c("ul3", "dH1A", "dH1B"),
   Type=c("Control", "KO", "KO"))
mergeAllXY(df4, df5, df6)
#>    CellLine Treatment Time    Type
#> 1      dH1A       Dex  12h      KO
#> 2      dH1A       Dex   0h      KO
#> 3      dH1A   Vehicle   0h      KO
#> 4      dH1A       Dex  24h      KO
#> 5      dH1A   Vehicle  24h      KO
#> 6      dH1A   Vehicle  12h      KO
#> 7      dH1B       Dex  12h      KO
#> 8      dH1B       Dex  24h      KO
#> 9      dH1B       Dex   0h      KO
#> 10     dH1B   Vehicle   0h      KO
#> 11     dH1B   Vehicle  12h      KO
#> 12     dH1B   Vehicle  24h      KO
#> 13      ul3       Dex   0h Control
#> 14      ul3       Dex  12h Control
#> 15      ul3       Dex  24h Control
#> 16      ul3   Vehicle  24h Control
#> 17      ul3   Vehicle   0h Control
#> 18      ul3   Vehicle  12h Control

# note the factor order is maintained
mergeAllXY(df4, df5, df6)$CellLine
#>  [1] dH1A dH1A dH1A dH1A dH1A dH1A dH1B dH1B dH1B dH1B dH1B dH1B ul3  ul3  ul3 
#> [16] ul3  ul3  ul3 
#> Levels: ul3 dH1A dH1B
mergeAllXY(df4, df5)$Treatment
#>  [1] Dex     Dex     Dex     Dex     Dex     Dex     Dex     Dex     Dex    
#> [10] Vehicle Vehicle Vehicle Vehicle Vehicle Vehicle Vehicle Vehicle Vehicle
#> Levels: Vehicle Dex

# merge "all" can append rows to a data.frame
df4b <- data.frame(check.names=FALSE,
   CellLine=rep("dH1C", 2),
   Treatment=c("Vehicle", "Dex"))
mergeAllXY(df4, df4b)
#>   CellLine Treatment
#> 1      ul3   Vehicle
#> 2      ul3       Dex
#> 3     dH1A   Vehicle
#> 4     dH1A       Dex
#> 5     dH1B   Vehicle
#> 6     dH1B       Dex
#> 7     dH1C   Vehicle
#> 8     dH1C       Dex

# factor order is maintained, new levels are appended
mergeAllXY(df4, df4b)$CellLine
#> [1] ul3  ul3  dH1A dH1A dH1B dH1B dH1C dH1C
#> Levels: ul3 dH1A dH1B dH1C

# merge proceeds except shows missing data
mergeAllXY(df4, df4b, df5, df6)
#>    CellLine Treatment Time    Type
#> 1       ul3       Dex   0h Control
#> 2       ul3   Vehicle   0h Control
#> 3       ul3       Dex  12h Control
#> 4       ul3       Dex  24h Control
#> 5       ul3   Vehicle  12h Control
#> 6       ul3   Vehicle  24h Control
#> 7      dH1A       Dex  12h      KO
#> 8      dH1A       Dex   0h      KO
#> 9      dH1A   Vehicle  12h      KO
#> 10     dH1A   Vehicle   0h      KO
#> 11     dH1A       Dex  24h      KO
#> 12     dH1A   Vehicle  24h      KO
#> 13     dH1B       Dex  24h      KO
#> 14     dH1B       Dex   0h      KO
#> 15     dH1B       Dex  12h      KO
#> 16     dH1B   Vehicle  24h      KO
#> 17     dH1B   Vehicle   0h      KO
#> 18     dH1B   Vehicle  12h      KO
#> 19     dH1C       Dex   0h    <NA>
#> 20     dH1C       Dex  12h    <NA>
#> 21     dH1C       Dex  24h    <NA>
#> 22     dH1C   Vehicle   0h    <NA>
#> 23     dH1C   Vehicle  12h    <NA>
#> 24     dH1C   Vehicle  24h    <NA>

# note that appending rows is tricky, the following is incorrect
df6b <- data.frame(check.names=FALSE,
   CellLine="dH1C",
   Type="KO")
mergeAllXY(df4, df4b, df5, df6, df6b)
#>    CellLine    Type Treatment Time
#> 1       ul3 Control       Dex   0h
#> 2       ul3 Control   Vehicle   0h
#> 3       ul3 Control       Dex  12h
#> 4       ul3 Control       Dex  24h
#> 5       ul3 Control   Vehicle  12h
#> 6       ul3 Control   Vehicle  24h
#> 7      dH1A      KO       Dex  12h
#> 8      dH1A      KO       Dex   0h
#> 9      dH1A      KO   Vehicle  12h
#> 10     dH1A      KO   Vehicle   0h
#> 11     dH1A      KO       Dex  24h
#> 12     dH1A      KO   Vehicle  24h
#> 13     dH1B      KO       Dex  24h
#> 14     dH1B      KO       Dex   0h
#> 15     dH1B      KO       Dex  12h
#> 16     dH1B      KO   Vehicle  24h
#> 17     dH1B      KO   Vehicle   0h
#> 18     dH1B      KO   Vehicle  12h
#> 19     dH1C      KO      <NA> <NA>
#> 20     dH1C    <NA>       Dex   0h
#> 21     dH1C    <NA>       Dex  12h
#> 22     dH1C    <NA>       Dex  24h
#> 23     dH1C    <NA>   Vehicle   0h
#> 24     dH1C    <NA>   Vehicle  12h
#> 25     dH1C    <NA>   Vehicle  24h

# but it can be resolved by merging df6 and df6b
mergeAllXY(df4, df4b, df5, mergeAllXY(df6, df6b))
#>    CellLine Treatment Time    Type
#> 1      dH1A       Dex   0h      KO
#> 2      dH1A   Vehicle   0h      KO
#> 3      dH1A       Dex  12h      KO
#> 4      dH1A       Dex  24h      KO
#> 5      dH1A   Vehicle  12h      KO
#> 6      dH1A   Vehicle  24h      KO
#> 7      dH1B       Dex   0h      KO
#> 8      dH1B       Dex  12h      KO
#> 9      dH1B   Vehicle   0h      KO
#> 10     dH1B   Vehicle  12h      KO
#> 11     dH1B       Dex  24h      KO
#> 12     dH1B   Vehicle  24h      KO
#> 13     dH1C       Dex   0h      KO
#> 14     dH1C       Dex  12h      KO
#> 15     dH1C       Dex  24h      KO
#> 16     dH1C   Vehicle   0h      KO
#> 17     dH1C   Vehicle  12h      KO
#> 18     dH1C   Vehicle  24h      KO
#> 19      ul3       Dex   0h Control
#> 20      ul3       Dex  12h Control
#> 21      ul3       Dex  24h Control
#> 22      ul3   Vehicle   0h Control
#> 23      ul3   Vehicle  12h Control
#> 24      ul3   Vehicle  24h Control

# it may be easier to recognize by sorting with mixedSortDF()
mixedSortDF(honorFactor=TRUE,
   mergeAllXY(df4, df4b, df5, mergeAllXY(df6, df6b)))
#>    CellLine Treatment Time    Type
#> 22      ul3   Vehicle   0h Control
#> 23      ul3   Vehicle  12h Control
#> 24      ul3   Vehicle  24h Control
#> 19      ul3       Dex   0h Control
#> 20      ul3       Dex  12h Control
#> 21      ul3       Dex  24h Control
#> 2      dH1A   Vehicle   0h      KO
#> 5      dH1A   Vehicle  12h      KO
#> 6      dH1A   Vehicle  24h      KO
#> 1      dH1A       Dex   0h      KO
#> 3      dH1A       Dex  12h      KO
#> 4      dH1A       Dex  24h      KO
#> 9      dH1B   Vehicle   0h      KO
#> 10     dH1B   Vehicle  12h      KO
#> 12     dH1B   Vehicle  24h      KO
#> 7      dH1B       Dex   0h      KO
#> 8      dH1B       Dex  12h      KO
#> 11     dH1B       Dex  24h      KO
#> 16     dH1C   Vehicle   0h      KO
#> 17     dH1C   Vehicle  12h      KO
#> 18     dH1C   Vehicle  24h      KO
#> 13     dH1C       Dex   0h      KO
#> 14     dH1C       Dex  12h      KO
#> 15     dH1C       Dex  24h      KO

# again, factor order is maintained
mergeAllXY(df4, df4b, df5, sort=FALSE, mergeAllXY(df6, df6b))$CellLine
#>  [1] ul3  ul3  ul3  ul3  ul3  ul3  dH1A dH1A dH1A dH1A dH1A dH1A dH1B dH1B dH1B
#> [16] dH1B dH1B dH1B dH1C dH1C dH1C dH1C dH1C dH1C
#> Levels: ul3 dH1A dH1B dH1C

# the result can be sorted properly
mixedSortDF(honorFactor=TRUE,
   mergeAllXY(df4, df4b, df5, mergeAllXY(df6, df6b)))
#>    CellLine Treatment Time    Type
#> 22      ul3   Vehicle   0h Control
#> 23      ul3   Vehicle  12h Control
#> 24      ul3   Vehicle  24h Control
#> 19      ul3       Dex   0h Control
#> 20      ul3       Dex  12h Control
#> 21      ul3       Dex  24h Control
#> 2      dH1A   Vehicle   0h      KO
#> 5      dH1A   Vehicle  12h      KO
#> 6      dH1A   Vehicle  24h      KO
#> 1      dH1A       Dex   0h      KO
#> 3      dH1A       Dex  12h      KO
#> 4      dH1A       Dex  24h      KO
#> 9      dH1B   Vehicle   0h      KO
#> 10     dH1B   Vehicle  12h      KO
#> 12     dH1B   Vehicle  24h      KO
#> 7      dH1B       Dex   0h      KO
#> 8      dH1B       Dex  12h      KO
#> 11     dH1B       Dex  24h      KO
#> 16     dH1C   Vehicle   0h      KO
#> 17     dH1C   Vehicle  12h      KO
#> 18     dH1C   Vehicle  24h      KO
#> 13     dH1C       Dex   0h      KO
#> 14     dH1C       Dex  12h      KO
#> 15     dH1C       Dex  24h      KO
```
