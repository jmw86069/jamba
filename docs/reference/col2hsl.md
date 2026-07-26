# convert R color to HSL color matrix

convert R color to HSL color matrix

## Usage

``` r
col2hsl(x, ...)
```

## Arguments

- x:

  `character` vector with R compatible colors.

- ...:

  additional arguments are ignored.

## Value

`numeric` matrix of H, S, L color values.

## Details

This function takes an R color and converts to an HSL matrix, using the
`farver` package
[`farver::decode_colour()`](https://farver.data-imaginist.com/reference/decode_colour.html)
the colorspace package, and
[`RGB`](https://colorspace.R-Forge.R-project.org/reference/RGB.html) and
[`polarLUV`](https://colorspace.R-Forge.R-project.org/reference/polarLUV.html)
functions. It is also used to maintain alpha transparency, to enable
interconversion via other color manipulation functions as well.

When `model="hsl"` this function uses
[`farver::decode_colour()`](https://farver.data-imaginist.com/reference/decode_colour.html)
and bypasses `colorspace`. In future the `colorspace` dependency will
likely be removed in favor of using `farver`. In any event,
`model="hsl"` is equivalent to using `model="polarLUV"` and
`fixup=TRUE`, except that it should be much faster.

## See also

Other jam color functions:
[`alpha2col()`](https://jmw86069.github.io/jamba/reference/alpha2col.md),
[`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md),
[`col2alpha()`](https://jmw86069.github.io/jamba/reference/col2alpha.md),
[`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
[`col2hsv()`](https://jmw86069.github.io/jamba/reference/col2hsv.md),
[`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md),
[`fixYellow()`](https://jmw86069.github.io/jamba/reference/fixYellow.md),
[`fixYellowHue()`](https://jmw86069.github.io/jamba/reference/fixYellowHue.md),
[`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md),
[`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md),
[`hsl2col()`](https://jmw86069.github.io/jamba/reference/hsl2col.md),
[`hsv2col()`](https://jmw86069.github.io/jamba/reference/hsv2col.md),
[`isColor()`](https://jmw86069.github.io/jamba/reference/isColor.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md),
[`rainbow2()`](https://jmw86069.github.io/jamba/reference/rainbow2.md),
[`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md),
[`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md),
[`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md),
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

## Examples

``` r
x <- c("#FF000044", "#FF0000", "firebrick");
names(x) <- x;
showColors(x)

xhsl <- col2hsl(x)
xhsl
#>         #FF000044 #FF0000 firebrick
#> H       0.0000000       0   0.00000
#> S     100.0000000     100  67.92453
#> L      50.0000000      50  41.56863
#> alpha   0.2666667       1   1.00000

xhex <- hsl2col(xhsl)
showColors(list(x=x,
   xhex=xhex),
   groupCellnotes=FALSE)


withr::with_par(list("mfrow"=c(4, 4), "mar"=c(0.2, 1, 4, 1)), {

for (H in seq(from=0, to=360, length.out=17)[-17]) {
S <- 75;
Lseq <- seq(from=15, to=95, by=10);
hsl_gradient <- hsl2col(
   H=H,
   S=85,
   L=Lseq);
hcl_gradient <- hcl2col(
   H=H,
   C=85,
   L=Lseq);
names(hsl_gradient) <- Lseq;
names(hcl_gradient) <- Lseq;
showColors(xaxt="n",
   list(
      hsl=hsl_gradient,
      hcl=hcl_gradient),
   main=paste0("Hue: ", round(H),
      "\nSat: ", S,
      "\nLum: (as labeled)"),
   groupCellnotes=FALSE)
}
})

```
