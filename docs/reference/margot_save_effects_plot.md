# Save an effects figure at print size

Writes a figure from
[`margot_plot_effects()`](https://go-bayes.github.io/margot/reference/margot_plot_effects.md)
at its exact print size: a vector PDF with embedded fonts (through
[`grDevices::cairo_pdf()`](https://rdrr.io/r/grDevices/cairo.html)) and
a PNG (through
[`ragg::agg_png()`](https://ragg.r-lib.org/reference/agg_png.html)). The
white margin lies inside the stated size.

## Usage

``` r
margot_save_effects_plot(plot, file, formats = c("pdf", "png"), dpi = 600)
```

## Arguments

- plot:

  Figure from
  [`margot_plot_effects()`](https://go-bayes.github.io/margot/reference/margot_plot_effects.md).

- file:

  Output path without extension; `.pdf` and `.png` are added.

- formats:

  Any of `"pdf"` and `"png"`.

- dpi:

  PNG resolution. Default 600.

## Value

The written paths, invisibly.

## See also

[`margot_plot_effects()`](https://go-bayes.github.io/margot/reference/margot_plot_effects.md)

## Examples

``` r
# \donttest{
effects <- data.frame(outcome = c("a", "b"), estimate = c(0.1, -0.02),
  lower = c(0.04, -0.07), upper = c(0.16, 0.03), conf_level = 0.95)
fig <- margot_plot_effects(effects, xlab = "Difference, SD units", width_mm = 89)
if (capabilities("cairo")) {
  margot_save_effects_plot(fig, file.path(tempdir(), "effects"), formats = "pdf")
}
#> Warning: unable to load shared object '/Library/Frameworks/R.framework/Resources/modules//R_X11.so':
#>   dlopen(/Library/Frameworks/R.framework/Resources/modules//R_X11.so, 0x0006): Library not loaded: /opt/X11/lib/libSM.6.dylib
#>   Referenced from: <12ACBF54-73E1-3691-8F9A-0BB7076A39D5> /Library/Frameworks/R.framework/Versions/4.6/Resources/modules/R_X11.so
#>   Reason: tried: '/opt/X11/lib/libSM.6.dylib' (fat file, but missing compatible architecture (have 'i386,x86_64', need 'arm64e, arm64e.v1, arm64, arm64')), '/System/Volumes/Preboot/Cryptexes/OS/opt/X11/lib/libSM.6.dylib' (no such file), '/opt/X11/lib/libSM.6.dylib' (fat file, but missing compatible architecture (have 'i386,x86_64', need 'arm64e, arm64e.v1, arm64, arm64')), '/Library/Frameworks/R.framework/Resources/lib/libSM.6.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk-21.0.8+9/Contents/Home/lib/server/libSM.6.dylib' (no such file)
#> Warning: unable to load shared object '/Library/Frameworks/R.framework/Resources/library/grDevices/libs//cairo.so':
#>   dlopen(/Library/Frameworks/R.framework/Resources/library/grDevices/libs//cairo.so, 0x0006): Library not loaded: /opt/X11/lib/libXrender.1.dylib
#>   Referenced from: <6832203D-652F-3360-B263-24468A45CF1B> /Library/Frameworks/R.framework/Versions/4.6/Resources/library/grDevices/libs/cairo.so
#>   Reason: tried: '/opt/X11/lib/libXrender.1.dylib' (fat file, but missing compatible architecture (have 'i386,x86_64', need 'arm64e, arm64e.v1, arm64, arm64')), '/System/Volumes/Preboot/Cryptexes/OS/opt/X11/lib/libXrender.1.dylib' (no such file), '/opt/X11/lib/libXrender.1.dylib' (fat file, but missing compatible architecture (have 'i386,x86_64', need 'arm64e, arm64e.v1, arm64, arm64')), '/Library/Frameworks/R.framework/Resources/lib/libXrender.1.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk-21.0.8+9/Contents/Home/lib/server/libXrender.1.dylib' (no such file)
#> Warning: failed to load cairo DLL
# }
```
