# Extract the first PNG image from an RTF file

Locates the first `\{\pict ... \pngblip ... <hex>\}` group, decodes the
hex-encoded bytes to raw, and returns the image data plus its declared
dimensions in twips (`\picwgoal` / `\pichgoal`).

## Usage

``` r
extract_png(path)
```

## Arguments

- path:

  Path to the .rtf file

## Value

list(png_bytes = raw, width_twips = integer, height_twips = integer) or
NULL if no PNG found
