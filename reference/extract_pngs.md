# Extract every PNG image from an RTF file, one per page

A figure RTF holds one `\pict` group per `\sectd` page, and each page
carries its own "Parameter: " line in the page header, exactly as a
table RTF does. Pages are returned in document order so the caller can
filter them by parameter and insert what remains.

## Usage

``` r
extract_pngs(path, parameters = NULL)
```

## Arguments

- path:

  Path to the .rtf file

- parameters:

  Optional character vector of parameter values to keep. Pages whose
  parameter is NA are always kept, matching filter_pages().

## Value

list(images, warnings) where images is a list of list(png_bytes,
width_twips, height_twips, parameter), possibly empty
