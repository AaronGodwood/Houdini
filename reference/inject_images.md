# Inject several PNG images, stacked, at one bookmark

A figure RTF can hold one image per page. They are inserted in document
order, each as its own paragraph, so the bookmark ends up with the whole
set rather than only the first.

## Usage

``` r
inject_images(session, bookmark_name, images)
```

## Arguments

- session:

  A docx session returned by open_docx()

- bookmark_name:

  Name of the bookmark

- images:

  List of list(png_bytes, width_twips, height_twips), from
  extract_pngs()

## Value

Invisibly TRUE
