# Extract bookmark names from a Word document

Extract bookmark names from a Word document

## Usage

``` r
extract_bookmarks(docx_path)
```

## Arguments

- docx_path:

  Path to the .docx file

## Value

Named character vector: bookmark name -\> bookmark id (also carries a
`"context"` attribute that is retrieved with `bookmark_contexts()`)
