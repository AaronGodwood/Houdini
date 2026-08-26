# Validate a config against a document and its tables

Validate a config against a document and its tables

## Usage

``` r
validate_config(
  config,
  bookmarks = character(),
  tables = character(),
  selections = list(),
  info = list()
)
```

## Arguments

- config:

  data.frame with Bookmark and Table columns

- bookmarks:

  Named vector of bookmarks in the document, as returned by
  extract_bookmarks(); its "context" attribute is used when present

- tables:

  Character vector of table names available in the RTF folder

- selections:

  Per-row selections keyed by row index as character

- info:

  Named list of table_info_from_pages() results, keyed by table name.
  Rows whose table is absent here skip the filter checks.

## Value

A list of findings, each list(row, severity, message, hint, bookmark,
table). Severity is "error" or "warning".
