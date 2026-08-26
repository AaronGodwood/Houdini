# Generate output-pane HTML (excluded cols/rows fully hidden)

Generate output-pane HTML (excluded cols/rows fully hidden)

## Usage

``` r
get_table_html_output(
  path,
  excluded_cols = NULL,
  excluded_rows = NULL,
  excluded_header_rows = NULL,
  parameters = NULL,
  timelines = NULL,
  levels = NULL,
  pages = NULL,
  hide_data = FALSE
)
```

## Arguments

- path:

  Path to the .rtf file

- excluded_cols:

  Integer vector of 1-based column indices to remove

- excluded_rows:

  Integer vector of stable data-row IDs to remove

- excluded_header_rows:

  Integer vector of 1-based header row indices

- parameters:

  Character vector of parameter values to keep (NULL = all)

- timelines:

  Character vector of timeline labels to keep (NULL = all)

- levels:

  Character vector of indent level titles to keep (NULL = all)

- pages:

  Pre-parsed RTF pages (output of parse_rtf()); parsed from path if NULL

- hide_data:

  toggle to replace all data in tables with XX

## Value

HTML string
