# Generate Word XML for an RTF table

Generate Word XML for an RTF table

## Usage

``` r
get_table_xml(
  path,
  excluded_cols = NULL,
  excluded_rows = NULL,
  excluded_header_rows = NULL,
  parameters = NULL,
  timelines = NULL,
  levels = NULL,
  text_width_twips = NULL,
  pages = NULL,
  hide_data = FALSE
)
```

## Arguments

- path:

  Path to the .rtf file

- excluded_cols:

  Integer vector of 1-based column indices to exclude (NULL = none)

- excluded_rows:

  Integer vector of stable data-row IDs to exclude (NULL = none)

- excluded_header_rows:

  Integer vector of 1-based header row indices to exclude

- parameters:

  Character vector of parameter values to keep (NULL = all)

- timelines:

  Character vector of timeline labels to keep (NULL = all)

- levels:

  Character vector of indent level titles to keep (NULL = all)

- text_width_twips:

  Target table width in twips (NULL = use RTF widths)

- pages:

  Pre-parsed RTF pages (output of parse_rtf()); parsed from path if NULL

- hide_data:

  toggle to replace all data in tables with XX

## Value

Character string containing a \<w:tbl\> XML fragment
