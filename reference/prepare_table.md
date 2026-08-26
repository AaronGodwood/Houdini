# Prepare a combined table with filtering and exclusions applied

Common pipeline used by html.R and xml.R public functions.

## Usage

``` r
prepare_table(
  pages = NULL,
  path = NULL,
  excluded_cols = NULL,
  excluded_rows = NULL,
  excluded_header_rows = NULL,
  parameters = NULL,
  timelines = NULL,
  levels = NULL,
  hide_data = FALSE
)
```

## Arguments

- pages:

  Pre-parsed pages (from parse_rtf) or NULL

- path:

  RTF file path (used only if pages is NULL)

- excluded_cols:

  Integer vector of column indices to exclude

- excluded_rows:

  Integer vector of stable data-row IDs to exclude (as assigned by
  `parse_rtf`; equal to the row's position in the unfiltered combined
  table)

- excluded_header_rows:

  Integer vector of header row indices to exclude

- parameters:

  Parameter filter

- timelines:

  Timeline filter

- levels:

  Indent level filter

- hide_data:

  toggle to replace all data in tables with XX

## Value

list(combined, included_cols) where combined has rows filtered
