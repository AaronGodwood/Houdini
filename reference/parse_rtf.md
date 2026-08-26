# Parse an RTF file into a list of page objects

Parse an RTF file into a list of page objects

## Usage

``` r
parse_rtf(path, hide_data = FALSE)
```

## Arguments

- path:

  Path to the .rtf file

- hide_data:

  toggle to replace all data in tables with XX

## Value

List of page objects, each with:

- parameter:

  character or NA

- header:

  block of header rows (see section 4)

- data:

  block of data rows, with stable `row_id`s
