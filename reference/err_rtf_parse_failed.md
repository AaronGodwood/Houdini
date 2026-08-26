# RTF parsing failed at a specific stage

RTF parsing failed at a specific stage

## Usage

``` r
err_rtf_parse_failed(path, stage, cause = NULL)
```

## Arguments

- path:

  File path

- stage:

  One of: "split_pages", "parse_page", "parse_row", "extract_image"

- cause:

  Underlying condition or message string
