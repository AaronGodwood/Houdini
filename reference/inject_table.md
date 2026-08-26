# Inject a Word XML table after the paragraph containing a bookmark

Inject a Word XML table after the paragraph containing a bookmark

## Usage

``` r
inject_table(session, bookmark_name, xml_string)
```

## Arguments

- session:

  A docx session returned by open_docx()

- bookmark_name:

  Name of the bookmark

- xml_string:

  A \<w:tbl\> XML string (from get_table_xml())

## Value

Invisibly TRUE on success, FALSE if bookmark not found
