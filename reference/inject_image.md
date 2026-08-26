# Inject a PNG image after the paragraph containing a bookmark

Writes the PNG into word/media/, registers the relationship, then
inserts a minimal \<w:drawing\> block. The image is scaled to the
document text width maintaining the original aspect ratio from the RTF
`\picwgoal` / `\pichgoal`.

## Usage

``` r
inject_image(session, bookmark_name, png_bytes, width_twips, height_twips)
```

## Arguments

- session:

  A docx session returned by open_docx()

- bookmark_name:

  Name of the bookmark

- png_bytes:

  raw vector of PNG bytes (from extract_png())

- width_twips:

  Original image width in twips (from extract_png())

- height_twips:

  Original image height in twips (from extract_png())

## Value

Invisibly TRUE on success, FALSE if bookmark not found
