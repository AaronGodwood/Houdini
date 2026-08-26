# Getting started with Houdini

Houdini inserts tables and figures into a Word document. You give it
three things - a Word template with named bookmarks, a folder of RTF
files, and a mapping between them (usually from an excel document) - and
it writes a finished document with Word tables at each bookmark.

This vignette builds a small report from scratch: two tables, one
figure, and a filter.

    #> [1] "/tmp/Rtmpgr2byL/escapology_study/rtf/t_reactions.rtf"
    #> [1] "/tmp/Rtmpgr2byL/escapology_study/rtf/t_noise.rtf"
    #> [1] "/tmp/Rtmpgr2byL/escapology_study/rtf/f_applause.rtf"

## What you start with

A Word document containing bookmarks where tables or figures should go:

``` r

names(Houdini:::extract_bookmarks(template))
#> [1] "Table_Reactions" "Table_Noise"     "Figure_Applause"
```

And a folder of RTF files, one per table or figure:

``` r

list.files(rtf_dir)
#> [1] "f_applause.rtf"  "t_noise.rtf"     "t_reactions.rtf"
```

## Looking at a table first

Before wiring anything up,
[`houdini_preview()`](../reference/houdini_preview.md) renders a single
RTF so you can see how Houdini has parsed it. It writes a self-contained
HTML page and opens it; `browse = FALSE` just returns the path.

``` r

preview <- houdini_preview(file.path(rtf_dir, "t_noise.rtf"), browse = FALSE)
```

The page header tells you what Houdini found:

4 columns × 6 rows · 2 pages · parameters: Audience Noise (DB),
No. Fainters (n)

Two pages, one per parameter, which is what makes the parameter filter
below work. Preview accepts the same filters as a real run, so you can
check a selection before committing it to a config:

``` r

noise_only <- houdini_preview(
  file.path(rtf_dir, "t_noise.rtf"),
  parameters = "Audience Noise (DB)",
  browse = FALSE
)
```

## Mapping bookmarks to tables

The mapping is done via an Excel document or a data frame with the same
columns. `Bookmark` and `Dataset` are required; the rest are optional
filters.

``` r

config <- data.frame(
  Bookmark   = c("Table_Reactions", "Table_Noise", "Figure_Applause"),
  Dataset    = c("t_reactions.rtf", "t_noise.rtf", "f_applause.rtf"),
  Parameters = c("", "Audience Noise (DB)", ""),
  Levels     = c("Trick Class", "", ""),
  stringsAsFactors = FALSE
)
config
#>          Bookmark         Dataset          Parameters      Levels
#> 1 Table_Reactions t_reactions.rtf                     Trick Class
#> 2     Table_Noise     t_noise.rtf Audience Noise (DB)            
#> 3 Figure_Applause  f_applause.rtf
```

Two filters are doing work here:

- `Parameters` keeps only the *Audience Noise (DB)* page of the two-page
  table.
- `Levels` keeps only top-level rows of the reactions table, dropping
  the indented trick names beneath each class.

Both accept several values separated by semicolons,
e.g. `"Week 1; Week 4"`.

## Checking the config before you run

[`houdini_validate()`](../reference/houdini_validate.md) runs every
check the app shows in its warnings panel, without opening the app and
without writing anything. It reports the whole list at once rather than
stopping at the first problem, which is what you want in a scheduled
job:

``` r

problems <- houdini_validate(template, config, rtf_dir, quiet = TRUE)
nrow(problems)
#> [1] 0
```

A clean config returns no rows. When something is wrong you get one row
per problem, with the config row it came from and a hint:

``` r

broken <- config
broken$Bookmark[2] <- "Table_Nosie"   # a typo
houdini_validate(template, broken, rtf_dir, quiet = TRUE)[, c("row", "severity", "message")]
#>   row severity                                                message
#> 1   2    error Bookmark 'Table_Nosie' not found in the Word document.
```

In a CI step, stop on anything that would produce a wrong document:

``` r

problems <- houdini_validate(template, config, rtf_dir)
if (any(problems$severity == "error")) stop("config is not ready")
```

## Generating the document

``` r

status <- apparate(template, config, rtf_dir, quiet = TRUE)
output <- sub(".docx$", "_Houdini_Output.docx", template)
file.exists(output)
#> [1] TRUE
```

[`apparate()`](../reference/apparate.md) returns a per-row status list
rather than stopping at the first problem so if one table throws an
error the rest can still run fine:

``` r

vapply(status, function(s) if (is.null(s$err)) "ok" else "error", character(1))
#>    1    2    3 
#> "ok" "ok" "ok"
```

A failing row carries a structured error with a hint. Note this runs
against a copy of the template: each run writes its output beside its
input, so pointing a second run at the same template would overwrite the
document just produced.

``` r

scratch <- file.path(project, "scratch.docx")
file.copy(template, scratch)
#> [1] TRUE

bad <- apparate(scratch,
                data.frame(Bookmark = "No_Such_Bookmark",
                           Dataset  = "t_noise.rtf",
                           stringsAsFactors = FALSE),
                rtf_dir, quiet = TRUE)
conditionMessage(bad[["1"]]$err)
#> [1] "Bookmark 'No_Such_Bookmark' not found in the Word document."
Houdini:::err_hint(bad[["1"]]$err)
#> [1] "Check that the bookmark name in the config table matches exactly (case-sensitive) the bookmark in the Word file."
```

## Checking what came out

The generated document holds proper Word tables, not images of tables:

``` r

doc <- xml2::read_xml(read_docx_xml(output))
ns <- c(w = W)
c(tables   = length(xml2::xml_find_all(doc, ".//w:tbl", ns = ns)),
  drawings = length(xml2::xml_find_all(doc, ".//w:drawing", ns = ns)))
#>   tables drawings 
#>        2        1
```

The filters took effect. Only the requested parameter survived:

``` r

texts <- xml2::xml_text(xml2::xml_find_all(doc, ".//w:t", ns = ns))
c("Audience Noise page kept" = any(grepl("82.4", texts)),
  "Fainters page dropped"    = !any(grepl("3.1", texts)))
#> Audience Noise page kept    Fainters page dropped 
#>                     TRUE                     TRUE
```

And the level filter kept the trick classes while dropping the
individual tricks:

``` r

c("Illusions kept"   = any(grepl("Illusions", texts)),
  "Apparition dropped" = !any(grepl("Apparition", texts)))
#>     Illusions kept Apparition dropped 
#>               TRUE               TRUE
```

## Working from Excel

For anything reproducible, keep the mapping in a excel document.
`Export Excel` in the app writes this format for you.

``` r

sheet <- file.path(project, "config.xlsx")
writexl::write_xlsx(config, sheet)

from_excel <- file.path(project, "from_excel.docx")
file.copy(template, from_excel)
#> [1] TRUE

status <- apparate(from_excel, sheet, rtf_dir, quiet = TRUE)
vapply(status, function(s) if (is.null(s$err)) "ok" else "error", character(1))
#>    1    2    3 
#> "ok" "ok" "ok"
```

If you want to pass a data frame just pass that rather than the excel
file path. When `quiet` is `FALSE`, a log is written next to the output
recording every row, the filters applied, and any warnings.

## Using the app

[`run_app()`](../reference/run_app.md) opens the same pipeline as a
Shiny app. It is the easier way to build a configuration in the first
place: load the template and RTF folder, click a row to preview it, and
click column headers or rows in the preview to exclude them.

``` r

run_app()
```

The two workflows share a format, so the usual pattern is to build a
configuration interactively, export it to Excel, then run it can from a
script whenever the RTFs are regenerated or reimported back into the
app.

## Regenerating

Re-running over a document that Houdini already produced replaces the
injected content in place rather than adding a second copy, so it is
safe to run repeatedly as the source RTFs change:

``` r

again <- apparate(output, config, rtf_dir, quiet = TRUE)
final <- sub("\\.docx$", "_Houdini_Output.docx", output)

doc2 <- xml2::read_xml(read_docx_xml(final))
length(xml2::xml_find_all(doc2, ".//w:tbl", ns = c(w = W)))
#> [1] 2
```

Still three pieces of content, not six.

## Where to go next

- [`?apparate`](../reference/apparate.md) for the full set of run
  options
- [`?houdini_preview`](../reference/houdini_preview.md) for previewing a
  single file
- [`?parse_xl`](../reference/parse_xl.md) for the recognised config
  columns
