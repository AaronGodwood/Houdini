# Package index

## All functions

- [`apparate()`](https://aarongodwood.github.io/Houdini/reference/apparate.md)
  : Run the full houdini injection pipeline

- [`as_houdini_error()`](https://aarongodwood.github.io/Houdini/reference/as_houdini_error.md)
  :

  Return `e` unchanged if it is already a houdini_error, otherwise wrap
  it

- [`best_match()`](https://aarongodwood.github.io/Houdini/reference/best_match.md)
  : Best-matching candidate for a name

- [`bookmarks_contexts()`](https://aarongodwood.github.io/Houdini/reference/bookmarks_contexts.md)
  : Contexts of extracted bookmarks

- [`close_docx()`](https://aarongodwood.github.io/Houdini/reference/close_docx.md)
  : Finalise and save a Word document session

- [`combine_pages()`](https://aarongodwood.github.io/Houdini/reference/combine_pages.md)
  : Combine multiple pages into a single flat table

- [`err_bookmark_bad_context()`](https://aarongodwood.github.io/Houdini/reference/err_bookmark_bad_context.md)
  : Bookmark is inside a table cell, header, footer, or text box

- [`err_bookmark_duplicate()`](https://aarongodwood.github.io/Houdini/reference/err_bookmark_duplicate.md)
  : Duplicate bookmark mapping across config rows

- [`err_bookmark_missing()`](https://aarongodwood.github.io/Houdini/reference/err_bookmark_missing.md)
  : Bookmark not found in the Word document

- [`err_docx_unreadable()`](https://aarongodwood.github.io/Houdini/reference/err_docx_unreadable.md)
  : docx file cannot be read

- [`err_excel_unreadable()`](https://aarongodwood.github.io/Houdini/reference/err_excel_unreadable.md)
  : excel file cannot be read

- [`err_exclusion_out_of_range()`](https://aarongodwood.github.io/Houdini/reference/err_exclusion_out_of_range.md)
  : Excluded column index is out of range

- [`err_format()`](https://aarongodwood.github.io/Houdini/reference/err_format.md)
  : Format an error for display in the UI (message + hint on a new line
  if present)

- [`err_hint()`](https://aarongodwood.github.io/Houdini/reference/err_hint.md)
  : Return the hint field if the condition is a houdini_error, otherwise
  NULL

- [`err_image_extract_failed()`](https://aarongodwood.github.io/Houdini/reference/err_image_extract_failed.md)
  : PNG image could not be extracted from an RTF file

- [`err_image_inject_failed()`](https://aarongodwood.github.io/Houdini/reference/err_image_inject_failed.md)
  : Image injection failed

- [`err_rtf_parse_failed()`](https://aarongodwood.github.io/Houdini/reference/err_rtf_parse_failed.md)
  : RTF parsing failed at a specific stage

- [`err_rtf_unreadable()`](https://aarongodwood.github.io/Houdini/reference/err_rtf_unreadable.md)
  : RTF file cannot be read

- [`err_xml_inject_failed()`](https://aarongodwood.github.io/Houdini/reference/err_xml_inject_failed.md)
  : XML table injection failed

- [`extract_bookmarks()`](https://aarongodwood.github.io/Houdini/reference/extract_bookmarks.md)
  : Extract bookmark names from a Word document

- [`extract_png()`](https://aarongodwood.github.io/Houdini/reference/extract_png.md)
  : Extract the first PNG image from an RTF file

- [`extract_pngs()`](https://aarongodwood.github.io/Houdini/reference/extract_pngs.md)
  : Extract every PNG image from an RTF file, one per page

- [`filter_levels()`](https://aarongodwood.github.io/Houdini/reference/filter_levels.md)
  : Filter rows by indent level value

- [`filter_pages()`](https://aarongodwood.github.io/Houdini/reference/filter_pages.md)
  : Filter pages by parameter value

- [`filter_timelines()`](https://aarongodwood.github.io/Houdini/reference/filter_timelines.md)
  : Filter rows by timeline value

- [`get_levels()`](https://aarongodwood.github.io/Houdini/reference/get_levels.md)
  : Get all data/indent levels across pages

- [`get_parameters()`](https://aarongodwood.github.io/Houdini/reference/get_parameters.md)
  : Get all unique parameter values across pages

- [`get_table_html()`](https://aarongodwood.github.io/Houdini/reference/get_table_html.md)
  : Generate full HTML preview for an RTF file

- [`get_table_html_output()`](https://aarongodwood.github.io/Houdini/reference/get_table_html_output.md)
  : Generate output-pane HTML (excluded cols/rows fully hidden)

- [`get_table_html_selection()`](https://aarongodwood.github.io/Houdini/reference/get_table_html_selection.md)
  : Generate selection-pane HTML (interactive, data attributes, greying)

- [`get_table_info()`](https://aarongodwood.github.io/Houdini/reference/get_table_info.md)
  : Get summary info about an RTF file for the UI

- [`get_table_xml()`](https://aarongodwood.github.io/Houdini/reference/get_table_xml.md)
  : Generate Word XML for an RTF table

- [`get_timelines()`](https://aarongodwood.github.io/Houdini/reference/get_timelines.md)
  : Get all unique timeline values across pages

- [`houdini_app()`](https://aarongodwood.github.io/Houdini/reference/houdini_app.md)
  : Return the Houdini Shiny application object

- [`houdini_pin_folder()`](https://aarongodwood.github.io/Houdini/reference/houdini_pin_folder.md)
  : Pins a folder of RTF files to POSIT Connect

- [`houdini_preview()`](https://aarongodwood.github.io/Houdini/reference/houdini_preview.md)
  : Preview a single RTF table or figure

- [`houdini_validate()`](https://aarongodwood.github.io/Houdini/reference/houdini_validate.md)
  : Check a configuration before generating a document

- [`houdini_watch()`](https://aarongodwood.github.io/Houdini/reference/houdini_watch.md)
  : Watch an RTF folder and regenerate the document on every change

- [`image_parameters()`](https://aarongodwood.github.io/Houdini/reference/image_parameters.md)
  : Parameter values carried by each page of a figure RTF

- [`inject_image()`](https://aarongodwood.github.io/Houdini/reference/inject_image.md)
  : Inject a PNG image after the paragraph containing a bookmark

- [`inject_images()`](https://aarongodwood.github.io/Houdini/reference/inject_images.md)
  : Inject several PNG images, stacked, at one bookmark

- [`inject_table()`](https://aarongodwood.github.io/Houdini/reference/inject_table.md)
  : Inject a Word XML table after the paragraph containing a bookmark

- [`is_image_rtf()`](https://aarongodwood.github.io/Houdini/reference/is_image_rtf.md)
  : Check weather an RTF file contains an embedded image (PNG)

- [`match_score()`](https://aarongodwood.github.io/Houdini/reference/match_score.md)
  : Similarity score between two names in the range 0 to 1

- [`open_docx()`](https://aarongodwood.github.io/Houdini/reference/open_docx.md)
  : Open a Word document for editing

- [`parse_rtf()`](https://aarongodwood.github.io/Houdini/reference/parse_rtf.md)
  : Parse an RTF file into a list of page objects

- [`parse_xl()`](https://aarongodwood.github.io/Houdini/reference/parse_xl.md)
  : Parse a excel config into config + selections

- [`prepare_table()`](https://aarongodwood.github.io/Houdini/reference/prepare_table.md)
  : Prepare a combined table with filtering and exclusions applied

- [`process_document()`](https://aarongodwood.github.io/Houdini/reference/process_document.md)
  : Process a Word document: inject all configured RTF content at
  bookmarks

- [`read_xlsx()`](https://aarongodwood.github.io/Houdini/reference/read_xlsx.md)
  : Read a houdini xlsx workbook

- [`resolve_cols()`](https://aarongodwood.github.io/Houdini/reference/resolve_cols.md)
  : Resolve column specification to integer indices

- [`run_app()`](https://aarongodwood.github.io/Houdini/reference/run_app.md)
  : Launch the Houdini shiny app

- [`slice_range()`](https://aarongodwood.github.io/Houdini/reference/slice_range.md)
  : Resolve a start/end row range to integer indices

- [`table_info_from_pages()`](https://aarongodwood.github.io/Houdini/reference/table_info_from_pages.md)
  : Get summary info from already-parsed pages (used by app.R's cache)

- [`warn_filter_not_found()`](https://aarongodwood.github.io/Houdini/reference/warn_filter_not_found.md)
  : Filter not found
