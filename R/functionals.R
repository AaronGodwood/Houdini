prev_apply <- function(data, prev = NULL, counter = 1, f){
  if(purrr::is_empty(data))
     return(c())
  return(c(f(data[1],prev,counter),prev_apply(data[-1],data[1],(counter + 1), f)))

}


dapply <- function(x, bookmarks, datasets , footnotes, jmp_tbl,width){
  if(is_empty(bookmarks))
  {
    return(x)
  }
  x <- tryCatch(
    {
      print(bookmarks[1])
      if(is_table(bookmarks[1])){
        new_table <- prep_table(datasets[1],footnotes = footnotes[1])

        add_table(
          dapply(x,bookmarks[-1],datasets[-1],footnotes[-1],jmp_tbl,width),
          bookmark = bookmarks[1],
          table = new_table,
          jmp_tbl = jmp_tbl
        )
      }
      else
      {
        img_width <- (get_png_size(datasets[1])[["width"]])/96
        img_height <- (get_png_size(datasets[1])[["height"]])/96
        new_height <- img_height*(width/img_width)
        add_figure(
          dapply(x,bookmarks[-1],datasets[-1],footnotes[-1],jmp_tbl,width),
          bookmarks[1],
          datasets[1],
          jmp_tbl,
          width = width,
          height = new_height)
      }
    },
    error = function(e){
      print(e)
      dapply(x,bookmarks[-1],datasets[-1], footnotes[-1], jmp_tbl, width)
    }
  )

}
