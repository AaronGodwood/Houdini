# wrappers.R - marking injected content so it can be found again.


# Prefix identifying a wrapper. Anything under this namespace is ours.
WRAPPER_PREFIX <- "_Houdini"

# Characters of hash carried in the wrapper name
HASH_CHARS <- 8L

# Word caps bookmark names at 40 characters
MAX_BOOKMARK_NAME <- 40L

# Fingerprint of a table's visible content.
content_hash <- function(xml_string_or_node) {
  node <- if (inherits(xml_string_or_node, c("xml_node", "xml_document"))) {
    xml_string_or_node
  } else {
    read_xml(xml_string_or_node)
  }
  hash_texts(node_texts(node))
}

# Visible text of one or more nodes, in document order.
node_texts <- function(nodes) {
  if (inherits(nodes, c("xml_node", "xml_document"))) nodes <- list(nodes)
  unlist(lapply(nodes, function(n) {
    xml_text(xml_find_all(n, "descendant-or-self::w:t", ns = c(w = W_NS)))
  }), use.names = FALSE)
}

# Single place the digest is computed, so the two callers cannot drift apart.
hash_texts <- function(texts) {
  substr(digest::digest(paste0(texts, collapse = ""), algo = "md5"),
         1L, HASH_CHARS)
}

# Wrapper bookmark name for a target bookmark, carrying the content hash.
#
#   _Houdini_<bookmark>_<hash>
wrapper_name <- function(bookmark, hash = NULL) {
  safe <- gsub("[^A-Za-z0-9_]", "_", bookmark)
  suffix <- if (is.null(hash)) "" else paste0("_", hash)
  room <- MAX_BOOKMARK_NAME - nchar(WRAPPER_PREFIX) - 1L - nchar(suffix)
  if (nchar(safe) > room) safe <- substr(safe, 1L, room)
  paste0(WRAPPER_PREFIX, "_", safe, suffix)
}

# Match any wrapper for `bookmark`, whatever hash it carries (or none, for
# wrappers written before hashing existed).
wrapper_pattern <- function(bookmark) {
  safe <- gsub("[^A-Za-z0-9_]", "_", bookmark)
  room <- MAX_BOOKMARK_NAME - nchar(WRAPPER_PREFIX) - 1L - 1L - HASH_CHARS
  if (nchar(safe) > room) safe <- substr(safe, 1L, room)
  paste0("^", WRAPPER_PREFIX, "_", safe, "(_[0-9a-f]{", HASH_CHARS, "})?$")
}

# The hash recorded in a wrapper name, or NA when it carries none.
wrapper_hash <- function(name) {
  m <- regmatches(name, regexpr(sprintf("_[0-9a-f]{%d}$", HASH_CHARS), name))
  if (length(m) == 0L) NA_character_ else substring(m, 2L)
}

is_wrapper_name <- function(name) {
  !is.na(name) & startsWith(name, WRAPPER_PREFIX)
}

# Locate the wrapper belonging to `bookmark`, whatever hash its name carries.
# Returns list(start, end, name, hash), or NULL when absent - the normal case
# for a document generated before wrappers existed.
find_wrapper <- function(doc, bookmark) {
  starts <- xml_find_all(doc, ".//w:bookmarkStart", ns = c(w = W_NS))
  if (length(starts) == 0L) return(NULL)

  names <- xml_attr(starts, "name")
  hit <- which(!is.na(names) & grepl(wrapper_pattern(bookmark), names))
  if (length(hit) == 0L) return(NULL)

  start <- starts[[hit[1L]]]
  id <- xml_attr(start, "id")
  if (is.na(id)) return(NULL)

  end <- xml_find_first(
    doc, sprintf(".//w:bookmarkEnd[@w:id='%s']", id), ns = c(w = W_NS))
  if (inherits(end, "xml_missing")) return(NULL)

  list(start = start, end = end, name = names[hit[1L]],
       hash = wrapper_hash(names[hit[1L]]))
}

# Hash of what currently sits inside a wrapper, computed the same way as
# content_hash() does for outgoing XML so the two are comparable.
wrapper_content_hash <- function(wrap) {
  hash_texts(node_texts(wrapper_inner_nodes(wrap)))
}

# Nodes sitting strictly between the two markers: the previously injected
# content. Empty when the markers are adjacent, which happens if a user
# deleted the table but left the wrapper in place.
wrapper_inner_nodes <- function(wrap) {
  id <- xml_attr(wrap$start, "id")
  out <- list()
  node <- xml_find_first(wrap$start, "following-sibling::*[1]",
                         ns = c(w = W_NS))
  while (!inherits(node, "xml_missing")) {
    if (xml_name(node) == "bookmarkEnd" &&
        identical(xml_attr(node, "id"), id)) {
      break
    }
    out[[length(out) + 1L]] <- node
    node <- xml_find_first(node, "following-sibling::*[1]", ns = c(w = W_NS))
  }
  out
}

# Replace a wrapper's contents with `xml_block`, leaving the markers in place
# and restamping the name with the new content's hash. Any image relationships
# held by the outgoing content are released first, or the media files
# accumulate in the package on every run.
replace_wrapper_contents <- function(session, wrap, xml_block, bookmark,
                                     hash = NULL) {
  inner <- wrapper_inner_nodes(wrap)
  for (node in inner) cleanup_image_rels(session, node)

  # Insert first, then drop the old nodes: adding after the start marker keeps
  # the new content inside the fence regardless of how many nodes are removed.
  xml_add_sibling(wrap$start, xml_block, .where = "after")
  for (node in inner) xml_remove(node)

  if (!is.null(hash)) {
    xml_attr(wrap$start, "w:name") <- wrapper_name(bookmark, hash)
  }
  invisible(TRUE)
}

# Fence `xml_block` between a fresh pair of markers placed after `node`.
# Mutates session$next_bmk_id so ids stay unique within the document.
wrap_new_content <- function(session, node, xml_block, name) {
  id <- session$next_bmk_id
  session$next_bmk_id <- id + 1L

  # Built back to front: each insertion goes immediately after `node`, so
  # emitting end, then content, then start leaves them in the right order.
  xml_add_sibling(node, read_xml(sprintf(
    '<w:bookmarkEnd xmlns:w="%s" w:id="%d"/>', W_NS, id)), .where = "after")
  xml_add_sibling(node, xml_block, .where = "after")
  xml_add_sibling(node, read_xml(sprintf(
    '<w:bookmarkStart xmlns:w="%s" w:id="%d" w:name="%s"/>', W_NS, id, name)),
    .where = "after")
  invisible(TRUE)
}
