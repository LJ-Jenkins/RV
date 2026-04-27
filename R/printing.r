#' @export
"print.RV::Registry" <- function(
  x, width = getOption("width") - 20L, give.attr = FALSE, ...
) {
  utils::str(x, width = width, give.attr = give.attr, ...)
}

#' @export
"print.RV::Schema" <- function(
  x, width = getOption("width") - 20L, give.attr = FALSE, ...
) {
  utils::str(x, width = width, give.attr = give.attr, ...)
}

#' @export
"print.RV::Validator" <- function(
  x, width = getOption("width") - 20L, give.attr = FALSE, ...
) {
  utils::str(x, width = width, give.attr = give.attr, ...)
}

backtick <- function(x) paste0("`", x, "`", collapse = ", ")

paste_as_path <- function(x) {
  paste0(
    "[[",
    lapply(x, function(x) {
      if (is.character(x)) paste0("'", x, "'") else x
    }),
    "]]",
    collapse = ""
  )
}

# based on lobstr::tree()
# all credit to authors
error_tree <- function(
  x,
  max_depth,
  max_width,
  max_rows
) {
  state <- new.env(parent = emptyenv())
  state$n <- 0L
  state$truncated <- FALSE

  truncate_line <- function(line, trunc_prefix = "msg") {
    if (nchar(line, type = "width") <= max_width) {
      return(line)
    }
    j <- paste0(" ...[", trunc_prefix, " truncated]")
    paste0(substr(line, 1, max(1, max_width - nchar(j, type = "width"))), j)
  }

  txt <- error_tree_lines(
    x,
    prefix = "",
    depth = 0L,
    max_depth = max_depth,
    max_width = max_width,
    max_rows = max_rows,
    state = state,
    trunc_fn = truncate_line
  )

  if (state$truncated) {
    txt <- c(txt, "...[errors truncated]")
  }

  paste0(txt, collapse = "\n")
}

error_tree_lines <- function(
  x,
  prefix,
  depth,
  max_depth,
  max_width,
  max_rows,
  state,
  trunc_fn
) {
  stopifnot(is.list(x))

  n <- length(x)
  if (n == 0) {
    return(character())
  }
  lines <- character()

  for (i in seq_along(x)) {
    if (state$n >= max_rows) {
      state$truncated <- TRUE
      break
    }

    name <- names(x)[i]
    val <- x[[i]]
    last <- i == n

    branch <- if (last) "└─" else "├─"
    line_prefix <- paste0(prefix, branch)

    if (depth >= max_depth) {
      line <- paste0(line_prefix, " ...[msg truncated]")
      lines <- c(lines, trunc_fn(line, trunc_prefix = "depth"))
      state$n <- state$n + 1L
      next
    }

    if (is.list(val)) {
      line <- paste0(line_prefix, name)
      lines <- c(lines, trunc_fn(line))
      state$n <- state$n + 1L

      child_prefix <- paste0(prefix, if (last) "  " else "│ ")

      child_lines <- error_tree_lines(
        val,
        prefix = child_prefix,
        depth = depth + 1L,
        max_depth = max_depth,
        max_width = max_width,
        max_rows = max_rows,
        state = state,
        trunc_fn = trunc_fn
      )

      lines <- c(lines, child_lines)
    } else {
      txt <- val
      line <- paste0(line_prefix, name, ': "', txt, '"')
      lines <- c(lines, trunc_fn(line))
      state$n <- state$n + 1L
    }
  }

  lines
}
