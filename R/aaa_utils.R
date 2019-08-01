read_utf8 <- function(file, encoding = 'UTF-8') {
  if (inherits(file, 'connection')) con <- file else {
    con <- base::file(file, encoding = encoding); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}


parse_html_comments <- function(path) {
  lines_ <- readLines(path, warn = FALSE)
  starts <- grep("<!--", lines_)
  ends <- grep("-->", lines_)

  if (length(starts) != length(ends)) {
    stop("There's a comment open/close mismatch.")
  }

  result <- rep(NA, length(starts))

  for (i in seq_along(starts)) {
    if (starts[i] == ends[i]) {
      # Single line
      result[i] <- lines_[starts[i]]
    } else {
      # Multiple lines
      result[i] <- paste(trimws(lines_[starts[i]:ends[i]]),
                         collapse = " ")
    }
    result[i] <- sub("<!--", "", result[i])
    result[i] <- sub("-->", "", result[i])
  }

  trimws(result)
}



os_type <- function() {
  .Platform$OS.type
}

sys_type <- function() {
  if (os_type() == "windows") {
    "windows"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "macos"
  } else if (Sys.info()["sysname"] == "Linux") {
    "linux"
  } else if (os_type() == "unix") {
    # "unix"
    "linux"
  } else {
    stop("Unknown OS")
  }
}


type_url = function(id, page_id = NULL, type = "png") {
  url = paste0(
    "https://docs.google.com/presentation/d/",
    id, "/export/", type, "?id=", id)
  if (!is.null(page_id)) {
    url = paste0(url, "&pageid=", page_id)
  }
  url
}
png_url = type_url
pptx_url = function(id) {
  type_url(id, page_id = NULL, type = "pptx")
}
pdf_url = function(id) {
  type_url(id, page_id = NULL, type = "pdf")
}

download_png_urls = function(urls) {
  res = vapply(urls, function(url) {
    tfile = tempfile(fileext = ".png")
    httr::GET(url, httr::write_disk(tfile))
    tfile
  }, FUN.VALUE = character(1))
  return(res)
}



#' Get Slide ID from URL
#'
#' @param x URL of slide
#'
#' @return A character vector
#' @export
#'
#' @examples
#' x = paste0("https://docs.google.com/presentation/d/",
#' "1Tg-GTGnUPduOtZKYuMoelqUNZnUp3vvg_7TtpUPL7e8",
#' "/edit#slide=id.g154aa4fae2_0_58")
#' get_slide_id(x)
get_slide_id = function(x) {
  x = sub(".*presentation/", "", x)
  x = sub("/d/e", "/d", x) # if you publish by accident
  x = sub("^(d|e)/", "", x)
  x = strsplit(x, "/")[[1]]
  x = x[ !grepl("^(edit|pub|export|png)", x)]
  x = x[ nchar(x) > 5]
  x
}

#' @export
#' @rdname get_slide_id
make_slide_url = function(x) {
  x = get_slide_id(x)
  x = paste0("https://docs.google.com/presentation/d/",x)
  x
}
