blank_lines = function(x) {
  grepl("^\\s*$", x)
}
is_blank = function (x) {
  if (length(x))
    all(blank_lines(x))
  else TRUE
}
# taken from rmarkdown::: partition_yaml_front_matter
partition_yaml_front_matter = function(input_lines)
{
  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 && (
      delimiters[2] - delimiters[1] >
      1) && grepl("^---\\s*$", input_lines[delimiters[1]])) {
      if (delimiters[1] == 1)
        TRUE
      else is_blank(input_lines[1:delimiters[1] - 1])
    }
    else {
      FALSE
    }
  }
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {
    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]
    input_body <- c()
    if (delimiters[1] > 1)
      input_body <- c(input_body,
                      input_lines[1:delimiters[1] -
                                    1])
    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body, input_lines[-(1:delimiters[2])])
    list(front_matter = front_matter, body = input_body)
  }
  else {
    list(front_matter = NULL, body = input_lines)
  }
}


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


parse_xaringan_comments <- function(path) {
  lines_ <- readLines(path, warn = FALSE)
  lines_ = partition_yaml_front_matter(lines_)
  lines_ = lines_$body
  starts <- grep("^\\?\\?\\?\\s*$", lines_)
  slides <- grep("^---\\s*$", lines_)
  # comments for title slides
  comments_on_title_slide = FALSE
  if (length(starts) == 0) {
    lines_ = lines_[slides[1]:length(lines_)]
  } else {
    if (min(starts) < min(slides)) {
      comments_on_title_slide = TRUE
      lines_ = lines_[starts[1]:length(lines_)]
    } else {
      lines_ = lines_[slides[1]:length(lines_)]
    }
  }

  # if (comments_on_title_slide) {
  # added for title_slide
    lines_ = c("---", lines_)
  # }
  starts <- grep("^\\?\\?\\?\\s*$", lines_)
  ends <- grep("^---\\s*$", lines_)
  ends = unique(ends)



  res = mapply(
    function(from, to, index) {
      x = seq(from = from, to = to)
      vals = lines_[x]
      data.frame(line_number = x,
                 vals = vals,
                 slide_number = index,
                 stringsAsFactors = FALSE)
    }, ends, c(ends[-1] - 1, length(lines_)), 1:length(ends),
    SIMPLIFY = FALSE)
  res = do.call(rbind, res)
  res$comment_line = res$line_number %in% starts
  res = split(res, res$slide_number)
  # if more than one htis may be an issue
  res = lapply(res, function(x) {
    if (any(x$comment_line)) {
      comment_lines = x$line_number[ x$comment_line ]
      # remove the ???
      x$comment_line[ x$line_number == comment_lines] = FALSE
      x$comment_line[ x$line_number > max(comment_lines)] = TRUE
    }
    return(x)
  })
  res = do.call(rbind, res)
  rownames(res) = NULL

  # need to remove code here somehow!!
  res = split(res, res$slide_number)
  # if more than one htis may be an issue
  result = sapply(res, function(x) {
    paste(x$vals[x$comment_line], collapse = " ")
  })
  result = unname(result)

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
