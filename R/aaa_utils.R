# Indicate which elements of x are blank (contain only whitespace)
blank_lines = function(x) {
  grepl("^\\s*$", x)
}

# Indicates whether all elements of x are blank or contain only whitespace characters
is_blank = function(x) {
  if (length(x) > 0) {
    all(blank_lines(x))
  }
  else {
    TRUE
  }
}

# (Taken from rmarkdown::: partition_yaml_front_matter)
# Returns YAML front_matter and body
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

# Reads text file encoded in UTF-8 format and returns content of character vector
read_utf8 <- function(file, encoding = 'UTF-8') {
  if (inherits(file, 'connection')) {
    con <- file
  } else {
    con <- base::file(file, encoding = encoding); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}

# Get text from html comments in an Rmd
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
    # Replace any remaining HTML comment tags
    result[i] <- sub("<!--", "", result[i])
    result[i] <- sub("-->", "", result[i])
  }

  trimws(result)
}

# Extract comments from file
parse_speak_comments <- function(path) {
  lines_ <- readLines(path, warn = FALSE)
  starts <- grep("```\\{speak", lines_)
  ends <- grep("```\\s*$", lines_)
  ends = sapply(starts, function(x) {
    min(ends[ ends > x])
  })

  if (length(starts) != length(ends)) {
    stop("There's a comment open/close mismatch.")
  }
  starts = starts + 1
  ends = ends - 1

  result <- rep(NA, length(starts))

  for (i in seq_along(starts)) {
    if (starts[i] == ends[i]) {
      # Single line
      result[i] <- lines_[starts[i]]
    } else if (starts[i] < ends[i]) {
      # nothing there
    }  else {
      # Multiple lines
      result[i] <- paste(trimws(lines_[starts[i]:ends[i]]),
                         collapse = " ")
    }
    # Replace any remaining HTML comment tags
    result[i] <- sub("<!--", "", result[i])
    result[i] <- sub("-->", "", result[i])
  }
  # Assign empty string to any NA elements
  result[is.na(result)] = ""

  trimws(result)
}


# Parse comments from a xaringan slide deck
parse_xaringan_comments <- function(path) {
  # Read in lines of file
  lines_ <- readLines(path, warn = FALSE)
  # Partition YAML front matter
  lines_ <- partition_yaml_front_matter(lines_)
  # Extract body of slide deck
  lines_ <- lines_$body

  # Identify positions of comments and slide breaks
  starts <- grep("^\\?\\?\\?\\s*$", lines_)
  slides <- grep("^---\\s*$", lines_)
  # Comments for title slides
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


# Returns type of operating system
os_type <- function() {
  .Platform$OS.type
}

# Determine type of operating system
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

# Constructs an URL to export an image file from a Google Slides
type_url <- function(id, page_id = NULL, type = "png") {
  url <- paste0("https://docs.google.com/presentation/d/",
                id, "/export/", type, "?id=", id)
  if (!is.null(page_id)) {
    url = paste0(url, "&pageid=", page_id)
  }
  url
}

png_url = type_url

# Constructs an URL to export to pptx
pptx_url = function(id) {
  type_url(id, page_id = NULL, type = "pptx")
}

# Constructs an URL to export to pdf
pdf_url = function(id) {
  type_url(id, page_id = NULL, type = "pdf")
}

# Extract page IDs of slides in a Google Slides presentation
#' @importFrom jsonlite fromJSON
get_page_ids = function(id) {
  id = get_slide_id(id)
  url = paste0("https://docs.google.com/presentation/d/", id)
  tfile = tempfile(fileext = ".html")
  res = httr::GET(url, httr::write_disk(tfile))
  httr::stop_for_status(res)
  cr = httr::content(res)
  script = rvest::html_nodes(cr, xpath ="//script")
  script = rvest::html_text(script)
  script = unique(script)
  script = gsub("DOCS_modelChunk = undefined;", "", script)
  script = script[ grepl("DOCS_modelChunk\\s=\\s\\[", x = script)]

  all_types = c("PREDEFINED_LAYOUT_UNSPECIFIED",
                "BLANK",
                "CAPTION_ONLY",
                "TITLE",
                "TITLE_AND_BODY",
                "TITLE_AND_TWO_COLUMNS",
                "TITLE_ONLY",
                "SECTION_HEADER",
                "SECTION_TITLE_AND_DESCRIPTION",
                "ONE_COLUMN_TEXT",
                "MAIN_POINT",
                "BIG_NUMBER",
                paste0("CUSTOM_", 1:100))
  types = paste0(all_types, collapse = "|")
  # script = script[grepl(types, script)]
  ss = strsplit(script, "; DOC")
  ss = lapply(ss, trimws)
  ss = lapply(ss, function(x) {
    x[!grepl("^DOC", x)] = paste0(" DOC", x[!grepl("^DOC", x)])
    x
  })
  ss = lapply(ss, function(x) {
    x = x[grepl("^DOCS_modelChunk\\s=\\s\\[", x)]
    x = x[ !x %in% "DOCS_modelChunk = undefined"]
    x = sub("^DOCS_modelChunk\\s=\\s\\[", "[", x)
    x
  })
  ss = unlist(ss)
  pages = lapply(ss, jsonlite::fromJSON)
  pages = sapply(pages, function(x) {
    x = x[sapply(x, function(r) any(unlist(r) %in% all_types))]
    x = x[length(x)]
    x
  })
  pages = sapply(pages, function(x) {
    if (length(x) < 2) {
      if (length(x) == 0) {
        return(NA)
      }
      x = x[[1]]
      if (length(x) < 2) {
        return(NA)
      }
    }
    x[[2]]
  })
  pages = pages[ !is.na(pages) ]
  if (length(pages) >= 2) {
    pages = c(pages[1], grep("^g", pages[2:length(pages)], value = TRUE))
  }
  if (pages[1] != "p") {
    pages = unique(c("p", pages))
  }
  urls = type_url(id = id, page_id = pages)
  pages = pages[check_png_urls(urls)]
  pages
}

# Check if vector of URLs is valid (Status Code = 200)
check_png_urls <- function(urls) {
  res = vapply(urls, function(url) {
    tfile = tempfile(fileext = ".png")
    ret = httr::GET(url)
    httr::status_code(ret) == 200
  }, FUN.VALUE = logical(1))
  return(res)
}

# Downloads PNG image files to local file system
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
get_slide_id <- function(x) {
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
make_slide_url <- function(x) {
  x = get_slide_id(x)
  x = paste0("https://docs.google.com/presentation/d/",x)
  x
}

#' @rdname get_slide_id
#' @export
#' @examples
#' x = "https://drive.google.com/drive/folders/1pXBQQdd1peI56GtQT-jEZ59xSmhqQlFC?usp=sharing"
#' get_folder_id(x)
#' x = "1pXBQQdd1peI56GtQT-jEZ59xSmhqQlFC"
#' get_folder_id(x)
get_folder_id = function(x) {
  res = httr::parse_url(x)
  x = res$path
  x = sub(".*folders/", "", x)
  x = sub("[?].*", "", x)
  x = x[ nchar(x) > 5]
  x = trimws(x)
  x
}
