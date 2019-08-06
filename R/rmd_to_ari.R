# using ioslides
get_nslides_ioslides = function(rendered_file) {
  doc = xml2::read_html(rendered_file)
  num_slides = doc %>%
    rvest::html_nodes(xpath = "//slide") %>%
    rvest::html_attr("data-total-slides") %>%
    unique()
  num_slides = suppressWarnings(as.numeric(num_slides))
  num_slides = stats::na.omit(num_slides)
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = max(num_slides)
}

get_nslides_xaringan = function(rendered_file) {
  doc = xml2::read_html(rendered_file)
  num_slides = doc %>%
    rvest::html_nodes(
      xpath = '//div[ @class="remark-slide-number"]') %>%
    rvest::html_text("data-total-slides") %>%
    unique()
  num_slides = sub(".*/", "", num_slides)
  num_slides = trimws(num_slides)
  num_slides = suppressWarnings(as.numeric(num_slides))
  num_slides = stats::na.omit(num_slides)
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = max(num_slides)
}

get_nslides_slidy = function(rendered_file) {
  doc = xml2::read_html(rendered_file)
  num_slides = doc %>%
    rvest::html_nodes(
      xpath = '//div[ @class="toolbar"]')
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = num_slides %>%
    rvest::html_nodes("span")
  txt = num_slides %>%
    html_text() %>%
    grepl(pattern = "slide")
  num_slides = num_slides[txt]
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = num_slides %>%
    html_text() %>%
    unique()
  num_slides = sub(".*/", "", num_slides)
  num_slides = trimws(num_slides)
  num_slides = suppressWarnings(as.numeric(num_slides))
  num_slides = stats::na.omit(num_slides)
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = max(num_slides)
}


get_nslides = function(slides) {
  ##### rendering
  rendered_file = tempfile(fileext = ".html")
  {rdom::rdom(slides, filename = rendered_file); TRUE}

  L = list(get_nslides_ioslides,
           get_nslides_xaringan,
           get_nslides_slidy)
  n_slides_guess = vapply(L, function(x) x(rendered_file),
                          FUN.VALUE = numeric(1))
  n_slides_guess = stats::na.omit(n_slides_guess)
  if (length(n_slides_guess) > 1) {
    n_slides_guess = max(n_slides_guess)
  }
  if (length(n_slides_guess) == 0) {
    n_slides_guess = NA_real_
  }
  return(n_slides_guess)
}


#' Convert a Rmd to an Ari Document
#'
#' @param path path to Rmd file
#' @param script optional spoken script, otherwise taken from the
#' HTML comments
#' @param capture_method Either `"vectorized"` or `"iterative"`. The
#' vectorized mode is faster though it can cause screens to repeat.
#' If making a video from an [rmarkdown::ioslides_presentation]
#' you should use `"iterative"`.
#' @param webshot_args a list of arguments to pass to [webshot::webshot]
#' @param ... additional arguments to pass to [make_ari_document]
#' @param verbose print diagnostic messages
#'
#' @return The output of [make_ari_document]
#' @export
#'
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom xml2 read_html
#' @importFrom rdom rdom
#' @importFrom rmarkdown render
#' @importFrom webshot webshot
#' @importFrom stats na.omit
#'
#' @examples \dontrun{
#' path = system.file("extdata", "example.Rmd", package = "ariExtra")
#' res = rmd_to_ari(path)
#' res$output_file
#' }
rmd_to_ari = function(
  path,
  script = NULL,
  capture_method = c("iterative", "vectorized"),
  webshot_args = list(),
  ...,
  verbose = TRUE
) {

  if (is.null(script)) {
    if (verbose) {
      message("Parsing HTML comments for script")
    }
    paragraphs = parse_html_comments(path)
    script = tempfile(fileext = ".txt")
    if (verbose > 1) {
      message(paste0("script is at: ", script))
    }
    writeLines(paragraphs, script)
  } else {
    stopifnot(length(script) == 1)
    stopifnot(file.exists(script))
    paragraphs = readLines(script)
  }

  capture_method = match.arg(capture_method)
  if (!(capture_method %in% c("vectorized", "iterative"))) {
    stop('capture_method must be either "vectorized" or "iterative"')
  }

  if (verbose) {
    message("Rendering Rmd to HTML")
  }
  # render the HTML output
  tfile = tempfile(fileext = ".html")
  # need to figure out number of damn slides
  # either /slide for ioslides or --- for xaringan
  slides = rmarkdown::render(input = path, output_file = tfile)

  if (verbose) {
    message("Getting the number of slides")
  }
  n_slides_guess = get_nslides(slides)
  if (!is.na(n_slides_guess)) {
    n_slides_guess = length(paragraphs)
  }

  if (n_slides_guess != length(paragraphs)) {
    warning(
      paste0(
        "Number of slides don't seem to match the script",
        ", use <!-- ; --> for slides with no comment"
      )
    )
  }

  # Get the links
  if (file.exists(slides)) {
    slides <- normalizePath(slides)
    if (.Platform$OS.type == "windows") {
      slides <- paste0("file://localhost/", slides)
    } else {
      slides <- paste0("file://localhost", slides)
    }
  } else {
    stop("rendering the Rmd failed!")
  }

  # pngs = vapply()
  slide_nums <- seq.int(n_slides_guess)
  img_paths <- sapply(slide_nums, function(r) {
    tempfile(fileext = ".png")
  })

  if (verbose) {
    message("Running webshot to make PNGs")
  }
  args = as.list(webshot_args)
  if (capture_method == "vectorized") {
    args$url = paste0(slides, "#", slide_nums)
    args$file = img_paths
    do.call(webshot::webshot, args = args)
  } else {
    for (i in slide_nums) {
      args$url = paste0(slides, "#", i)
      args$file = img_paths[i]
      do.call(webshot::webshot, args = args)
    }
  }

  make_ari_document(
    images = img_paths,
    script = script, ..., verbose = verbose)

}
