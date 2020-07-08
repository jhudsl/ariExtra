####################################################
# Wrapper functions
####################################################
mario_auth = function(api_key = NULL) {
  auth_hdr = NULL
  if (nzchar(api_key) && !is.null(api_key)) {
    auth_hdr = httr::add_headers(
      Authorization = paste0("Key ", api_key))
  }
  auth_hdr
}

mario_voices = mario = function(
  service = NULL,
  api_url = "https://rsconnect.biostat.jhsph.edu/ario",
  api_key = Sys.getenv("CONNECT_API_KEY"),
  ...
) {

  auth_hdr = mario_auth(api_key)
  query = list()
  query$service = service
  response = httr::GET(
    url = paste0(api_url, "/list_voices"),
    query = query,
    auth_hdr, ...)
  httr::stop_for_status(response)
  out = jsonlite::fromJSON(
    httr::content(response, as = "text"),
    flatten = TRUE)
  out
  # response
}

mario = function(
  file,
  script = NULL,
  api_url = "https://rsconnect.biostat.jhsph.edu/ario",
  api_key = Sys.getenv("CONNECT_API_KEY"),
  voice = NULL,
  service = NULL,
  target = NULL,
  token = NULL,
  ...
) {
  auth_hdr = mario_auth(api_key)

  if (all(file.exists(file))) {
    zipfile = tempfile(fileext = ".zip")
    utils::zip(zipfile, files = file)
    file = zipfile
    body = list(
      file = httr::upload_file(file)
    )
  } else {
    # google slide ids
    body = list(
      file = file
    )
  }


  if (is.character(script) &&
      !file.exists(script)) {
    message("writing out script to a file")
    paragraphs = script
    script = tempfile(fileext = ".txt")
    writeLines(paragraphs, script)
  }
  if (!is.null(script) && file.exists(script)) {
    script = httr::upload_file(script)
  }

  body$script = script
  body$service = service
  body$voice = voice
  if (!is.null(target) && is.null(token)) {
    stop("If target specified, token needs to be set")
  }
  body$target = target
  if (!is.null(token)) {
    if (inherits(token, "Token")) {
      tokenfile = tempfile(fileext = ".rds")
      saveRDS(token, file = tokenfile)
      token = tokenfile
    }
    stopifnot(file.exists(token))
    token = httr::upload_file(token)
  }
  body$token = token

  response = httr::POST(
    url = paste0(api_url, "/to_ari"),
    body = body,
    auth_hdr, ...)
  response
}

mario_write_video = function(response) {
  httr::stop_for_status(response)
  bin_data = httr::content(response)
  bin_data = bin_data$video[[1]]
  bin_data = base64enc::base64decode(bin_data)
  output = tempfile(fileext = ".mp4")
  writeBin(bin_data, output)
  output
}

mario_subtitles = function(response) {
  httr::stop_for_status(response)
  bin_data = httr::content(response)
  bin_data = bin_data$subtitles[[1]]
  bin_data = base64enc::base64decode(bin_data)
  rawToChar(bin_data)
}

open_video = function(response, open = TRUE) {
  output = mario_write_video(response)
  if (open) {
    system2("open", output)
  }
  output
}
