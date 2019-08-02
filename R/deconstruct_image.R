#' @examples 
#' # from https://www.youtube.com/watch?v=gb7aQhgdFZk
#' res = deconstruct_movie("~/Desktop/test/opal_grad.mp4")
deconstruct_movie = function(file) {
  ext = tools::file_ext(file)
  run_dir = tempfile()
  dir.create(run_dir)
  new_file = tempfile(
    tmpdir = run_dir,
    fileext = paste0(".", ext))
  file.copy(file, new_file)
  tmp_type = tempfile(tmpdir = run_dir, fileext = "_%05d.png")
  tmp_time = tempfile(tmpdir = run_dir, fileext = ".txt")
  args = c("-hide_banner", "-i", 
    new_file,
    "-vf", 
    paste0("select='isnan(prev_selected_t)+gt(scene\\,0.0001)',", 
           "metadata=print:file=", tmp_time),
    "-vsync", "vfr",
    tmp_type)
  res = system2("ffmpeg", args = args)
  if (res != 0) {
    warning("Result was not zero!")
  }
  base = strsplit(basename(tmp_type), "_")[[1]][1]
  pngs = list.files(path = run_dir, pattern = paste0(base, ".*.png"),
             full.names = TRUE)
  
  L = list(
    time_file = tmp_time,
    pngs = pngs,
    png_base_format = tmp_type
  ) 
  return(L)
}

extract_movie_audio = function(file) {
  wav = tempfile(fileext = ".wav")
  args = c("-hide_banner", "-i", 
           file,
           wav)
  res = system2("ffmpeg", args = args)
  if (res != 0) {
    warning("Result was not zero!")
  }
  return(wav)
}
