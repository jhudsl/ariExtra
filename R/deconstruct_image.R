# Deconstruct Movie into Scene Images with Scene Change Detection using FFMPEG
deconstruct_movie = function(file) {
  # Determine file extension of input movie file
  ext = tools::file_ext(file)
  # Create temp directory
  run_dir = tempfile()
  dir.create(run_dir)
  # Create new file
  new_file = tempfile(tmpdir = run_dir,
                      fileext = paste0(".", ext))
  # Copy input file into new_file
  file.copy(file, new_file)

  # Create temporary png and txt files
  tmp_type = tempfile(tmpdir = run_dir, fileext = "_%05d.png")
  tmp_time = tempfile(tmpdir = run_dir, fileext = ".txt")

  # Construct a system command that invokes ffmpeg program
  # Takes a video file as input,
  # Applies a filter to select frames based on scene change detection
  # Outputs the selected frames as individual PNG images
  args = c("-hide_banner", "-i",
           new_file,
           "-vf",
           paste0("select='isnan(prev_selected_t)+gt(scene\\,0.0001)',",
                  "metadata=print:file=", tmp_time),
           "-vsync", "vfr",
           tmp_type)
  # Run system command
  res = system2("ffmpeg", args = args)

  if (res != 0) {
    warning("Result was not zero!")
  }
  base = strsplit(basename(tmp_type), "_")[[1]][1]
  pngs = list.files(path = run_dir, pattern = paste0(base, ".*.png"),
                    full.names = TRUE)

  # Construct list with timecode file, list of extracted frame filenames,
  # and temporary frame file base format
  L = list(
    time_file = tmp_time,
    pngs = pngs,
    png_base_format = tmp_type
  )
  return(L)
}

# Extract Audio from Movie
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

