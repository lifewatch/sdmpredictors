get_sysdata <- function() {
  # internal function used to load the layers information and statistics
  fname <- "sysdata.rda"
  dir <- getOption("sdmpredictors_datadir")
  if(is.null(dir)) {
    dir <- file.path(tempdir(), "sdmpredictors")
  }
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  tmp <- tempfile("sysdata_sdmpredictors", fileext = ".rda")
  outfile <- paste0(dir, "/", fname)
  
  ## only download every 60 minutes
  if(!file.exists(outfile) || difftime(Sys.time(), file.mtime(outfile), units = "mins") > 24*60 || file.size(outfile) <= 0) {
    ok <- -1
    try({
      urlroot <- .data$urlsysdata
      url <- paste0(urlroot, fname)
      ok <- utils::download.file(url, tmp, quiet = TRUE)
    }, silent = TRUE)
    if(ok == 0) {
      file.copy(tmp, outfile, overwrite = TRUE)
    }
  }
  if(file.exists(outfile)) {
    e <- new.env()
    load(outfile, envir = e)
    return(e$.data)
  } else {
    return(.data)
  }
}