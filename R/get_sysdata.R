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
  dir <- normalizePath(dir)
  tmp <- tempfile("sysdata_sdmpredictors", fileext = ".rda")
  outfile <- file.path(dir, fname)
  data <- .data
  ## only download every 60 minutes
  if(!file.exists(outfile) || difftime(Sys.time(), file.mtime(outfile), units = "mins") > 24*60 || file.size(outfile) <= 0) {
    ok <- -1
    try({
      urlroot <- .data$urlsysdata
      url <- paste0(urlroot, fname)
      ok <- utils::download.file(url, tmp, quiet = TRUE)
      if(ok == 0) {
        e <- new.env()
        load(tmp, envir = e)
        if(!is.null(e$.data$creation) && e$.data$creation > .data$creation) {
          data <- e$.data
          file.copy(tmp, outfile, overwrite = TRUE)
        }
      }
    }, silent = TRUE)
  }
  return(data)
}