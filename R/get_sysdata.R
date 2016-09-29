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
  outfile <- paste0(dir, "/", fname)
  
  ## only download every 60 minutes
  if(!file.exists(outfile) || difftime(Sys.time(), file.mtime(outfile), units = "mins") > 60) {
    try({
      urlroot <- .data$urlsysdata
      url <- paste0(urlroot, fname)
      utils::download.file(url, outfile, quiet = TRUE)
    }, silent = TRUE)
  }
  if(file.exists(outfile) && file.size(outfile) > 0) {
    e <- new.env()
    load(outfile, envir = e)
    return(e$.data)
  } else {
    return(.data)
  }
}