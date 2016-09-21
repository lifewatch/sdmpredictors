get_sysdata <- function() {
  # internal function used to load the layers information and statistics
  fname <- "sysdata.rda"
  outfile <- paste0(get_datadir(NULL), "/", fname)
  
  ## only download every 60 minutes
  if(!file.exists(outfile) || difftime(Sys.time(), file.mtime(outfile), units = "mins") > 60) {
    tryCatch({
      urlroot <- .data$urlsysdata
      url <- paste0(urlroot, fname)
      download.file(url, outfile, quiet = TRUE)
    }, error = {}, finally = {})
  }
  if(file.exists(outfile) && file.size(outfile) > 0) {
    e <- new.env()
    load(outfile, envir = e)
    return(e$.data)
  } else {
    return(.data)
  }
}