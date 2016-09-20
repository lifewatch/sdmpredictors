get_sysdata <- function() {
  fname <- "sysdata.rda"
  outfile <- paste0(get_datadir(NULL), "/", fname)
  ## only download every 5 minutes
  if(!file.exists(outfile) || difftime(Sys.time(), file.mtime(outfile), units = "mins") > 5) {
    tryCatch({
      urlroot <- "http://www.phycology.ugent.be/research/sdmpredictors/"
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