#' Compress a file
#' 
#' The internal method \code{compress_file} compresses a file and returns the
#' path to the result.
#' 
#' @usage compress_file(filename, outputdir, method = "gzip", overwrite = FALSE,
#' remove = TRUE)
#' 
#' @param filename character. Path to the file that should be compressed.
#' @param outputdir character. Path to the output directory.
#' @param method character. The default value is \code{"gzip"}, the supported
#'   methods are \code{"gzip"} and \code{"bzip2"}.
#' @param overwrite logical. If \code{TRUE} and the output file already exists
#'   then the file is silently overwritten, otherwise an exception is thrown.
#' @param remove logical. If \code{TRUE} then the input file is remove
#'   afterwards, otherwise not.
#'   
#' @return The path of the compressed file.
#'   
#' @seealso \code{\link{decompress_file}}
compress_file <- function(filename, outputdir, method="gzip", overwrite=FALSE, remove=TRUE) {
  # @examples
  # print("TODO examples compress_file")
  base <- basename(filename)
  outputpath <- paste0(outputdir, "/", base)
  if(method == "gzip") {
    outputpath <- paste0(outputpath, ".gz")
    R.utils::gzip(filename, destname=outputpath, temporary=FALSE, skip=FALSE,
                  overwrite=overwrite, remove=remove, BFR.SIZE=1e+07, compression=9)
  } else if (method == "bzip2") {
    outputpath <- paste0(outputpath, ".bz2")
    R.utils::bzip2(filename, destname=outputpath, temporary=FALSE, skip=FALSE,
                   overwrite=overwrite, remove=remove, BFR.SIZE=1e+07, compression=9)
  } else {
    stop(sprintf("Argument method is not supported: %s", method))
  }
  return(outputpath)
}

#' Decompress a file
#' 
#' The internal method \code{decompress_file} decompresses a file and returns
#' the path to the result.
#' 
#' @usage decompress_file(filename, outputdir, overwrite = FALSE, remove = TRUE)
#' 
#' @param filename character. Path to the file that should be decompressed.
#' @param outputdir character. Path to the output directory.
#' @param overwrite logical. If \code{TRUE} and the output file already exists
#'   then the file is silently overwritten, otherwise an exception is thrown.
#' @param remove logical. If \code{TRUE} then the input file is removed
#'   afterwards, otherwise not.
#'   
#' @return The path of the decompressed file.
#'   
#' @seealso \code{\link{compress_file}}
decompress_file <- function(filename, outputdir, overwrite=FALSE, remove=TRUE) {
  # @examples 
  # #decompress_file("compressed/sst_mean.gri.gz", "decompressed", overwrite = FALSE)
  # #decompress_file("compressed/gri.bz2", "decompressed", overwrite = TRUE)
  # #decompress_file("compressed/gri.bz2", "decompressed", remove = TRUE)
  # #decompress_file("compressed/sst_mean.gri.invalidextension", decompressed")
  # print("TODO examples decompress_file")
  base <- basename(filename)
  extension <- tools::file_ext(base)
  outputpath <- paste0(outputdir,"/", tools::file_path_sans_ext(base))
  if (extension == "gz") {
    R.utils::gunzip(filename, destname=outputpath, temporary=FALSE, skip=FALSE, 
                    overwrite=FALSE, remove=TRUE, BFR.SIZE=1e+07)
  } else if (extension == "bz2") {
    R.utils::bunzip2(filename, destname=outputpath, temporary=FALSE, skip=FALSE, 
                     overwrite=FALSE, remove=TRUE, BFR.SIZE=1e+07)
  } else {
    stop(sprintf("Filename extension is not supported: %s", extension))
  }
  return(outputpath)
}