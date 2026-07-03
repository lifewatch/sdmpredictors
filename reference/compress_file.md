# Compress a file

The internal method `compress_file` compresses a file and returns the
path to the result.

## Usage

``` r
compress_file(filename, outputdir, method="gzip", overwrite=FALSE, 
  remove=TRUE)
```

## Arguments

- filename:

  character. Path to the file that should be compressed.

- outputdir:

  character. Path to the output directory.

- method:

  character. The default value is `"gzip"`, the supported methods are
  `"gzip"` and `"bzip2"`.

- overwrite:

  logical. If `TRUE` and the output file already exists then the file is
  silently overwritten, otherwise an exception is thrown.

- remove:

  logical. If `TRUE` then the input file is remove afterwards, otherwise
  not.

## Value

The path of the compressed file.

## See also

[`decompress_file`](http://lifewatch.github.io/sdmpredictors/reference/decompress_file.md)
