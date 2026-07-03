# Decompress a file

The internal method `decompress_file` decompresses a file and returns
the path to the result.

## Usage

``` r
decompress_file(filename, outputdir, overwrite=FALSE, remove=TRUE)
```

## Arguments

- filename:

  character. Path to the file that should be decompressed.

- outputdir:

  character. Path to the output directory.

- overwrite:

  logical. If `TRUE` and the output file already exists then the file is
  silently overwritten, otherwise an exception is thrown.

- remove:

  logical. If `TRUE` then the input file is removed afterwards,
  otherwise not.

## Value

The path of the decompressed file.

## See also

[`compress_file`](http://lifewatch.github.io/sdmpredictors/reference/compress_file.md)
