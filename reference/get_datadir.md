# Get data directory

Find out where the environmental data is stored

## Usage

``` r
get_datadir(datadir)
```

## Arguments

- datadir:

  character. Directory as passed to
  [`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md).
  This does not change the data directory used by
  [`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md)
  but only shows the result of passing a specific datadir. If `NULL` is
  passed then the `sdmpredictors_datadir` option is read. To set this
  run `options(sdmpredictors_datadir = "<your preferred directory>")` in
  every session or in your .RProfile.

## Value

Path to the data directory.

## See also

[`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md)
