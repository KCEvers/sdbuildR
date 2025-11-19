# Modify header of stock-and-flow model

The header of a stock-and-flow model contains metadata about the model,
such as the name, author, and version. Modify the header of an existing
model with standard or custom properties.

## Usage

``` r
header(
  sfm,
  name = "My Model",
  caption = "My Model Description",
  created = Sys.time(),
  author = "Me",
  version = "1.0",
  URL = "",
  doi = "",
  ...
)
```

## Arguments

- sfm:

  Stock-and-flow model, object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

- name:

  Model name. Defaults to "My Model".

- caption:

  Model description. Defaults to "My Model Description".

- created:

  Date the model was created. Defaults to Sys.time().

- author:

  Creator of the model. Defaults to "Me".

- version:

  Model version. Defaults to "1.0".

- URL:

  URL associated with model. Defaults to "".

- doi:

  DOI associated with the model. Defaults to "".

- ...:

  Optional other entries to add to the header.

## Value

A stock-and-flow model object of class
[`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md)

## Examples

``` r
sfm <- xmile() |>
  header(
    name = "My first model",
    caption = "This is my first model",
    author = "Kyra Evers",
    version = "1.1"
  )
```
