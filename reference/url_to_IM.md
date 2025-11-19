# Extract Insight Maker model from URL

Create XML string from Insight Maker URL. For internal use; use
[`insightmaker_to_sfm()`](https://kcevers.github.io/sdbuildR/reference/insightmaker_to_sfm.md)
to import an Insight Maker model.

## Usage

``` r
url_to_IM(URL, file = NULL)
```

## Arguments

- URL:

  String with URL to an Insight Maker model

- file:

  If specified, file path to save Insight Maker model to. If NULL, do
  not save model.

## Value

XML string with Insight Maker model

## See also

[`insightmaker_to_sfm()`](https://kcevers.github.io/sdbuildR/reference/insightmaker_to_sfm.md)

## Examples

``` r
xml <- url_to_IM(
  URL =
    "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
)
```
