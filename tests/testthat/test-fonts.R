test_that("is_webfont_id() recognises Fontsource ids", {
  expect_true(is_webfont_id("eb-garamond"))
  expect_true(is_webfont_id("stix-two-text"))
  expect_true(is_webfont_id("lora"))
  expect_true(is_webfont_id("source-serif-4"))

  # System fonts and CSS generic families are not webfont ids
  expect_false(is_webfont_id("Times New Roman"))
  expect_false(is_webfont_id("Georgia"))
  expect_false(is_webfont_id("EB Garamond"))
  expect_false(is_webfont_id("serif"))
  expect_false(is_webfont_id("sans-serif"))
  expect_false(is_webfont_id("monospace"))
  expect_false(is_webfont_id(""))
  expect_false(is_webfont_id(NA_character_))
  expect_false(is_webfont_id(2))
})


test_that("webfont_css() builds all four faces for a webfont id", {
  css <- webfont_css("eb-garamond")
  expect_type(css, "character")
  expect_length(css, 1)

  # One @font-face rule per face, each declaring the id as the family
  expect_equal(stringr::str_count(css, stringr::fixed("@font-face")), 4)
  expect_equal(stringr::str_count(css, stringr::fixed("font-family: 'eb-garamond'")), 4)
  expect_true(grepl("font-style: normal; font-weight: 400", css, fixed = TRUE))
  expect_true(grepl("font-style: normal; font-weight: 700", css, fixed = TRUE))
  expect_true(grepl("font-style: italic; font-weight: 400", css, fixed = TRUE))
  expect_true(grepl("font-style: italic; font-weight: 700", css, fixed = TRUE))
  expect_true(grepl("@fontsource/eb-garamond@5/files/eb-garamond-latin-400-normal.woff2", css, fixed = TRUE))

  # Non-regular faces fall back to the regular file, so fonts lacking a face
  # (e.g. no italics) stay in-family instead of dropping to the default font
  rules <- strsplit(css, "\n")[[1]]
  regular_rule <- rules[grepl("font-style: normal; font-weight: 400", rules)]
  italic_rule <- rules[grepl("font-style: italic; font-weight: 400", rules)]
  expect_equal(stringr::str_count(regular_rule, stringr::fixed("url(")), 1)
  expect_equal(stringr::str_count(italic_rule, stringr::fixed("url(")), 2)
  expect_true(grepl("eb-garamond-latin-400-italic.woff2) format('woff2'), url(https://cdn.jsdelivr.net/npm/@fontsource/eb-garamond@5/files/eb-garamond-latin-400-normal.woff2", italic_rule, fixed = TRUE))
})


test_that("webfont_css() is NULL for system fonts", {
  expect_null(webfont_css("Times New Roman"))
  expect_null(webfont_css("serif"))
})


test_that("the default font is bundled and embedded offline", {
  # The shipped default must be present on disk so plots render without
  # internet; if this fails the font files are missing from inst/fonts/
  default <- getOption("sdbuildR.font_family", default = "stix-two-text")
  dir <- webfont_local_dir(default)
  expect_true(nzchar(dir))
  faces <- vapply(webfont_faces(), function(f) f$stem, character(1))
  for (stem in faces) {
    expect_true(file.exists(
      file.path(dir, sprintf("%s-latin-%s.woff2", default, stem))
    ))
  }

  # A bundled font embeds its bytes as data: URIs, with no CDN request
  css <- webfont_css(default)
  expect_equal(stringr::str_count(css, stringr::fixed("@font-face")), 4)
  expect_true(grepl("data:font/woff2;base64,", css, fixed = TRUE))
  expect_false(grepl("jsdelivr", css, fixed = TRUE))
  # The base64 in each data URI must be unbroken (no whitespace)
  expect_false(grepl("base64,[A-Za-z0-9+/=]*\\s", css))
})


test_that("non-bundled webfont ids link the CDN, not a data URI", {
  expect_equal(webfont_local_dir("eb-garamond"), "")
  css <- webfont_css("eb-garamond")
  expect_true(grepl("jsdelivr", css, fixed = TRUE))
  expect_false(grepl("data:font", css, fixed = TRUE))
})


test_that("apply_webfont() attaches CSS and marks the widget", {
  pl <- plotly::plot_ly()
  pl2 <- apply_webfont(pl, "eb-garamond")

  expect_identical(attr(pl2, "sdbuildR_webfont"), "eb-garamond")
  prepended <- as.character(htmltools::tagList(pl2$prepend))
  expect_true(grepl("@font-face", prepended, fixed = TRUE))
  expect_true(grepl("eb-garamond", prepended, fixed = TRUE))

  # System fonts leave the widget untouched
  pl3 <- apply_webfont(pl, "Times New Roman")
  expect_identical(pl3, pl)
  expect_null(attr(pl3, "sdbuildR_webfont"))
})


test_that("plot methods attach webfonts for webfont ids", {
  sfm <- stockflow("sir")

  # Stock-and-flow diagram (grViz)
  pl_sfm <- plot(sfm, font_family = "eb-garamond")
  expect_identical(attr(pl_sfm, "sdbuildR_webfont"), "eb-garamond")

  # The shipped default is the stix-two-text webfont
  expect_identical(attr(plot(sfm), "sdbuildR_webfont"), "stix-two-text")

  # Simulation plot (plotly)
  sim <- simulate(sfm)
  pl_sim <- plot(sim, font_family = "eb-garamond")
  expect_identical(attr(pl_sim, "sdbuildR_webfont"), "eb-garamond")
  expect_identical(attr(plot(sim), "sdbuildR_webfont"), "stix-two-text")

  # System fonts attach nothing
  expect_null(attr(plot(sim, font_family = "Times New Roman"), "sdbuildR_webfont"))
})


test_that("the sdbuildR.font_family option sets the default font", {
  sfm <- stockflow("sir")

  withr::local_options(sdbuildR.font_family = "Times New Roman")
  expect_null(attr(plot(sfm), "sdbuildR_webfont"))
  expect_true(grepl("fontname=\"Times New Roman\"", plot(sfm)$x$diagram, fixed = TRUE))

  withr::local_options(sdbuildR.font_family = "spectral")
  expect_identical(attr(plot(sfm), "sdbuildR_webfont"), "spectral")

  # An explicit font_family always wins over the option
  expect_identical(attr(plot(sfm, font_family = "lora"), "sdbuildR_webfont"), "lora")
})


test_that("set_plot_font() restyles plots at export time", {
  withr::local_options(sdbuildR.font_family = "Times New Roman")
  sfm <- stockflow("sir")

  # grViz: fontname attributes in the DOT source are rewritten
  pl_sfm <- set_plot_font(plot(sfm), "eb-garamond")
  expect_false(grepl("Times New Roman", pl_sfm$x$diagram, fixed = TRUE))
  expect_true(grepl("fontname=\"eb-garamond\"", pl_sfm$x$diagram, fixed = TRUE))
  expect_identical(attr(pl_sfm, "sdbuildR_webfont"), "eb-garamond")

  # plotly: the layout font is changed
  pl <- set_plot_font(plotly::plot_ly(x = 1:2, y = 1:2, type = "scatter", mode = "lines"), "eb-garamond")
  expect_identical(pl$x$layout$font$family, "eb-garamond")
  expect_identical(attr(pl, "sdbuildR_webfont"), "eb-garamond")

  # System fonts restyle without attaching a webfont
  pl_sys <- set_plot_font(plot(sfm), "Georgia")
  expect_true(grepl("fontname=\"Georgia\"", pl_sys$x$diagram, fixed = TRUE))
  expect_null(attr(pl_sys, "sdbuildR_webfont"))
})


test_that("set_plot_font() reaches fonts set per-element (annotations, sliders, axes)", {
  pl <- plotly::plot_ly(x = 1:2, y = 1:2, type = "scatter", mode = "lines")
  pl <- plotly::layout(pl,
    font = list(family = "Times New Roman"),
    xaxis = list(title = list(text = "x", font = list(family = "Times New Roman"))),
    annotations = list(list(text = "note", font = list(family = "Times New Roman", size = 12))),
    sliders = list(list(
      currentvalue = list(font = list(family = "Times New Roman")),
      steps = list(list(method = "skip", label = "a"))
    ))
  )

  layout <- set_plot_font(pl, "eb-garamond")$x$layout
  expect_identical(layout$font$family, "eb-garamond")
  expect_identical(layout$xaxis$title$font$family, "eb-garamond")
  expect_identical(layout$annotations[[1]]$font$family, "eb-garamond")
  expect_identical(layout$sliders[[1]]$currentvalue$font$family, "eb-garamond")

  # Other font properties are untouched
  expect_identical(layout$annotations[[1]]$font$size, 12)
})


test_that("export_plot() validates font_family", {
  skip_on_cran()

  sfm <- stockflow("sir")
  pl <- plot(sfm)
  file <- tempfile(fileext = ".png")
  expect_error(export_plot(pl, file, font_family = 2), "font_family")
  expect_error(export_plot(pl, file, font_family = c("a", "b")), "font_family")
})


test_that("export_plot() validates close_browser", {
  skip_on_cran()

  sfm <- stockflow("sir")
  pl <- plot(sfm)
  file <- tempfile(fileext = ".png")
  expect_error(export_plot(pl, file, close_browser = "yes"), "close_browser")
  expect_error(export_plot(pl, file, close_browser = NA), "close_browser")
  expect_error(export_plot(pl, file, close_browser = c(TRUE, FALSE)), "close_browser")
})


test_that("inject_svg_webfont() inserts a style element after the svg tag", {
  skip_on_cran()

  svg <- "<svg width=\"10\" height=\"10\"><text>hi</text></svg>"
  out <- inject_svg_webfont(svg, "eb-garamond")

  expect_true(grepl("<svg width=\"10\" height=\"10\">\n<defs><style", out, fixed = TRUE))
  expect_true(grepl("@font-face", out, fixed = TRUE))
  # Only the opening tag is targeted, and system fonts are a no-op
  expect_identical(inject_svg_webfont(svg, "Times New Roman"), svg)
})
