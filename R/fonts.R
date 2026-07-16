# Webfont support for plots and diagrams
#
# Plots and stock-and-flow diagrams are htmlwidgets, rendered by a browser
# (and exported through a headless browser), so fonts do not need to be
# installed on anyone's system: they can be loaded as *webfonts*, just like a
# webpage does. A font_family written as an all-lowercase kebab-case
# identifier (e.g. "stix-two-text", "eb-garamond") is treated as a font id
# from the Fontsource project (https://fontsource.org/), whose font files are
# served by the jsDelivr CDN at paths that follow directly from the id --
# there is no font list to maintain in the package. Any other font_family
# (e.g. "Times New Roman") is passed through untouched and resolved as a
# system font.
#
# The package's default font is bundled under
# inst/fonts/ and embedded directly in the widget as a data: URI, so plots
# render in it even without internet. For any other Fontsource id, R itself
# still never downloads the font: a small CSS @font-face rule pointing at the
# hosted woff2 files is attached to the widget, and whatever displays the plot
# fetches them at render time, falling back to a default font when offline.


#' Is this font family a Fontsource webfont id?
#'
#' All-lowercase kebab-case names are treated as Fontsource ids. Generic CSS
#' family keywords (which are also lowercase) are excluded so that e.g.
#' font_family = "serif" keeps its usual CSS meaning.
#'
#' @param font_family Font family name.
#' @returns TRUE or FALSE.
#' @noRd
is_webfont_id <- function(font_family) {
  generic <- c("serif", "sans-serif", "monospace", "cursive", "fantasy", "system-ui")

  is.character(font_family) && length(font_family) == 1 && !is.na(font_family) &&
    grepl("^[a-z0-9]+(-[a-z0-9]+)*$", font_family) &&
    !font_family %in% generic
}


#' The four woff2 faces declared for every webfont
#'
#' Maps to plotly/DiagrammeR's regular, bold, italic and bold-italic text.
#' Each `stem` is the "<weight>-<style>" part of the Fontsource woff2
#' filename (`<id>-latin-<stem>.woff2`).
#'
#' @returns A list of faces, each a list(style, weight, stem).
#' @noRd
webfont_faces <- function() {
  list(
    list(style = "normal", weight = "400", stem = "400-normal"),
    list(style = "normal", weight = "700", stem = "700-normal"),
    list(style = "italic", weight = "400", stem = "400-italic"),
    list(style = "italic", weight = "700", stem = "700-italic")
  )
}


#' Directory of bundled woff2 files for a webfont id, or "" if not bundled
#'
#' The package ships its default font under
#' `inst/fonts/<id>/` so plots render in it without internet access. Any
#' other webfont id is not bundled and is loaded from the Fontsource CDN.
#'
#' @param font_family Webfont id.
#' @returns Absolute path to the font directory, or "" if not bundled.
#' @noRd
webfont_local_dir <- function(font_family) {
  system.file("fonts", font_family, package = "sdbuildR")
}


#' A woff2 file's bytes encoded as a data: URI
#'
#' Embedding the font bytes directly in the `@font-face` `src` lets the
#' browser render the font with no network request, so a bundled font works
#' offline.
#'
#' @param path Path to a woff2 file.
#' @returns A "data:font/woff2;base64,..." string.
#' @noRd
woff2_data_uri <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  # jsonlite line-wraps its base64 output; a data URI must be unbroken
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(raw))
  sprintf("data:font/woff2;base64,%s", b64)
}


#' Build the @font-face CSS for a webfont id
#'
#' For a bundled font (shipped under `inst/fonts/`), the woff2 bytes are
#' embedded as data: URIs so the font renders without internet access. For
#' any other webfont id, the faces are linked to the Fontsource CDN, which
#' the browser fetches at render time (falling back to a default font when
#' offline).
#'
#' Each CDN face also lists the regular file as a fallback src: if a font
#' lacks that face (e.g. display fonts without italics), a declared face
#' whose only source fails would otherwise make the browser skip the family
#' entirely for that text (so e.g. all <i> text would render in the wrong
#' family). With the fallback src, the browser activates the regular file
#' for that face instead. Bundled faces are known to exist, so they need no
#' fallback (and a duplicated data URI would needlessly bloat the CSS).
#'
#' The assembled CSS is cached per font id, since encoding a bundled font is
#' not free and `plot()` may be called many times in a session.
#'
#' @param font_family Font family name.
#' @returns A character string of CSS rules, or NULL if `font_family` is not
#'   a webfont id.
#' @noRd
webfont_css <- function(font_family) {
  if (!is_webfont_id(font_family)) {
    return(NULL)
  }

  cache <- .sdbuildR_env[["webfont_css"]]
  if (!is.null(cache) && !is.null(cache[[font_family]])) {
    return(cache[[font_family]])
  }

  faces <- webfont_faces()
  local_dir <- webfont_local_dir(font_family)

  if (nzchar(local_dir)) {
    # Bundled: embed the font bytes so the plot renders offline
    srcs <- vapply(faces, function(face) {
      path <- file.path(
        local_dir, sprintf("%s-latin-%s.woff2", font_family, face$stem)
      )
      sprintf("url(%s) format('woff2')", woff2_data_uri(path))
    }, character(1))
  } else {
    # Not bundled: link the Fontsource CDN, with the regular as a fallback
    cdn <- function(stem) {
      sprintf(
        "https://cdn.jsdelivr.net/npm/@fontsource/%s@5/files/%s-latin-%s.woff2",
        font_family, font_family, stem
      )
    }
    regular <- cdn("400-normal")
    srcs <- vapply(faces, function(face) {
      url <- cdn(face$stem)
      src <- sprintf("url(%s) format('woff2')", url)
      if (url != regular) {
        src <- paste0(src, sprintf(", url(%s) format('woff2')", regular))
      }
      src
    }, character(1))
  }

  rules <- vapply(seq_along(faces), function(i) {
    face <- faces[[i]]
    sprintf(
      "@font-face { font-family: '%s'; font-style: %s; font-weight: %s; src: %s; }",
      font_family, face$style, face$weight, srcs[[i]]
    )
  }, character(1))

  css <- paste(rules, collapse = "\n")

  if (is.null(cache)) {
    cache <- list()
  }
  cache[[font_family]] <- css
  .sdbuildR_env[["webfont_css"]] <- cache

  css
}


#' Attach a webfont to an htmlwidget (plotly or grViz)
#'
#' If `font_family` is a webfont id, prepend a <style> tag with the font's
#' CSS rules to the widget so browsers (including the headless browser used
#' for exports) can load the font, and record the font in an attribute so
#' export_diagram() knows a browser-based export path is required. Any other
#' `font_family` leaves the widget untouched (it is then resolved as a system
#' font by whatever renders the widget).
#'
#' @param pl An htmlwidget (plotly or grViz object).
#' @param font_family Font family name.
#' @returns The (possibly modified) widget.
#' @noRd
apply_webfont <- function(pl, font_family) {
  css <- webfont_css(font_family)

  if (is.null(css)) {
    return(pl)
  }

  pl <- htmlwidgets::prependContent(pl, htmltools::tags$style(htmltools::HTML(css)))
  attr(pl, "sdbuildR_webfont") <- font_family
  pl
}


#' Inject @font-face CSS into an SVG string
#'
#' Inserts a <style> element right after the opening <svg> tag so browsers
#' viewing the exported SVG load the webfont. Non-browser SVG renderers
#' (e.g. librsvg) ignore @font-face and fall back to system fonts.
#'
#' @param svg SVG document as a single character string.
#' @param font_family Font family name (must be a webfont id).
#' @returns The SVG string with the style element injected.
#' @noRd
inject_svg_webfont <- function(svg, font_family) {
  css <- webfont_css(font_family)

  if (is.null(css)) {
    return(svg)
  }

  style <- sprintf("<defs><style type=\"text/css\"><![CDATA[\n%s\n]]></style></defs>", css)
  sub("(<svg[^>]*>)", paste0("\\1\n", style), svg)
}
