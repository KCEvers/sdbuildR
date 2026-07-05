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
# R itself never downloads any fonts: a small CSS @font-face rule pointing at
# the hosted woff2 files is attached to the widget, and whatever displays the
# plot fetches them at render time. Without internet, the plot falls back to
# a default font.


#' Default font family for plots and diagrams
#'
#' Reads the sdbuildR.font_family option, so users can set their preferred
#' default once (e.g. in .Rprofile) instead of passing font_family to every
#' plot() call. The shipped default is the "stix-two-text" webfont
#' (STIX Two Text, the serif used by scientific publishers).
#'
#' @returns Character font family name.
#' @noRd
default_font_family <- function() {
  getOption("sdbuildR.font_family", default = "stix-two-text")
}


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


#' URLs of the four woff2 faces for a webfont id
#'
#' Fontsource file paths follow directly from the font id; the URLs are
#' pinned to a major version so the paths cannot change underneath us.
#'
#' The four faces map to plotly/DiagrammeR's regular, <b>, and <i> text.
#'
#' @param font_family Webfont id, e.g. "stix-two-text".
#' @returns Named character vector of URLs, keyed as style_weight.
#' @noRd
webfont_files <- function(font_family) {
  url <- function(weight, style) {
    sprintf(
      "https://cdn.jsdelivr.net/npm/@fontsource/%s@5/files/%s-latin-%s-%s.woff2",
      font_family, font_family, weight, style
    )
  }
  c(
    "normal_400" = url(400, "normal"),
    "normal_700" = url(700, "normal"),
    "italic_400" = url(400, "italic"),
    "italic_700" = url(700, "italic")
  )
}


#' Build the @font-face CSS for a webfont id
#'
#' Each non-regular face lists the regular file as a fallback src: if a font
#' lacks that face (e.g. display fonts without italics), a declared face
#' whose only source fails would otherwise make the browser skip the family
#' entirely for that text (falling back to the default font, so e.g. all
#' <i> text would render in the wrong family). With the fallback src, the
#' browser activates the regular file for that face instead, keeping the
#' text in-family.
#'
#' @param font_family Font family name.
#' @returns A character string of CSS rules, or NULL if `font_family` is not
#'   a webfont id.
#' @noRd
webfont_css <- function(font_family) {
  if (!is_webfont_id(font_family)) {
    return(NULL)
  }

  files <- webfont_files(font_family)
  regular <- files[["normal_400"]]
  rules <- vapply(names(files), function(key) {
    style <- sub("_.*", "", key)
    weight <- sub(".*_", "", key)
    src <- sprintf("url(%s) format('woff2')", files[[key]])
    if (files[[key]] != regular) {
      src <- paste0(src, sprintf(", url(%s) format('woff2')", regular))
    }
    sprintf(
      "@font-face { font-family: '%s'; font-style: %s; font-weight: %s; src: %s; }",
      font_family, style, weight, src
    )
  }, character(1))

  paste(rules, collapse = "\n")
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
