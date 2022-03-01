#### Load colours and palettes ---------------------------------------------------


#' The OSINTR colours
#'
#' These colours are used in \code{osr_plot} and form the basis for our
#' palettes.
#'
#' @export
osr_colours <- c(
  `midnight blue`   = "#051C2C",
  `ocean blue`      = "#002060",
  `azure blue`      = "#00A9F4",
  `sky blue`        = "#0070C0",
  `bondi blue`      = "#3C96B4",
  `celestial blue`  = "#AAE6F0",
  `periwinkle blue` = "#AFC3FF",
  `cobalt blue`     = "#1F40E6",
  `astronaut blue`  = "#004362",
  `bermuda gray`    = "#678ea0",
  `black pearl`     = "#002138",
  `persian green`   = "#00958E",
  `bright sun`      = "#FECC40",
  `morning glory`   = "#A0C5DD",
  `lochmara`        = "#0074bc",
  `mango tango`     = "#dd7400",
  `bermuda`         = "#7adbd4",
  `black`           = "#000000",
  `abbey`           = "#58595b",
  `iron`            = "#d1d3d4",
  `white`           = "#ffffff",
  `tamarillo`       = "#A9111F"
)


#' Extract OSINTR colours as hex codes
#'
#' This function allows you to get the hex code associated with one or more OSINTR
#' colours. It's often used inside \code{osr_plot} to set specific colours for
#' certain series.
#'
#' @param ... The given name/s of a OSINTR colour (eg 'astronaut blue' or 'iron')
#'
#' @examples
#' get_osr_hex("persian green")
#' get_osr_hex("midnight", "mango tango")
#'
#' @export
get_osr_hex <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return(osr_colours)

  osr_colours[cols]
}


#' Predefined OSINTR colours combined into palettes
#'
#' This is a list of OSINTR colours combined into palettes. The palettes are used
#' for different plots and maps.
#' @export
osr_palettes <- list(
  `brand`      = get_osr_hex("midnight blue", "bermuda gray", "black pearl",
                             "persian green", "bright sun"),
  `neutrals`   = get_osr_hex("black", "abbey", "iron"),
  `graph`      = get_osr_hex("midnight blue", "azure blue", "sky blue",
                             "ocean blue", "sky blue", "bondi blue",
                             "cobalt blue", "astronaut blue", "periwinkle blue"),
  `sequential` = get_osr_hex("white", "astronaut blue"),
  `diverging`  = get_osr_hex("bright sun", "white", "persian green")
)


#' Interpolate a OSINTR colour palette
#'
#' This function takes a OSINTR colour palette and generates more colours from it,
#' so that there are enough to make your chart.
#'
#' The interpolation method is set to "spline" (the default is "linear") in an
#' attempt to reduce the number of vomit colours that get produced when
#' generating many colours.
#'
#' @param palette (character; default = \code{"brand"}) given name of a OSINTR
#'   palette: \code{\link{osr_palettes}}
#' @param reverse (boolean; default = \code{FALSE}) indicating if palette should
#'   be reverse
#' @param ... Additional arguments to pass to \code{colorRampPalette} see
#'   details here \code{\link[grDevices]{colorRamp}}
#'
#' @seealso \code{\link{osr_palettes}}
#'
#' @export
make_osr_pal <- function(palette = "graph", reverse = FALSE, ...) {
  pal <- osr_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal,
                              ...,
                              interpolate = "spline")
}



#### Scales for ggplot2 --------------------------------------------------------




#' Scale constructor for OSINTR colours
#'
#' If you get an error about not enough colours, try a different palette (such
#' as \code{graph}) or set \code{discrete = FALSE}.
#'
#' @param palette Character name of palette in \code{osr_palettes}
#' @param discrete Boolean indicating whether colour aesthetic is discrete or
#'   not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to \code{\link{discrete_scale}} or
#'   \code{\link{scale_color_gradientn}}, used respectively when discrete is
#'   TRUE or FALSE
#'
#' @describeIn scale_colour_osr For colour scales
#'
#' @export
scale_colour_osr <-
  function(palette = NULL,
           discrete = FALSE,
           reverse = FALSE,
           ...) {
    if (is.null(palette) & discrete) {
      palette <- "graph"
    }

    if (is.null(palette) & !discrete) {
      palette <- "sequential"
    }

    pal <- make_osr_pal(palette = palette, reverse = reverse)

    if (discrete) {
      ggplot2::discrete_scale("colour", paste0("osr_", palette), palette = pal, ...)
    } else {
      ggplot2::scale_color_gradientn(colours = pal(256), ...)
    }
  }



#' @describeIn scale_colour_osr For fill scales
#'
#' @export
#'
scale_fill_osr <-
  function(palette = NULL,
           discrete = FALSE,
           reverse = FALSE,
           ...) {

    if (is.null(palette) & discrete) {
      palette <- "graph"
    }

    if (is.null(palette) & !discrete) {
      palette <- "sequential"
    }

    pal <- make_osr_pal(palette = palette, reverse = reverse)

    if (discrete) {
      ggplot2::discrete_scale("fill", paste0("osr_", palette), palette = pal, ...)
    } else {
      ggplot2::scale_fill_gradientn(colours = pal(256), ...)
    }
  }



#' The OSINTR plotting options
#'
#' These options set the default ggplot2 colours options for the R session.
#'
osr_plot_opts <- list(
  ggplot2.continuous.colour = scale_colour_osr,
  ggplot2.continuous.fill   = scale_fill_osr,
  ggplot2.discrete.colour   = list(unname(osr_palettes$graph),
                                   make_osr_pal()(16),
                                   make_osr_pal()(24),
                                   make_osr_pal()(32)),
  ggplot2.discrete.fill     = list(unname(osr_palettes$graph),
                                   make_osr_pal()(16),
                                   make_osr_pal()(24),
                                   make_osr_pal()(32))
)


#' The null plotting options
#'
#' These options restore the default ggplot2 colours options for the R session.
#'
null_plot_opts <- list(
  ggplot2.continuous.colour = NULL,
  ggplot2.continuous.fill   = NULL,
  ggplot2.discrete.colour   = NULL,
  ggplot2.discrete.fill     = NULL
)


#' Set \code{ggplot2} default colours
#'
#' Sets global options for \code{ggplot2}. If \code{type = "osr"}, OSINTR colours
#' will be used by default in ggplot. If there are #' more than 32 levels it
#' will switch to \code{viridis} by default, so then you must use
#' \code{scale_colour_osr}. To restore the defaults use \code{type = "default"}.
#'
#' @param type (character) Which colours to use? One of "osr" or "default".
#'
#' @export
#' @examples
#' \dontrun{
#' set_plot_colours(type = "default")
#' }
set_plot_colours <- function(type) {
  if (type == "osr") {
    options(osr_plot_opts)
    set_osr_defaults()
    message("ggplot2 will use OSINTR colours (in the absence of a scale_colour etc function).")
  } else if (type == "default") {

    # Set default multi-colours
    options(null_plot_opts)
    # Set default aesthetics
    geom_defaults %>%
      purrr::iwalk(~set_geom_defaults(geom = .y, new = .x))
    message("ggplot2 will use default colours (in the absence of a scale_colour etc function).")
  } else {
    message("Not a valid type. Nothing changed.")
  }

}


#' Prepare ggplot2 geom_ defaults
#'
#' ggplot2 geoms have default aesthetics, however they can be changed for each
#' session. This function is a generalised way to set geom defaults. It only
#' changes settings where the default is not NA; no need for \code{geom_point}
#' to have a default font family.
#'
#' @param geom (character) the geom to change (e.g. \code{point} or
#'   \code{line})
#' @param aes (character) the aesthetic to change (e.g. \code{colour} or
#'   \code{alpha})
#' @param setting (various) what to set the aesthetic (e.g. a colour or a
#'   number)
#'
#' @return the default aesthetic ready to be set
prep_aes <- function(geom,
                     aes,
                     setting) {
  #Check if the geom inherits from elsewhere
  if (is.null(geom)) {
    return(NULL)
  }

  # Check if the setting exists for this geom
  if (is.null(geom[[aes]])) {
    return(geom)
  }


  # Change the setting only is it's not NA
  if (!is.na(geom[[aes]])) {
    geom[[aes]] <- setting
  }


  return(geom)
}

#' Set default geom aesthetics
#'
#' All arguments are passed to \code{ggplot2::update_geom_defaults}, but this
#' function adds a \code{NULL} check.
#'
#' @param geom (character) the geom to update
#' @param new (various) the aesthetics to update
#'
set_geom_defaults <- function(geom,
                              new) {
  if (is.null(new)) {
    return(NULL)
  } else {
    ggplot2::update_geom_defaults(geom = geom,
                                  new = new)
  }

}



#' Set the default ggplot2 aesthetics to OSINTR's
#'
#' A wrapper for a bunch of other functions to set ggplot2 default aesthetics.
#' Used on load.
set_osr_defaults <- function() {

  check_subclass <- utils::getFromNamespace("check_subclass", "ggplot2")

  defs <- all_geoms %>%
    purrr::map(purrr::safely(check_subclass),
               "Geom",
               env = parent.frame()) %>%
    purrr::map(purrr::pluck,
               "result") %>%
    purrr::map(purrr::pluck,
               "default_aes") %>%
    purrr::set_names(all_geoms)

  # Just font family to Calibri and col/fill to OSINTR blue
  osr_defs <- defs %>%
    purrr::map(prep_aes,
               aes = "family",
               setting = "Calibri") %>%
    purrr::map(prep_aes,
               aes = "colour",
               setting = get_osr_hex("astronaut blue")) %>%
    purrr::map(prep_aes,
               aes = "fill",
               setting = get_osr_hex("astronaut blue"))

  purrr::iwalk(osr_defs,
               ~set_geom_defaults(geom = .y,
                                  new = .x))
}

#' All ggplot geoms
#'
#' A list of ggplot2 geoms for restoring default aesthetics
#'
all_geoms <- list(
  "abline",
  "area",
  "bar",
  # "bin2d",
  "boxplot",
  "col",
  "contour",
  "contour_filled",
  # "count",
  "crossbar",
  "curve",
  "density",
  "density_2d",
  "density_2d_filled",
  "density2d",
  "density2d_filled",
  "dotplot",
  "errorbar",
  "errorbarh",
  # "freqpoly",
  "function",
  "hex",
  # "histogram",
  "hline",
  # "jitter",
  "label",
  "label_repel",
  "line",
  "linerange",
  "map",
  "path",
  "point",
  "pointrange",
  "polygon",
  # "qq",
  # "qq_line",
  "quantile",
  "raster",
  "rect",
  "ribbon",
  "rug",
  "segment",
  "sf",
  # "sf_label",
  # "sf_text",
  "smooth",
  "spoke",
  "step",
  "text",
  "text_repel",
  "tile",
  "violin",
  "vline"
)


#### Set base theme -------------------------------------------------------------

#' OSINTR-styled ggplot2 theme
#'
#' A ggplot2 theme based on \code{theme_minimal} using Calibri, OSINTR colours and
#' customised element positioning.
#'
#' ggplot2 themes don't affect \code{geom_} colours, they are addressed
#' elsewhere.
#'
#' @param ... arguments pass to \code{theme_minimal}
#'
#' @export
theme_osr <- function(...) {
  ggplot2::theme_minimal(...) %+replace%
    ggplot2::theme(
      line = ggplot2::element_line(colour = get_osr_hex("iron")),
      text = ggplot2::element_text(colour = get_osr_hex("abbey"),
                                   family = "Calibri"),
      axis.title.y = ggplot2::element_text(angle = 90,
                                           margin = ggplot2::margin(0, 20, 0, 5)),
      axis.title.y.right = ggplot2::element_text(angle = 90,
                                                 margin = ggplot2::margin(0, 5, 0, 20)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(15, 15, 10, 15)
    )
}

#' OSINTR-styled ggplot2 wrapper
#'
#' If you get an error about not enough colours, try a different palette (such
#' as graph) or set \code{discrete = FALSE} in \code{scale_colour_osr} or
#' \code{scale_fill_osr}
#'
#' @param ... Standard ggplot arguments
#'
#' @seealso \code{\link{scale_colour_osr}}
#' @seealso \code{\link{scale_fill_osr}}
#' @export
#'
osr_plot <- function(...) {


  ggplot2::ggplot(...) +
    # Set theme
    theme_osr()

}

#' OSINTR-styled ggplot2 theme for maps
#'
#' A ggplot2 theme based on \code{theme_bw} using Calibri, OSINTR colours and
#' customised element positioning.
#'
#' ggplot2 themes don't affect \code{geom_} colours, they are addressed
#' elsewhere.
#'
#' @param ... arguments pass to \code{theme_bw}
#' @export
#'
theme_osr_map <- function(...) {
  ggplot2::theme_bw(...) %+replace%
    ggplot2::theme(
      axis.line             = ggplot2::element_blank(),
      axis.text             = ggplot2::element_blank(),
      axis.ticks            = ggplot2::element_blank(),
      axis.title            = ggplot2::element_blank(),
      legend.justification  = c(0, 0),
      legend.key.size       = grid::unit(0.6, "lines"),
      legend.position       = c(0, 0),
      panel.background      = ggplot2::element_blank(),
      panel.border          = ggplot2::element_blank(),
      panel.grid            = ggplot2::element_blank(),
      panel.spacing         = grid::unit(0, "lines"),
      plot.background       = ggplot2::element_blank(),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      text                  = ggplot2::element_text(
        colour = get_osr_hex("abbey"),
        family = "Calibri")
    )
}


#' OSINTR-styled ggmap wrapper
#'
#' If you get an error about not enough colours, try a different palette (such
#' as graph) or set \code{discrete = FALSE} in \code{scale_colour_osr} or
#' \code{scale_fill_osr}
#'
#'
#' @param ... Standard ggmap arguments
#' @param crs Numeric CRS for \code{ggplot2::coord_sf} (default = 7844, GDA2020
#'   degrees). See \url{https://epsg.io/7844} for others (7845 is for meters)
#' @export
#'
osr_gmap <- function(...,
                     crs = 7844) {

  function_needs("ggmap")
  # Based heavily on ggthemes:theme_map
  ggmap::ggmap(...) +
    # Set CRS
    ggplot2::coord_sf(crs = sf::st_crs(crs)) +
    theme_osr_map()
}


#' Save images with opinions
#'
#' Wrapper for \code{ggsave} using golden ratio, png and cairo and other
#' opinions. Using type = 'cairo' fixes some anti-aliasing problems.
#'
#' All arguments passed to \code{ggsave()}
#'
#' @param filename (character) file name to create on disk.
#' @param plot The plot to save
#' @param dpi (numeric; default = 320) dpi to use
#' @param scale (numeric; default = 1.5) 	multiplicative scaling factor
#' @param height (numeric; default = 11.25) image height
#' @param width (numeric; default = 18) image width
#' @param device (character; default = 'png') graphics device
#' @param type (character; default = 'cairo') type
#' @param units (character; default = 'cm') units
#' @param bg (character; default = "white") plot background colour, needed
#'   because of update to \code{ggsave}
#' @param ... Standard \code{ggsave} arguments

#'
#' @export
osr_save <- function(filename,
                     plot = ggplot2::last_plot(),
                     dpi = 320,
                     scale = 1.5,
                     height = 11.25,
                     width = 18,
                     device = "png",
                     type = "cairo",
                     units = "cm",
                     bg = "white",
                     ...) {

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    dpi = dpi,
    scale = scale,
    height = height,
    width = width,
    device = device,
    type = type,
    units = units,
    bg = bg,
    ...
  )
}

#' Save a map made with osr_gmap
#'
#' Very similar to \code{osr_save}, but this will extract the aspect ratio of
#' the map passed to it and edit the image dimensions to accommodate that ratio.
#'
#' @param filename (character) file name to create on disk.
#' @param plot The plot to save
#' @param dpi (numeric; default = 320) dpi to use
#' @param scale (numeric; default = 1.5) 	multiplicative scaling factor
#' @param height (numeric; default = 11.25) image height
#' @param width (numeric; default = 18) image width
#' @param device (character; default = 'png') graphics device
#' @param type (character; default = 'cairo') type
#' @param units (character; default = 'cm') units
#' @param ... Standard ggsave arguments
#'
#' @export
#'
osr_map_save <- function(filename,
                         plot = ggplot2::last_plot(),
                         dpi = 320,
                         scale = 1.5,
                         height = 11.25,
                         width = 18,
                         device = "png",
                         type = "cairo",
                         units = "cm",
                         ...) {

  # Get the aspect ratio
  asp <- extract_map_aspect_ratio(plot)

  if (asp >= 1) {
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      dpi = dpi,
      scale = scale,
      height = height,
      width = height / asp,
      device = device,
      type = type,
      units = units,
      ...
    )
  }

  if (asp < 1) {
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      dpi = dpi,
      scale = scale,
      height = width * asp,
      width = width,
      device = device,
      type = type,
      units = units,
      ...
    )
  }

}


#' Filter an inherited ggplot dataset to return the maximum of the specified
#' column (optionally group_by before filtering).
#'
#' The use case of \code{take_max()} is within a ggplot layer where you want to
#' highlight the final data point on a curve with \code{geom_point(data =
#' take_max(date, country))} or \code{geom_label_repel(data = take_max(date,
#' country))}. The effect is to group and then filter the dataset inherited from
#' the previous ggplot layer down to a single row per group, so you can first
#' plot a curve with \code{geom_line()} and then place a point at the end of the
#' curve with \code{geom_point()}. If no group_by variable is provided, a single
#' row tibble is returned.
#'
#' @details At a technical level \code{take_max()} is a nested function. It
#' returns a unary function (i.e. a function with only one parameter) that
#' filters the inherited dataset in a ggplot2 layer. Refer to the data section
#' of the \code{geom_point} help documentation -- the third option is "A
#' function will be called with a single argument, the plot data. The return
#' value must be a data.frame, and will be used as the layer data." At a
#' super-technical level -- under the hood this function uses tidy-evaluation
#' (tidyverse version of non-standard evaluation). Specifically, it uses double
#' curly braces \code{{{col_to_max}}}, which is an abstraction of
#' !!enquo(col_to_max) - further details at
#' \href{https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/}{rlang blog post}
#'
#' @param col_to_max (name_of_column to take max - unquoted) Column you want the
#'   maximum value of (e.g. days_since). Don't use quotation marks around column
#'   name.
#' @param group_by (name_of_column to group_by - unquoted) Column you want to
#'   group_by (e.g. country). Don't use quotation marks around column name.
#'
#' @return A unary function to be applied to inherited dataset in a ggplot2
#'   layer.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' country_1 <- tibble::tibble(received_date = seq(from = 0,
#'                                                 to = 10 * pi,
#'                                                 length.out = 1000),
#'                             y_data = sin(received_date),
#'                             country = "Australia")
#'
#' country_2 <- tibble::tibble(received_date = seq(from = 0,
#'                                                 to = 10 * pi,
#'                                                 length.out = 1000),
#'                             y_data = 0.125 * cos(received_date) - 0.25,
#'                             country = "United Kingdom")
#'
#'chart_data <- dplyr::bind_rows(country_1, country_2)
#'
#'ggplot2::ggplot(data = chart_data,
#'                mapping = ggplot2::aes(x = received_date,
#'                                       y = y_data,
#'                                       col = country,
#'                                       label = country)) +
#'  ggplot2::theme(legend.position = "none") +
#'  ggplot2::geom_line() +
#'  ggplot2::geom_point(data = osintr::take_max(received_date, country)) +
#'  ggrepel::geom_label_repel(data = osintr::take_max(received_date, country))
#'
#' }
#'
take_max <- function(col_to_max, group_by) {
  function(z) {
    dplyr::slice_max(.data = dplyr::group_by(z, {{group_by}}),
                     order_by = {{col_to_max}},
                     n = 1,
                     with_ties = FALSE)
  }
}
