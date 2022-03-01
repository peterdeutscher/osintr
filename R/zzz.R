.onLoad <- function(libname, pkgname) {

  # can_use_file_keyring()
  set_plot_colours(type = "osintr")
  options(yaml.eval.expr = TRUE)
  # Load fonts
  if (.Platform$OS.type == "windows")  {
    windowsFonts <- grDevices::windowsFonts # nolint
    extrafont::loadfonts("win", quiet = TRUE)
  }

  # NOT CURRENTLY NEEDED
  # # Identify the environment and determine if a tempdir is needed for keyring
  # if (get_env() == "vm") {
  #   mydir <- NULL
  # } else {
  #   mydir <- tempdir()
  # }
  #
  # # Attempt to use the keying on all platforms
  # try(use_file_keyring(dir = mydir),
  #     silent = FALSE)

}
utils::globalVariables(c("windowsFonts"))

.onAttach <- function(libname, pkgname) {
  # Messages about default plot colours changed
  packageStartupMessage("For standard geoms, your ggplot2 default colours have been changed to OSINTR's colours. You might be able to affect non-standard geoms (e.g. geom_text_repel) by running osintr::set_plot_colours('osintr') after librarying their package.")
  packageStartupMessage("See osintr::set_plot_colours() for more information and to see how to return to them to normal.")

  # Check Calibri is installed
  fnt <- extrafont::fonttable()
  if (!any(grepl("Calibri", fnt$FamilyName))) {
    packageStartupMessage("NOTE: Calibri is a required font for this theme to work by default")
    packageStartupMessage("Please use osintr::import_calibri() to install Calibri")
  }
}
