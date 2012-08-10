# TODO: right credits to devtools (source: devtools/R/install.R)

install <- function(pkg = NULL, reload = TRUE, quick = FALSE, args = NULL) {
  pkg <- as.package(pkg)
  message("Installing ", pkg$package)
  install_deps(pkg)  
  
  #built_path <- build(pkg, tempdir())
  #on.exit(unlink(built_path))    
  
  opts <- c(
    paste("--library=", shQuote(.libPaths()[1]), sep = ""),
    "--with-keep.source")
  if (quick) {
    opts <- c(opts, "--no-docs", "--no-multiarch", "--no-demo")
  }
  opts <- paste(paste(opts, collapse = " "), paste(args, collapse = " "))
  
  R(paste("CMD INSTALL ", shQuote(built_path), " ", opts, sep = ""))
  
  if (reload) reload(pkg)
  invisible(TRUE)
}
