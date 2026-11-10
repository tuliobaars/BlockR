# Launch BlockR from the project folder (RStudio "Run App", Posit Connect, ...).
# With the package installed you can simply call BlockR::run_app().
#
# Settings (see ?blockr_options), e.g.:
#   options(blockr.data_source = "synthetic", blockr.synthetic = list(N = 200, seed = 7))
#   options(blockr.data_source = "parquet", blockr.parquet_dir = "path/to/adam")
#   options(blockr.color_mode = "dark")   # default colour mode ("light"; "auto" follows the OS)
#   options(blockr.style = FALSE)         # plain teal look of the source templates

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
BlockR::run_app()
