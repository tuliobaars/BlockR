# Cache behaviour. Uses the fast "bundled" source, a temporary cache folder,
# a throw-away memory cache and a throw-away "global" environment.

load_quiet <- function(...) {
  msgs <- character(0)
  res <- withCallingHandlers(
    load_blockr_data(...),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(result = res, messages = paste(msgs, collapse = "\n"))
}

test_that("first load builds from the source, then memory, then disk cache", {
  local_blockr_options()
  cache_dir <- getOption("blockr.cache_dir")
  mem <- new.env()

  first <- load_quiet(memory_env = mem, cache_dir = cache_dir)
  expect_match(first$messages, "bundled source \\(fresh")
  expect_true(file.exists(file.path(cache_dir, "blockr_adam.rds")))
  expect_true(exists("data_cache", envir = mem, inherits = FALSE))

  second <- load_quiet(memory_env = mem, cache_dir = cache_dir)
  expect_match(second$messages, "session memory cache")
  expect_identical(second$result$datasets, first$result$datasets)

  third <- load_quiet(memory_env = new.env(), cache_dir = cache_dir)
  expect_match(third$messages, "disk cache")
  expect_identical(third$result$datasets, first$result$datasets)
})

test_that("the default memory cache lives in the package, not the Global Environment", {
  local_blockr_options()
  global_before <- ls(globalenv(), all.names = TRUE)
  load_quiet()
  expect_true(exists("data_cache", envir = the, inherits = FALSE))
  # (.Random.seed may appear: the ADEX pre-processing sets a seed, as in the template)
  expect_identical(setdiff(ls(globalenv(), all.names = TRUE), c(global_before, ".Random.seed")), character(0))
})

test_that("result carries datasets, metadata, source settings and signature", {
  local_blockr_options()
  res <- load_quiet(memory_env = new.env())$result
  expect_named(res, c("datasets", "meta", "source", "signature", "created"), ignore.order = TRUE)
  expect_identical(res$source$source, "bundled")
  expect_identical(res$signature, blockr_data_signature(blockr_source_config("bundled")))
})

test_that("refresh = TRUE rebuilds even with a valid cache", {
  local_blockr_options()
  mem <- new.env()
  load_quiet(memory_env = mem)
  again <- load_quiet(memory_env = mem, refresh = TRUE)
  expect_match(again$messages, "fresh")
})

test_that("use_cache = FALSE neither reads nor writes caches", {
  local_blockr_options()
  cache_dir <- withr::local_tempdir()
  mem <- new.env()
  res <- load_quiet(memory_env = mem, cache_dir = cache_dir, use_cache = FALSE)
  expect_match(res$messages, "fresh")
  expect_false(file.exists(file.path(cache_dir, "blockr_adam.rds")))
  expect_false(exists("data_cache", envir = mem, inherits = FALSE))
})

test_that("a corrupted cache file is ignored and rebuilt", {
  local_blockr_options()
  cache_dir <- getOption("blockr.cache_dir")
  writeLines("not an rds file", file.path(cache_dir, "blockr_adam.rds"))
  res <- load_quiet(memory_env = new.env(), cache_dir = cache_dir)
  expect_match(res$messages, "fresh")
  expect_type(readRDS(file.path(cache_dir, "blockr_adam.rds")), "list")
})

test_that("a stale cache (different signature) is not used", {
  local_blockr_options()
  cache_dir <- getOption("blockr.cache_dir")
  mem <- new.env()
  load_quiet(memory_env = mem, cache_dir = cache_dir)
  stale <- get("data_cache", envir = mem)
  stale$signature <- "old"
  assign("data_cache", stale, envir = mem)
  saveRDS(stale, file.path(cache_dir, "blockr_adam.rds"))
  res <- load_quiet(memory_env = mem, cache_dir = cache_dir)
  expect_match(res$messages, "fresh")
})

test_that("cache_global copies every dataset; blockr_clear_cache removes them", {
  local_blockr_options()
  cache_dir <- getOption("blockr.cache_dir")
  mem <- new.env()
  glob <- new.env()
  res <- load_quiet(memory_env = mem, global_env = glob, cache_dir = cache_dir, cache_global = TRUE)
  expect_match(res$messages, "available in the Global Environment")
  for (ds in names(res$result$datasets)) {
    expect_identical(get(ds, envir = glob), res$result$datasets[[ds]], info = ds)
  }
  blockr_clear_cache(cache_dir = cache_dir, remove_global_datasets = TRUE, global_env = glob, memory_env = mem)
  expect_length(ls(glob, all.names = TRUE), 0)
  expect_length(ls(mem, all.names = TRUE), 0)
  expect_false(file.exists(file.path(cache_dir, "blockr_adam.rds")))
})

test_that("the cache signature tracks code and data-source changes", {
  bundled <- blockr_data_signature(blockr_source_config("bundled"))
  expect_identical(bundled, blockr_data_signature(blockr_source_config("bundled")))

  s1 <- blockr_data_signature(blockr_source_config("synthetic", list(N = 50, seed = 1)))
  s2 <- blockr_data_signature(blockr_source_config("synthetic", list(N = 50, seed = 2)))
  s3 <- blockr_data_signature(blockr_source_config("synthetic", list(N = 60, seed = 1)))
  expect_length(unique(c(bundled, s1, s2, s3)), 4)

  # a change in the data-producing code gives a different fingerprint
  expect_false(identical(
    bundled,
    blockr_data_signature(blockr_source_config("bundled"), code = "changed code")
  ))
})

test_that("the code fingerprint covers the pipeline functions and is stable", {
  expect_identical(blockr_code_fingerprint(), blockr_code_fingerprint())
  expect_type(blockr_code_fingerprint(), "character")
})

test_that("the parquet signature changes when a file is added or modified", {
  dir <- withr::local_tempdir()
  writeLines("a", file.path(dir, "adsl.parquet"))
  cfg <- blockr_source_config("parquet", parquet_dir = dir)
  before <- blockr_data_signature(cfg)
  expect_identical(before, blockr_data_signature(cfg))

  writeLines("b", file.path(dir, "adae.parquet"))
  added <- blockr_data_signature(cfg)
  expect_false(identical(before, added))

  writeLines("longer content", file.path(dir, "adsl.parquet"))
  modified <- blockr_data_signature(cfg)
  expect_false(identical(added, modified))

  # non-ADaM files in the folder do not invalidate the cache
  writeLines("ignored", file.path(dir, "notes.txt"))
  expect_identical(blockr_data_signature(cfg), modified)
})

test_that("synthetic source runs end-to-end through the pipeline", {
  local_blockr_options()
  res <- load_quiet(source = "synthetic", synthetic = list(N = 30, seed = 11), memory_env = new.env())
  expect_match(res$messages, "synthetic source")
  expect_identical(nrow(res$result$datasets$ADSL), 30L)
  expect_identical(res$result$source$synthetic$seed, 11L)
})

test_that("parquet source runs end-to-end through the pipeline", {
  skip_if_not_installed("arrow")
  local_blockr_options()
  dir <- withr::local_tempdir()
  export_adam_parquet(ingest_bundled(), dir)
  res <- load_quiet(source = "parquet", parquet_dir = dir, memory_env = new.env())
  expect_match(res$messages, "parquet source")
  direct <- load_quiet(source = "bundled", memory_env = new.env(), use_cache = FALSE)$result
  for (ds in names(direct$datasets)) {
    expect_equal(as.data.frame(res$result$datasets[[ds]]), as.data.frame(direct$datasets[[ds]]),
                 ignore_attr = TRUE, info = ds)
  }
})
