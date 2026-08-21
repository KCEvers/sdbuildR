test_that("use_julia() works", {
  skip_if_julia_not_ready()

  # Stop Julia
  expect_no_error(expect_no_warning(JuliaConnectoR::stopJulia()))

  # This starts Julia
  expect_true(is_julia_working())
  expect_true(is_julia_version_ok())

  # Check if environment was initialized; now this should be false
  expect_false(is_julia_init())

  # Set up environment
  expect_no_error(expect_no_warning(expect_message(use_julia())))

  # But the environment should be installed after calling use_julia()
  expect_true(is_julia_env_setup())
  expect_true(is_julia_init())

  # Close Julia again
  expect_no_error(expect_no_warning(use_julia(stop = TRUE)))

  # Start Julia again
  expect_no_error(expect_no_warning(expect_message(use_julia())))
  expect_true(is_julia_working())
  expect_true(is_julia_env_setup())
  expect_true(is_julia_init())

  # Restart Julia
  expect_no_error(expect_no_warning(expect_message(use_julia(restart = TRUE))))
  expect_true(is_julia_working())
  expect_true(is_julia_env_setup())
  expect_true(is_julia_init())
})

test_that("julia_eval() works", {
  skip_if_julia_not_ready()

  # Stop Julia
  expect_no_error(expect_no_warning(expect_message(use_julia(stop = TRUE))))

  # Calling julia_eval should start new Julia session quietly
  expect_no_message(expect_no_warning(expect_no_error(julia_eval("1 + 1"))))

  # Clean up
  JuliaConnectoR::stopJulia()
})


test_that("use_julia() with threads works", {
  skip_if_julia_not_ready()
  old_threads <- Sys.getenv("JULIA_NUM_THREADS")
  get_threads <- function() as.integer(julia_eval("string(Threads.nthreads())"))

  # Set to 2 threads
  nthreads <- 2
  expect_no_error(use_julia(nthreads = nthreads))

  # Should not change global environment variable
  expect_equal(Sys.getenv("JULIA_NUM_THREADS"), old_threads)
  actual_threads <- get_threads()
  expect_equal(actual_threads, nthreads)
  expect_true(.sdbuildR_env[["jl"]][["use_threads"]])

  # If Julia was already running, it should restart and change the number of threads
  nthreads <- 4
  expect_no_error(use_julia(nthreads = nthreads))
  expect_equal(Sys.getenv("JULIA_NUM_THREADS"), old_threads)
  actual_threads <- get_threads()
  expect_equal(actual_threads, nthreads)
  expect_true(.sdbuildR_env[["jl"]][["use_threads"]])

  # Stopping Julia should reset the number of threads
  expect_no_error(use_julia(stop = TRUE))
  expect_equal(Sys.getenv("JULIA_NUM_THREADS"), old_threads)
  expect_false(.sdbuildR_env[["jl"]][["use_threads"]])

  # This will start Julia again, but should not use threads
  actual_threads <- get_threads()
  if (is.na(old_threads) || old_threads == "") {
    expect_equal(actual_threads, 1)
  } else {
    expect_equal(actual_threads, as.numeric(old_threads))
  }
})


test_that("find_manifest_julia_version() parses the julia_version field", {
  # v2.0 Manifest.toml records the Julia version that resolved the environment
  manifest <- tempfile()
  on.exit(unlink(manifest))
  writeLines(c('julia_version = "1.10.4"', 'manifest_format = "2.0"'), manifest)
  expect_equal(find_manifest_julia_version(manifest), "1.10.4")

  # Older manifests without the field should return NULL (check is skipped)
  no_field <- tempfile()
  on.exit(unlink(no_field), add = TRUE)
  writeLines('manifest_format = "1.0"', no_field)
  expect_null(find_manifest_julia_version(no_field))

  # Missing file should return NULL
  expect_null(find_manifest_julia_version(tempfile()))
})


test_that("julia_version_compatible() compares major and minor versions", {
  # Same major and minor is compatible, regardless of patch
  expect_true(julia_version_compatible("1.10.4", "1.10.9"))
  expect_true(julia_version_compatible("1.10.0", "1.10.0"))

  # Differing minor or major is incompatible
  expect_false(julia_version_compatible("1.10.4", "1.11.0"))
  expect_false(julia_version_compatible("1.10.4", "2.10.4"))

  # Unparseable versions should not block the user
  expect_true(julia_version_compatible("not-a-version", "1.10.4"))
})


test_that("Project.toml hash drift is detected", {
  # The shipped Project.toml should produce a stable, non-NA hash
  h <- project_toml_hash()
  expect_false(is.na(h))
  expect_identical(h, project_toml_hash())

  # Missing marker should not block (other version checks still apply)
  tmp <- tempfile()
  expect_true(is_julia_env_marker_current(tmp, h))

  # Matching hash is current; differing hash is stale
  writeLines(c("sdbuildR_version: 9.9.9", paste0("project_toml_md5: ", h)), tmp)
  on.exit(unlink(tmp))
  expect_true(is_julia_env_marker_current(tmp, h))
  expect_false(is_julia_env_marker_current(tmp, "deadbeef"))
})


test_that("install_julia_env() works", {
  skip_if_julia_not_ready()
  # skip_if(interactive())
  skip_if_no_internet()

  manifest_exists <- function() {
    file.exists(file.path(julia_env_dir(), "Manifest.toml"))
  }
  marker_exists <- function() {
    file.exists(julia_env_marker_file())
  }
  # Any sdbuildR-created file left behind in the environment directory
  env_files_exist <- function() {
    d <- julia_env_dir()
    file.exists(file.path(d, "Manifest.toml")) ||
      file.exists(file.path(d, "Project.toml")) ||
      file.exists(julia_env_marker_file())
  }

  # Test installation: environment, manifest, and provenance marker all created
  expect_no_error(install_julia_env())
  expect_true(manifest_exists())
  expect_true(marker_exists())

  # Removal cleans up *all* created files (no leftovers in R_user_dir)
  expect_no_error(install_julia_env(remove = TRUE))
  expect_false(manifest_exists())
  expect_false(marker_exists())
  expect_false(env_files_exist())
  expect_false(dir.exists(julia_env_dir()))
  expect_false(is_julia_env_setup(error = FALSE))
  expect_false(is_julia_env_setup(error = FALSE, force = TRUE))

  # Removing again should not cause an error, and should say so rather than claiming it
  # removed an environment that was not there.
  expect_message(expect_no_error(install_julia_env(remove = TRUE)), "no need to remove")
  expect_false(env_files_exist())

  # Install again and check that environment is ready
  expect_no_error(install_julia_env())
  expect_true(manifest_exists())
  expect_true(marker_exists())
  expect_true(is_julia_env_setup())
  expect_true(is_julia_env_setup(force = TRUE))
})

test_that("jl_startup_opts() activates the sdbuildR environment without clobbering user options", {
  # Julia is not needed: this is pure string construction.
  withr::with_envvar(c(JULIACONNECTOR_JULIAOPTS = ""), {
    opts <- jl_startup_opts()
    expect_match(opts, "--project=", fixed = TRUE)
    expect_match(opts, jl_path(julia_env_dir()), fixed = TRUE)
    expect_match(opts, "--startup-file=no", fixed = TRUE)
  })

  # Options the user set are kept. Note this deliberately uses an option that does not
  # affect Julia's precompilation cache key: flags like -O or --check-bounds do, and
  # would make Julia recompile the whole environment under a second cache key.
  withr::with_envvar(c(JULIACONNECTOR_JULIAOPTS = "--history-file=no"), {
    opts <- jl_startup_opts()
    expect_match(opts, "--history-file=no", fixed = TRUE)
    expect_match(opts, "--project=", fixed = TRUE)
    expect_match(opts, "--startup-file=no", fixed = TRUE)
  })

  # ... and flags they set themselves are not overridden.
  withr::with_envvar(
    c(JULIACONNECTOR_JULIAOPTS = "--project=@myenv --startup-file=yes"),
    {
      opts <- jl_startup_opts()
      expect_equal(lengths(regmatches(opts, gregexpr("--project=", opts))), 1L)
      expect_match(opts, "--project=@myenv", fixed = TRUE)
      expect_match(opts, "--startup-file=yes", fixed = TRUE)
      expect_false(grepl("--startup-file=no", opts, fixed = TRUE))
    }
  )
})


test_that("use_julia() does not leak JULIACONNECTOR_JULIAOPTS", {
  skip_if_julia_not_ready()

  # Unset: must stay unset.
  withr::with_envvar(c(JULIACONNECTOR_JULIAOPTS = NA), {
    expect_no_error(use_julia(quiet = TRUE))
    expect_equal(Sys.getenv("JULIACONNECTOR_JULIAOPTS"), "")
  })

  # Set by the user: must come back unchanged.
  withr::with_envvar(c(JULIACONNECTOR_JULIAOPTS = "--history-file=no"), {
    expect_no_error(use_julia(quiet = TRUE))
    expect_equal(Sys.getenv("JULIACONNECTOR_JULIAOPTS"), "--history-file=no")
  })

  JuliaConnectoR::stopJulia()
})


test_that("use_julia() starts Julia with the sdbuildR environment already active", {
  skip_if_julia_not_ready()

  use_julia(restart = TRUE, quiet = TRUE)

  # --project is applied at start-up, so the environment is active without
  # run_init_julia_env() having to call Pkg.activate().
  active <- julia_eval("string(something(Base.active_project(), \"\"))")
  expect_equal(normalizePath(active, winslash = "/", mustWork = FALSE),
               norm_path(file.path(julia_env_dir(), "Project.toml")))

  # Tables must resolve to the real package. JuliaConnectoR falls back to a stub
  # (dummy_tables.jl) when `import Tables` fails, which would silently break data
  # transfer, so it has to be a direct dependency of the environment.
  expect_false(grepl("dummy_tables", julia_eval("string(pathof(Tables))"), fixed = TRUE))

  JuliaConnectoR::stopJulia()
})
