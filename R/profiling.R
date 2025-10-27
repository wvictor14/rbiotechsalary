wait_for_app_to_start <- function(url) {
  httr2::request(url) |>
    httr2::req_retry(
      max_seconds = 5,
      backoff = function(attempt) 2**attempt
    )
}

measure_mem_usage <- function(host = "127.0.0.1") {
  result_file <- tempfile(fileext = "RDS")
  port <- httpuv::randomPort()
  #port <- 3838
  app_process <- callr::r_bg(
    function(result_file, port) {
      on.exit({
        saveRDS(bench::bench_process_memory(), result_file)
      })

      launch_app(port = port, server = function(input, output) {})
      launch_app(port = port)
    },
    args = list(result_file = result_file, port = port)
  )

  on.exit({
    if (app_process$is_alive()) {
      app_process$kill()
    }
  })

  app_url <- paste0("localhost:", port)

  message('app url ', app_url)
  wait_for_app_to_start(app_url)

  utils::browseURL(url = app_url)

  cat("Press [enter] to finish the test...")
  line <- readline()

  app_process$interrupt()

  app_process$wait()

  readRDS(result_file)
}
