test_that("setwd", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::req_get_headers(
      httr2::with_mocked_responses(mock_dir,
                                   wd_setwd(r, "test"))
    )[["X-rdav-wd"]],
    "/test"
  )
})

test_that("setwd root", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::req_get_headers(
      httr2::with_mocked_responses(mock_dir,
                                   wd_setwd(r, "/test"))
    )[["X-rdav-wd"]],
    "/test"
  )
})

test_that("setwd empty", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::req_get_headers(
      httr2::with_mocked_responses(mock_dir,
                                   wd_setwd(r, ""))
    )[["X-rdav-wd"]],
    NULL
  )
})

test_that("getwd", {
  r <- httr2::request("https://cloud.example.com")
  r <- httr2::req_headers(r, "X-rdav-wd" = "/test")
  expect_equal(wd_getwd(r), "/test")
})

test_that("create path", {
  expect_equal(create_path("a/b", "c/d"), "a/b/c/d")
})
