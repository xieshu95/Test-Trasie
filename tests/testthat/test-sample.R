test_that("sample when only one event", {
  
  expect_silent(
    sample(x = 1, size = 1, replace = TRUE, prob = c(0.1))
  )
  
  # expect_error(
  #   sample(x = 5, size = 1, replace = TRUE, prob = c(0.1)),
  #   "incorrect number of probabilities"
  # )
  expect_silent(
    DDD::sample2(x = 5, size = 1, replace = TRUE, prob = c(0.1))
  )
  expect_silent(
    DDD::rng_respecting_sample(x = 5, size = 1, replace = TRUE, prob = c(0.1))
  )
  
  set.seed(42)
  a <- DDD::sample2(x = 5, size = 1, replace = TRUE, prob = c(0.1))
  set.seed(42)
  b <- DDD::rng_respecting_sample(x = 5, size = 1, replace = TRUE, prob = c(0.1))
  expect_equal(object = a, expected = b)
}
)
