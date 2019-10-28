test_that("test two versions of rng_respecting_sample function", {
  rng_respecting_sample2<-function (x, size, replace, prob)
  {
    which_non_zero <- prob > 0
    non_zero_prob <- prob[which_non_zero]
    non_zero_x <- x[which_non_zero]
    testit::assert(length(non_zero_x) == length(non_zero_prob))
    return(DDD::sample2(x = non_zero_x, size = size, replace = replace,
                        prob = non_zero_prob))
  }
  x = list()
  y = list()
  for(i in 1:10){
   set.seed(i)
   x[[i]] <- DDD::rng_respecting_sample(1:10,10,replace = TRUE, prob = c(0,0.1,0,0.1,0,0.1,0,0.1,0,0.1))
   set.seed(i)
   y[[i]] <- rng_respecting_sample2(1:10,10,replace = TRUE, prob = c(0,0.1,0,0.1,0,0.1,0,0.1,0,0.1))
  }
  expect_identical(object = x,expected = y)
})
