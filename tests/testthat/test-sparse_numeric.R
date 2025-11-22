#HW5 Test Script
library(testthat)

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", expect_true(isGeneric("sparse_add")))
test_that("sparse mult generic", expect_true(isGeneric("sparse_mult")))
test_that("sparse sub generic", expect_true(isGeneric("sparse_sub")))
test_that("sparse crossprod generic", expect_true(isGeneric("sparse_crossprod")))

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

## HW 6 Tests

# mean()
test_that("mean works correctly", {
  x_dense <- c(0, 0, 3, 0, 5)
  x <- as(x_dense, "sparse_numeric")
  expect_equal(mean(x), mean(x_dense))
})

# norm()
test_that("norm works correctly", {
  x_dense <- c(0, 3, 4)
  x <- as(x_dense, "sparse_numeric")
  expect_equal(norm(x), sqrt(sum(x_dense^2)))
})

#standardize()
test_that("standardize works correctly", {
  x_dense <- c(0, 1, 2, 3, 4)
  x <- as(x_dense, "sparse_numeric")

  z <- standardize(x)

  expect_length(z, length(x_dense))
  expect_equal(mean(z), 0, tolerance = 1e-8)
  expect_equal(sd(z),   1, tolerance = 1e-8)
})

# standardize error

test_that("standardize errors on constant vector", {
  x <- as(c(1, 1, 1), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("standardize errors on short vectors", {
  x <- as(c(5), "sparse_numeric")
  expect_error(standardize(x))
})


#plot

test_that("plot runs without error", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  y <- as(c(3, 0, 4, 0), "sparse_numeric")

  # open a temporary plotting device
  png(filename = tempfile())

  expect_invisible(plot(x, y))

  dev.off()  # close device
})

###########################
# additonal test to incrase coverage


###########################
# additional tests to increase coverage

test_that("basic sparse_numeric methods work (smoke tests)", {
  x <- new("sparse_numeric",
           value  = c(1, 2, 3),
           pos    = c(1L, 3L, 5L),
           length = 5L)

  y <- new("sparse_numeric",
           value  = c(4, 5),
           pos    = c(2L, 5L),
           length = 5L)

  # Coercion / accessors
  v_x <- as.numeric(x)
  v_y <- as.numeric(y)
  expect_length(v_x, 5)
  expect_length(v_y, 5)

  # Arithmetic
  s  <- x + y
  d  <- x - y
  p  <- x * 2
  q  <- 2 * x

  expect_s4_class(s, "sparse_numeric")
  expect_s4_class(d, "sparse_numeric")
  expect_s4_class(p, "sparse_numeric")
  expect_s4_class(q, "sparse_numeric")

  # Mean / norm / dot product / length method
  m  <- mean(x)
  n  <- norm(x, type = "2")
  dp <- sparse_crossprod(x, y)
  lx <- length(x)

  expect_true(is.numeric(m))
  expect_true(is.numeric(n))
  expect_true(is.numeric(dp))
  expect_identical(lx, 5L)

  # Standardization
  z <- standardize(x)
  expect_true(is.numeric(z))
})

test_that("mismatched lengths throw an error", {
  x <- new("sparse_numeric",
           value  = c(1, 2),
           pos    = c(1L, 2L),
           length = 3L)

  y <- new("sparse_numeric",
           value  = c(3, 4),
           pos    = c(1L, 2L),
           length = 4L)

  expect_error(sparse_add(x, y))  # you could also test x + y here if you prefer
})

#####################
# 68% mark

test_that("plot works and does not error", {
  # create example objects (or reuse ones from another test)
  x <- new("sparse_numeric",
           value  = c(1, 2, 3),
           pos    = c(1L, 3L, 5L),
           length = 5L)

  y <- new("sparse_numeric",
           value  = c(4, 5),
           pos    = c(2L, 5L),
           length = 5L)

  # open a temporary PNG device with decent size
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 800, height = 600)

  # make sure we always clean up
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)

  # this is what we really care about
  expect_silent(plot(x, y))
})


test_that("show method runs and length method works", {
  x <- new("sparse_numeric",
           value  = c(1, 2),
           pos    = c(1L, 2L),
           length = 4L)

  # show() shouldn't error and should print something mentioning the class
  expect_output(show(x), "sparse_numeric")

  # length() should return the full logical length (4L here)
  expect_identical(length(x), 4L)
})


test_that("numeric to sparse_numeric coercion works", {
  z <- as( c(0, 2, 0, 4, 0), "sparse_numeric")
  expect_s4_class(z, "sparse_numeric")
  expect_identical(z@pos, c(2L, 4L))
  expect_identical(z@value, c(2, 4))
})

test_that("length method works", {
  x <- new("sparse_numeric",
           value  = c(1, 2),
           pos    = c(1L, 2L),
           length = 4L)

  expect_identical(length(x), 4L)
})


test_that("standardize errors for short vectors", {
  short <- new("sparse_numeric",
               value = numeric(),
               pos = integer(),
               length = 1L)
  expect_error(standardize(short))
})

test_that("standardize errors for constant vector", {
  const <- as(c(3,3,3,3), "sparse_numeric")
  expect_error(standardize(const))
})

test_that("sparse_mult mismatched lengths throws error", {
  a <- new("sparse_numeric", value = 1, pos = 1L, length = 3L)
  b <- new("sparse_numeric", value = 2, pos = 1L, length = 4L)
  expect_error(sparse_mult(a, b))
})


#######################################
# Test Mark 80%
test_that("numeric to sparse_numeric coercion works", {
  z <- as(c(0, 2, 0, 4), "sparse_numeric")
  expect_s4_class(z, "sparse_numeric")
  expect_identical(z@pos, c(2L, 4L))
  expect_identical(z@value, c(2, 4))
  expect_identical(z@length, 4L)
})

test_that("standardize errors on very short vectors", {
  x <- new("sparse_numeric",
           value = numeric(),
           pos = integer(),
           length = 1L)
  expect_error(standardize(x))
})

test_that("standardize errors when sd=0 (constant vector)", {
  x <- as(c(3, 3, 3, 3), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("sparse_sub errors with mismatched lengths", {
  x <- new("sparse_numeric", value=c(1), pos=1L, length=3L)
  y <- new("sparse_numeric", value=c(2), pos=1L, length=4L)
  expect_error(sparse_sub(x, y))
})

test_that("sparse_mult errors with mismatched lengths", {
  x <- new("sparse_numeric", value=c(1), pos=1L, length=3L)
  y <- new("sparse_numeric", value=c(2), pos=1L, length=4L)
  expect_error(sparse_mult(x, y))
})

test_that("sparse_crossprod errors with mismatched lengths", {
  x <- new("sparse_numeric", value=c(1), pos=1L, length=3L)
  y <- new("sparse_numeric", value=c(2), pos=1L, length=4L)
  expect_error(sparse_crossprod(x, y))
})

test_that("show works for empty sparse vector", {
  x <- new("sparse_numeric",
           value = numeric(),
           pos = integer(),
           length = 5L)
  expect_output(show(x), "nnz:0")
})

########################
# stayed at 80? try more

test_that("numeric to sparse_numeric with all zeros produces empty slots", {
  z <- as(c(0, 0, 0, 0), "sparse_numeric")
  expect_s4_class(z, "sparse_numeric")
  expect_identical(z@pos, integer())
  expect_identical(z@value, numeric())
  expect_identical(z@length, 4L)
})

test_that("plot works with no overlapping nonzeros", {
  x <- new("sparse_numeric",
           value  = c(1, 2),
           pos    = c(1L, 3L),
           length = 5L)

  y <- new("sparse_numeric",
           value  = c(4, 5),
           pos    = c(2L, 4L),  # no overlap with x
           length = 5L)

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 800, height = 600)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)

  expect_silent(plot(x, y))
})

test_that("show truncates output when many nonzeros", {
  x <- new("sparse_numeric",
           value  = as.numeric(1:12),
           pos    = as.integer(1:12),
           length = 12L)

  expect_output(show(x), "\\.\\.\\.")  # look for "..."
})

test_that("validity fails when pos has NA", {
  expect_error(
    new("sparse_numeric",
        value = c(1),
        pos = as.integer(NA),
        length = 3L)
  )
})


test_that("validity fails when pos is out of range", {
  expect_error(
    new("sparse_numeric",
        value = c(1),
        pos = 5L,
        length = 3L)
  )
})


test_that("validity fails when pos not strictly increasing", {
  expect_error(
    new("sparse_numeric",
        value = c(1, 2),
        pos = c(2L, 2L),
        length = 3L)
  )
})

test_that("validity fails when value contains zero", {
  expect_error(
    new("sparse_numeric",
        value = c(0),
        pos = c(1L),
        length = 3L)
  )
})


test_that("validity fails when value is non-finite", {
  expect_error(
    new("sparse_numeric",
        value = c(Inf),
        pos = c(1L),
        length = 3L)
  )
})

##############################################
# 81

test_that("numeric to sparse_numeric with all zeros gives empty slots", {
  z <- as(c(0, 0, 0, 0), "sparse_numeric")
  expect_s4_class(z, "sparse_numeric")
  expect_identical(z@pos, integer())
  expect_identical(z@value, numeric())
  expect_identical(z@length, 4L)
})

test_that("mean of length-0 sparse_numeric is NaN", {
  x0 <- new("sparse_numeric",
            value  = numeric(),
            pos    = integer(),
            length = 0L)
  m0 <- mean(x0)
  expect_true(is.nan(m0))
})

test_that("as( , 'numeric') coercion from sparse_numeric works", {
  x <- new("sparse_numeric",
           value  = c(1, 3),
           pos    = c(1L, 4L),
           length = 5L)

  v1 <- as(x, "numeric")   # uses setAs("sparse_numeric", "numeric")
  v2 <- as.numeric(x)      # uses your as.numeric() method

  expect_identical(v1, v2)
})

test_that("sparse_add drops entries that sum to zero", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(-1, 0, 0), "sparse_numeric")

  z <- sparse_add(x, y)
  expect_identical(z@pos, integer())
  expect_identical(z@value, numeric())
})

test_that("sparse_sub errors with mismatched lengths", {
  x <- new("sparse_numeric", value = c(1), pos = 1L, length = 3L)
  y <- new("sparse_numeric", value = c(2), pos = 1L, length = 4L)
  expect_error(sparse_sub(x, y))
})

test_that("sparse_mult errors with mismatched lengths", {
  x <- new("sparse_numeric", value = c(1), pos = 1L, length = 3L)
  y <- new("sparse_numeric", value = c(2), pos = 1L, length = 4L)
  expect_error(sparse_mult(x, y))
})

test_that("sparse_crossprod errors with mismatched lengths", {
  x <- new("sparse_numeric", value = c(1), pos = 1L, length = 3L)
  y <- new("sparse_numeric", value = c(2), pos = 1L, length = 4L)
  expect_error(sparse_crossprod(x, y))
})

test_that("show truncates output when many nonzeros", {
  x <- new("sparse_numeric",
           value  = as.numeric(1:12),
           pos    = as.integer(1:12),
           length = 12L)

  expect_output(show(x), "\\.\\.\\.")  # looks for "..."
})

test_that("plot works when there is no overlap", {
  x <- new("sparse_numeric",
           value  = c(1, 2),
           pos    = c(1L, 3L),
           length = 5L)

  y <- new("sparse_numeric",
           value  = c(4, 5),
           pos    = c(2L, 4L),  # no overlap with x
           length = 5L)

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 800, height = 600)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)

  expect_silent(plot(x, y))
})

#####################################
# 87 mark

test_that("validity fails when length slot has length > 1", {
  expect_error(
    new("sparse_numeric",
        value  = c(1),
        pos    = c(1L),
        length = c(3L, 4L))
  )
})

test_that("validity fails when length is negative", {
  expect_error(
    new("sparse_numeric",
        value  = c(1),
        pos    = c(1L),
        length = -3L)
  )
})

test_that("validity fails when length is NA", {
  expect_error(
    new("sparse_numeric",
        value  = c(1),
        pos    = c(1L),
        length = as.integer(NA))
  )
})

test_that("validity fails when value and pos lengths differ", {
  expect_error(
    new("sparse_numeric",
        value  = c(1, 2),
        pos    = c(1L),
        length = 3L)
  )
})

test_that("norm of all-zero sparse_numeric is 0", {
  x0 <- new("sparse_numeric",
            value  = numeric(),
            pos    = integer(),
            length = 5L)
  expect_identical(norm(x0, type = "2"), 0)
})

#############
# still not 90 - passes all 90 test on devtools

test_that("sparse_mult and * work for overlapping nonzeros", {
  x <- new("sparse_numeric",
           value  = c(2, 3),
           pos    = c(1L, 3L),
           length = 4L)

  y <- new("sparse_numeric",
           value  = c(5, 7),
           pos    = c(1L, 2L),
           length = 4L)

  # Overlap only at position 1: 2 * 5 = 10
  z1 <- sparse_mult(x, y)
  expect_s4_class(z1, "sparse_numeric")
  expect_identical(z1@pos, 1L)
  expect_identical(z1@value, 10)

  # The * operator for two sparse_numeric vectors should call sparse_mult
  z2 <- x * y
  expect_s4_class(z2, "sparse_numeric")
  expect_identical(z2@pos, z1@pos)
  expect_identical(z2@value, z1@value)
})
