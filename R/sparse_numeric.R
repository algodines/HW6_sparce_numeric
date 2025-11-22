## IMPORTED HW5 Class/Methods

#' Sparse numeric vector
#'
#' S4 class representing a sparse numeric vector.
#'
#' @slot value Numeric vector of non-zero entries.
#' @slot pos Integer vector of 1-based positions of non-zero entries.
#' @slot length Integer scalar giving the full length of the dense vector.
#'
#' @importFrom methods new
#' @importFrom graphics abline
#' @exportClass sparse_numeric
setClass(
  Class = "sparse_numeric",
  slots = c(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  )
)

# Validity Test
setValidity("sparse_numeric", function(object) {
  errs <- character()

  if (length(object@length) != 1L)
    errs <- c(errs, "'length' must be a single integer scalar.")
  if (!is.numeric(object@value))
    errs <- c(errs, "'value' must be numeric.")
  if (!is.integer(object@pos))
    errs <- c(errs, "'pos' must be integer.")
  if (length(object@value) != length(object@pos))
    errs <- c(errs, "'value' and 'pos' must have the same length.")

  if (length(object@length) == 1L) {
    if (is.na(object@length) || object@length < 0L)
      errs <- c(errs, "'length' must be a non-negative integer.")
  }

  n <- if (length(object@length) == 1L) as.integer(object@length) else NA_integer_

  # positions
  if (length(object@pos)) {
    if (any(is.na(object@pos)))
      errs <- c(errs, "'pos' cannot contain NA.")
    if (!is.na(n) && (any(object@pos < 1L) || any(object@pos > n)))
      errs <- c(errs, "'pos' must be within [1, length].")
    if (!isTRUE(all(diff(object@pos) > 0L)))
      errs <- c(errs, "'pos' must be strictly increasing (sorted, unique).")
  }

  # values
  if (length(object@value)) {
    if (any(!is.finite(object@value)))
      errs <- c(errs, "'value' must be finite (no NA/Inf).")
    if (any(object@value == 0))
      errs <- c(errs, "'value' must not contain zeros; omit zero entries.")
  }

  if (length(errs)) errs else TRUE
})

# internal helper: check same length
.check_same_len <- function(x, y) {
  if (x@length != y@length)
    stop("Operands must have the same 'length'.")
}

# merge add/sub
.merge_sum <- function(px, vx, py, vy, subtract = FALSE) {
  i <- 1L; j <- 1L
  nx <- length(px); ny <- length(py)
  out_pos <- integer(0L); out_val <- numeric(0L)

  while (i <= nx || j <= ny) {
    if (i <= nx && (j > ny || px[i] < py[j])) {
      pos <- px[i]; val <- vx[i]
      i <- i + 1L
    } else if (j <= ny && (i > nx || py[j] < px[i])) {
      pos <- py[j]; val <- if (subtract) -vy[j] else vy[j]
      j <- j + 1L
    } else { ## px[i] == py[j]
      pos <- px[i]
      val <- vx[i] + if (subtract) -vy[j] else vy[j]
      i <- i + 1L; j <- j + 1L
    }
    if (val != 0) { # keep nonzeros
      out_pos <- c(out_pos, pos)
      out_val <- c(out_val, val)
    }
  }

  list(pos = out_pos, val = out_val)
}

# product on overlap
.merge_prod <- function(px, vx, py, vy) {
  i <- 1L; j <- 1L
  nx <- length(px); ny <- length(py)
  out_pos <- integer(0L); out_val <- numeric(0L)

  while (i <= nx && j <= ny) {
    if (px[i] == py[j]) {
      val <- vx[i] * vy[j]
      if (val != 0) {
        out_pos <- c(out_pos, px[i])
        out_val <- c(out_val, val)
      }
      i <- i + 1L; j <- j + 1L
    } else if (px[i] < py[j]) {
      i <- i + 1L
    } else {
      j <- j + 1L
    }
  }
  list(pos = out_pos, val = out_val)
}

# dot product on overlap
.merge_dot <- function(px, vx, py, vy) {
  i <- 1L; j <- 1L
  nx <- length(px); ny <- length(py)
  s <- 0.0
  while (i <= nx && j <= ny) {
    if (px[i] == py[j]) {
      s <- s + vx[i] * vy[j]
      i <- i + 1L; j <- j + 1L
    } else if (px[i] < py[j]) {
      i <- i + 1L
    } else {
      j <- j + 1L
    }
  }
  s
}

.make_sparse <- function(pos, val, n) {
  if (length(pos)) {
    o <- order(pos)
    pos <- as.integer(pos[o])
    val <- val[o]
  } else {
    pos <- integer()
    val <- numeric()
  }
  new("sparse_numeric", value = val, pos = pos, length = as.integer(n))
}

# Coercions ---------------------------------------------------------------

# to sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  if (length(nz)) {
    .make_sparse(pos = nz, val = from[nz], n = length(from))
  } else {
    new("sparse_numeric",
        value  = numeric(),
        pos    = integer(),
        length = as.integer(length(from)))
  }
})

# to numeric
setAs("sparse_numeric", "numeric", function(from) {
  n <- as.integer(from@length)
  out <- numeric(n)
  if (length(from@pos))
    out[from@pos] <- from@value
  out
})

# Generics -----------------------------------------------------------------

#' Add two sparse_numeric vectors
#'
#' @param x,y Objects of class \code{sparse_numeric} with the same length.
#' @param ... Additional arguments (ignored).
#' @return A \code{sparse_numeric} vector.
#' @export
setGeneric("sparse_add",  function(x, y, ...) standardGeneric("sparse_add"))

#' Subtract two sparse_numeric vectors
#'
#' @inheritParams sparse_add
#' @param ... Additional arguments (ignored).
#' @return A \code{sparse_numeric} vector.
#' @export
setGeneric("sparse_sub",  function(x, y, ...) standardGeneric("sparse_sub"))

#' Elementwise product of two sparse_numeric vectors
#'
#' @inheritParams sparse_add
#' @param ... Additional arguments (ignored).
#' @return A \code{sparse_numeric} vector.
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' Sparse crossproduct (dot product)
#'
#' @inheritParams sparse_add
#' @param ... Additional arguments (ignored).
#' @return Numeric scalar giving the dot product.
#' @export
setGeneric("sparse_crossprod",
           function(x, y, ...) standardGeneric("sparse_crossprod"))

# Methods for sparse_* ----------------------------------------------------

#' Methods for sparse_numeric arithmetic generics
#'
#' These methods implement the actual arithmetic operations for
#' \code{sparse_numeric} vectors, corresponding to the exported generics
#' \code{sparse_add}, \code{sparse_sub}, \code{sparse_mult}, and
#' \code{sparse_crossprod}.
#'
#' @name sparse_numeric-arith-methods
#' @aliases
#'   sparse_add,sparse_numeric,sparse_numeric-method
#'   sparse_sub,sparse_numeric,sparse_numeric-method
#'   sparse_mult,sparse_numeric,sparse_numeric-method
#'   sparse_crossprod,sparse_numeric,sparse_numeric-method
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector.
#' @param ... Additional arguments (ignored).
#'
#' @return A \code{sparse_numeric} vector, except for \code{sparse_crossprod},
#'   which returns a numeric scalar.
NULL


#' @rdname sparse_numeric-arith-methods
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            m <- .merge_sum(x@pos, x@value, y@pos, y@value, subtract = FALSE)
            .make_sparse(m$pos, m$val, x@length)
          })

#' @rdname sparse_numeric-arith-methods
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            m <- .merge_sum(x@pos, x@value, y@pos, y@value, subtract = TRUE)
            .make_sparse(m$pos, m$val, x@length)
          })

#' @rdname sparse_numeric-arith-methods
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            m <- .merge_prod(x@pos, x@value, y@pos, y@value)
            .make_sparse(m$pos, m$val, x@length)
          })

#' @rdname sparse_numeric-arith-methods
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            .merge_dot(x@pos, x@value, y@pos, y@value)
          })

# Operator methods (+, -, *) ---------------------------------------------

#' Arithmetic operations for sparse_numeric vectors
#'
#' Implements elementwise arithmetic for two \code{sparse_numeric}
#' vectors of the same length.
#'
#' @name sparse_numeric-arith
#' @aliases +,sparse_numeric,sparse_numeric-method
#' @aliases -,sparse_numeric,sparse_numeric-method
#' @aliases *,sparse_numeric,sparse_numeric-method
#'
#' @param e1 A \code{sparse_numeric} vector.
#' @param e2 A \code{sparse_numeric} vector.
#' @return A \code{sparse_numeric} vector.
NULL

#' @rdname sparse_numeric-arith
setMethod("+",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_numeric-arith
setMethod("-",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_numeric-arith
setMethod("*",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

# show() method -----------------------------------------------------------

setMethod("show", "sparse_numeric", function(object) {
  nnz <- length(object@pos)
  cat("An object of class 'sparse_numeric'\n")
  cat("  length:", as.integer(object@length), "  nnz:", nnz, "\n", sep = "")
  if (nnz) {
    k <- min(nnz, 10L)
    cat("  first nonzeros (pos:value):\n")
    for (i in seq_len(k)) {
      cat("   ", object@pos[i], ":", format(object@value[i]), "\n", sep = "")
    }
    if (nnz > k) cat("   ...\n")
  }
})

# plot(x, y) --------------------------------------------------------------

setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            .check_same_len(x, y)
            overlap <- intersect(x@pos, y@pos)
            n <- as.integer(x@length)
            if (length(overlap) == 0L) {
              plot(NA,
                   xlim = c(1, n), ylim = c(0, 1),
                   xlab = "index", ylab = "overlap",
                   main = "No overlapping nonzero elements")
              return(invisible(NULL))
            }
            plot(overlap, rep(1, length(overlap)),
                 xlim = c(1, n), ylim = c(0, 1.1),
                 xlab = "index", ylab = "overlap (y=1)",
                 main = "Overlapping nonzero indices")
            abline(h = 1, lty = 3)
            invisible(NULL)
          })

# Length method -----------------------------------------------------------

#' Length of a sparse_numeric vector
#'
#' Returns the full logical length of the underlying dense vector
#' represented by a \code{sparse_numeric} object.
#'
#' @name sparse_numeric-length
#' @aliases length,sparse_numeric-method
#' @param x A \code{sparse_numeric} vector.
#' @return An integer scalar giving the length.
NULL

#' @rdname sparse_numeric-length
setMethod("length", "sparse_numeric",
          function(x) as.integer(x@length))

## HW6 Methods ------------------------------------------------------------

#' Mean of a sparse_numeric vector
#'
#' Computes arithmetic mean of \code{sparse_numeric} vector,
#' including implicit zeros.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param ... Additional arguments (ignored).
#' @return A numeric scalar giving mean.
#' @export
setMethod("mean",
          signature(x = "sparse_numeric"),
          function(x, ...) {
            n <- as.integer(x@length)
            if (n == 0L) {
              return(NaN)
            }
            sum(x@value) / n
          })

#' Euclidean norm of a sparse_numeric vector
#'
#' Computes Euclidean (L2) norm \eqn{sqrt(sum(x^2))} of a
#' \code{sparse_numeric} vector.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param type Ignored; included for compatibility with \code{\link[base]{norm}}.
#' @param ... Additional arguments (ignored).
#' @return Numeric scalar giving Euclidean norm.
#' @export
setGeneric("norm", function(x, type, ...) standardGeneric("norm"))

#' @rdname norm
setMethod("norm",
          signature(x = "sparse_numeric"),
          function(x, type, ...) {
            sqrt(sum(x@value^2))
          })

#' Standardize sparse_numeric vector
#'
#' Centers and scales a \code{sparse_numeric} vector by subtracting
#' the mean and dividing by the sample standard deviation (like base
#' \code{scale()}), computed including implicit zeros.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param ... Additional arguments (ignored).
#' @return Numeric vector of the same length as \code{x} with
#'   standardized values (mean 0, sd 1).
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
setMethod("standardize",
          signature(x = "sparse_numeric"),
          function(x, ...) {
            n <- as.integer(x@length)
            if (n <= 1L)
              stop("standardize() requires vector length >= 2.")

            sum_val  <- sum(x@value)
            mu       <- sum_val / n
            sumsq    <- sum(x@value^2)
            ss_total <- sumsq - n * mu^2

            # numeric guard
            if (ss_total < 0 && abs(ss_total) < 1e-12)
              ss_total <- 0

            if (ss_total <= 0)
              stop("cannot standardize a constant vector (sd = 0).")

            sd_val <- sqrt(ss_total / (n - 1L))

            # build standardized dense vector w/o materializing original
            z <- rep((-mu) / sd_val, n)
            if (length(x@pos))
              z[x@pos] <- (x@value - mu) / sd_val

            z
          })
###########################
#Additional coverage code

#' @export
setMethod("as.numeric", "sparse_numeric", function(x) {
  out <- numeric(x@length)
  out[x@pos] <- x@value
  out
})

#' @export
setMethod("*", signature(e1 = "sparse_numeric", e2 = "numeric"),
          function(e1, e2) {
            new("sparse_numeric",
                value  = e1@value * e2,
                pos    = e1@pos,
                length = e1@length)
          })
#' @export
setMethod("*", signature(e1 = "numeric", e2 = "sparse_numeric"),
          function(e1, e2) {
            new("sparse_numeric",
                value  = e2@value * e1,
                pos    = e2@pos,
                length = e2@length)
          })

setMethod("+",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))
