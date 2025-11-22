
<!-- README.md is generated from README.Rmd. Please edit that file -->

## sparseNumeric

<!-- badges: start -->

<!-- badges: end -->

The goal of ‘sparseNumeric’ is to provide an S4 class for representing
sparse numeric vectors - that is, vectors that contain many zeros - and
to turn the sparse-vector code from HW5 into a complete, well-documented
R package.

This package stors sparse vectors efficiently by keeping only the
non-zero values and their positions. It supports arithmetic operations
(ex. addition, multiplication, etc.) and dot product directly on the
sparse representation, so the full vector never needs to be rebulit.

In addition to basic arithmetic, the package also includes useful
statistical methods:

- ‘mean()’ - computes the mean of the vector, counding all zeros,
  without converting the vector back to dense form.
- ‘norm()’ - computes the Euclidean norm (the square root of the sum of
  squared values)
- ‘standardize()’ - standardizes the vector by subtracting its mean and
  dividing by its standard deviation.

All methods operate **directly on the sparse structure**, making them
both efficient and memory-friendly. The package includes roxygen2
documentation unit tests, a proper DESCRIPTION file, and Github Actions
to ensure the package passes R CMD check.

Along with these methods, the package includes:

- roxygen2 documentation for the class and all methods  
- a clean DESCRIPTION file with real metadata  
- unit tests written with **testthat**, reaching at least **90%
  coverage**  
- a passing **R CMD check**  
- a GitHub Actions workflow to run checks on every push  
- a `pkgdown` website documenting all functions

## Installation

You can install the development version of sparseNumeric from Github:

``` r
# install.packages("devtools")
devtools::install_github("algodines/HW6_sparce_numeric")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sparseNumeric)

# Create a regular R vector
v <- c(0, 0, 5, 0, 3)

# Convert to sparse_numeric
s <- as(v, "sparse_numeric")

# Display the sparse object
s
#> An object of class 'sparse_numeric'
#>   length:5  nnz:2
#>   first nonzeros (pos:value):
#>    3:5
#>    5:3

# Compute statistics
mean(s)
#> [1] 1.6
norm(s)
#> Note: method with signature 'sparse_numeric#ANY' chosen for function 'norm',
#>  target signature 'sparse_numeric#missing'.
#>  "ANY#missing" would also be valid
#> [1] 5.830952
standardize(s)
#> [1] -0.6949956 -0.6949956  1.4768656 -0.6949956  0.6081211
```

You can also perform arithemtic using the sparse methods:

``` r
x <- as(c(0, 1, 0, 2), "sparse_numeric")
y <- as(c(3, 0, 4, 0), "sparse_numeric")

x + y     # sparse addition
#> An object of class 'sparse_numeric'
#>   length:4  nnz:4
#>   first nonzeros (pos:value):
#>    1:3
#>    2:1
#>    3:4
#>    4:2
x - y     # sparse subtraction
#> An object of class 'sparse_numeric'
#>   length:4  nnz:4
#>   first nonzeros (pos:value):
#>    1:-3
#>    2:1
#>    3:-4
#>    4:2
x * y     # elementwise product
#> An object of class 'sparse_numeric'
#>   length:4  nnz:0
sparse_crossprod(x, y)  # dot product
#> [1] 0
```

Why use README.Rmd? ’`README.Rmd` is helpful because: - you can include
R code chunks that run automatically - output (numbers, tables,plots)
appears directly in your README - your README stays in sync with your
actual code - you can regenerate it at anytime with:

``` r
devtools::build_readme()
```

For example, here is a small built-in plot:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />
