context("Testing the special functions")


# k

expect_identical(k(1:10, 2:11), k(k(1:10, 2:11)))
expect_identical(is.na(k(1:10)), rep(FALSE, 10))
expect_identical(is.na(k(c(1:9, NA))), c(rep(FALSE, 9), TRUE))
expect_identical(is.na(k(1:10, 2:11)), rep(FALSE, 10))
expect_identical(is.na(k(c(1:9, NA), 2:11)), c(rep(FALSE, 9), TRUE))

expect_identical(k(1:10)[1:4], k(1:4))
expect_identical(k(1:10, 2:11)[1:4], k(1:4, 2:5))
