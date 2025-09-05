test_that("merge_columns works", {
  expect_equal(merge_columns(c("a","b","c"),c("d","e","f")),c("ad","be","cf"))
  expect_equal(merge_columns(c("a","b","c"),c("d","e","f"),"!!"),c("a!!d","b!!e","c!!f"))
  expect_equal(merge_columns(c("a","b","c"),c("d","e")),c("ad","be","cd"))
})


test_that("indent works", {
  expect_equal(indent(c("a","b","c"),c(1,2,3)),c(" a","  b","   c"))
})
