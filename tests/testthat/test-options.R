test_that("package options use defaults and validate assigned values", {
  old_options <- options(
    ciftiTools_EPS = NULL,
    ciftiTools_suppress_msgs = NULL
  )
  on.exit(options(old_options), add = TRUE)

  expect_equal(ciftiTools.getOption("EPS"), 1e-8)
  expect_true(ciftiTools.getOption("suppress_msgs"))

  option_table <- ciftiTools.listOptions()
  expect_identical(
    option_table$OptionName,
    c("wb_path", "EPS", "suppress_msgs")
  )
  expect_equal(nrow(option_table), 3)

  ciftiTools.setOption("EPS", "0.25")
  expect_equal(ciftiTools.getOption("EPS"), 0.25)
  expect_error(ciftiTools.setOption("EPS", 0))
  expect_error(ciftiTools.setOption("EPS", 1))
  expect_error(ciftiTools.setOption("EPS", c(0.1, 0.2)))

  ciftiTools.setOption("suppress_msgs", FALSE)
  expect_false(ciftiTools.getOption("suppress_msgs"))
  expect_error(ciftiTools.setOption("suppress_msgs", c(TRUE, FALSE)))
  expect_error(ciftiTools.setOption("unknown", TRUE))
})
