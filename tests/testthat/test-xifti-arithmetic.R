test_that("dscalar xiftis support arithmetic and math operations", {
  dscalar <- convert_xifti(load_parc("Schaefer_100"), "dscalar")
  base_values <- as.matrix(dscalar) + 1
  xii <- newdata_xifti(dscalar, base_values, newnames = "signal")
  xii2 <- newdata_xifti(dscalar, 3 * base_values, newnames = "signal")

  values <- as.matrix(xii)
  values2 <- as.matrix(xii2)

  added <- xii + xii2
  expect_true(is.xifti(added))
  expect_equal(as.matrix(added), values + values2)
  expect_identical(added$meta$cifti$names, xii$meta$cifti$names)
  expect_equal(as.matrix(xii2 - xii), values2 - values)
  expect_equal(as.matrix(xii * 2), values * 2)
  expect_equal(as.matrix(2 * xii), 2 * values)
  expect_equal(as.matrix(sqrt(xii)), sqrt(values))
  expect_equal(sum(xii), sum(values))

  right_only <- as.xifti(cortexR = matrix(1, nrow = 2))
  expect_error(xii + right_only, "brainstructures")

  left_masked_1 <- as.xifti(
    cortexL = matrix(1:4),
    cortexL_mwall = c(1, 1, 0, 0)
  )
  left_masked_2 <- as.xifti(
    cortexL = matrix(1:4),
    cortexL_mwall = c(1, 0, 1, 0)
  )
  expect_error(
    left_masked_1 + left_masked_2,
    "same resolution but different medial wall masks"
  )
})
