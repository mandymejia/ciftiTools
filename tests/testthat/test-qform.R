test_that("qform transformations round-trip for each quaternion branch", {
  forms <- list(
    list(qfac = 1, quaternion = c(0, 0, 0)),
    list(qfac = -1, quaternion = c(1, 0, 0)),
    list(qfac = 1, quaternion = c(0, 1, 0)),
    list(qfac = -1, quaternion = c(0, 0, 1))
  )

  for (form in forms) {
    pixdim <- c(form$qfac, 1.5, 2.25, 0.75, rep(0, 4))
    offset <- c(12, -3.5, 8)
    original <- ciftiTools:::Tfmat_from_qform(
      pixdim, form$quaternion, offset
    )
    recovered <- ciftiTools:::qform_from_Tfmat(original)
    rebuilt <- ciftiTools:::Tfmat_from_qform(
      recovered$pixdim, recovered$quatern_bcd, recovered$qoffset_xyz
    )

    expect_equal(rebuilt, original, tolerance = 1e-10)
  }
})

test_that("qform conversion rejects non-orthogonal transforms", {
  invalid <- diag(4)
  invalid[1, 2] <- 1

  expect_error(ciftiTools:::qform_from_Tfmat(invalid), "det\\(R\\)")
})
