test_that("packaged parcellations support in-memory parcel operations", {
  parc <- load_parc("Schaefer_100")
  expect_true(is.xifti(parc))
  expect_identical(parc$meta$cifti$intent, 3007)
  expect_error(load_parc("not_a_parcellation"))

  labels <- parc$meta$cifti$labels[[1]]
  parcel_values <- as.vector(as.matrix(parc))
  parcel_keys <- labels$Key

  mean_matrix <- ciftiTools:::parc_mean_mat(parc)
  expected_row_sums <- as.numeric(
    tabulate(match(parcel_values, parcel_keys), nbins = length(parcel_keys)) > 0
  )
  expect_equal(unname(rowSums(mean_matrix, na.rm = TRUE)), expected_row_sums)
  expect_equal(
    unname(colSums(!is.na(mean_matrix) & mean_matrix != 0)),
    rep(1, length(parcel_values))
  )

  left_n <- nrow(parc$data$cortex_left)
  right_n <- nrow(parc$data$cortex_right)
  left_values <- seq_len(left_n)
  right_values <- left_n + seq_len(right_n)
  xii <- as.xifti(
    cortexL = cbind(first = left_values, second = 2 * left_values),
    cortexR = cbind(first = right_values, second = 2 * right_values)
  )

  xii_values <- as.matrix(xii)
  expected <- t(vapply(
    parcel_keys,
    function(key) colMeans(xii_values[parcel_values == key, , drop = FALSE]),
    numeric(ncol(xii_values))
  ))
  rownames(expected) <- rownames(labels)

  parcel_means <- apply_parc(xii, parc)
  expect_equal(parcel_means, expected)

  parcel_xii <- apply_parc(xii, parc, return_as = "xifti")
  expect_true(is.xifti(parcel_xii))
  expect_identical(parcel_xii$meta$cifti$intent, 3006)
  expect_equal(
    as.matrix(parcel_xii),
    expected[match(parcel_values, parcel_keys), , drop = FALSE]
  )

  subcort_parc <- as.xifti(
    subcortVol = matrix(c(3, 4), ncol = 1),
    subcortLabs = c(3, 4),
    subcortMask = array(TRUE, dim = c(2, 1, 1))
  )
  subcort_parc <- convert_xifti(
    subcort_parc,
    "dlabel",
    levels_old = c(3, 4),
    levels = c(1, 2),
    labels = c("A", "B"),
    add_white = FALSE
  )
  combined <- parc_add_subcortex(parc, subcort_parc)
  last_cortex_key <- max(labels$Key)
  combined_labels <- combined$meta$cifti$labels[[1]]

  expect_equal(nrow(combined), nrow(parc) + 2)
  expect_equal(
    unname(combined$data$subcort[, 1]),
    last_cortex_key + c(1, 2)
  )
  expect_equal(
    unname(combined_labels$Key[match(c("A", "B"), rownames(combined_labels))]),
    last_cortex_key + c(1, 2)
  )
})

test_that("parcellation borders use the supplied face indices", {
  parcel_values <- c(1, 1, 2, 2, 3, 3)
  faces <- matrix(c(1, 2, 3, 3, 4, 5), ncol = 3, byrow = TRUE)

  expect_identical(
    parc_borders(parcel_values, surf = faces),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    parc_borders(parcel_values, surf = list(faces = faces)),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    parc_borders(rep(1, length(parcel_values)), surf = faces),
    rep(FALSE, length(parcel_values))
  )
})
