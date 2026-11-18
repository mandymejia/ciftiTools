test_that("surface masking supports a result with no faces", {
  surf <- list(
    vertices = matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0),
      ncol = 3, byrow = TRUE
    ),
    faces = matrix(c(1, 2, 3), ncol = 3),
    hemisphere = "left"
  )
  class(surf) <- "surf"

  masked <- mask_surf(surf, c(1, 0, 0))
  expect_equal(masked$vertices, surf$vertices[1, , drop = FALSE])
  expect_equal(dim(masked$faces), c(0L, 3L))
  expect_true(is.surf(masked))
  expect_equal(surf_area(masked), 0)
  expect_length(surf_area(masked, by = "face"), 0)
})

test_that("vertex areas stay associated with their vertex indices", {
  vertices <- rbind(
    c(0, 0, 1),
    c(1, 0, 0),
    c(0, 0, -1),
    c(-0.5, sqrt(3) / 2, 0),
    c(-0.5, -sqrt(3) / 2, 0)
  )
  faces <- matrix(
    c(1, 2, 4, 1, 4, 5, 1, 5, 2, 3, 4, 2, 3, 5, 4, 3, 2, 5),
    ncol = 3, byrow = TRUE
  )
  surf <- list(vertices = vertices, faces = faces, hemisphere = "left")
  class(surf) <- "surf"

  face_areas <- surf_area(surf, by = "face")
  expected <- vapply(seq_len(nrow(vertices)), function(vertex) {
    sum(face_areas[rowSums(faces == vertex) > 0]) / 3
  }, numeric(1))

  expect_equal(surf_area(surf), expected)
})

test_that("surface border extraction ignores depth", {
  surf <- list(
    vertices = matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0),
      ncol = 3, byrow = TRUE
    ),
    faces = matrix(c(1, 2, 3), ncol = 3),
    hemisphere = "left"
  )
  class(surf) <- "surf"
  mask <- c(TRUE, FALSE, FALSE)

  expect_identical(
    edit_mask_surf(mask, surf = surf, do = "borders"),
    edit_mask_surf(mask, surf = surf, do = "borders", depth = Inf)
  )
})

test_that("removing an empty set of substructures is a no-op", {
  xifti <- as.xifti(
    subcortVol = matrix(c(3, 4), ncol = 1),
    subcortLabs = c(3, 4),
    subcortMask = array(TRUE, dim = c(2, 1, 1))
  )

  expect_identical(remove_xifti(xifti, remove_sub = character()), xifti)
})

test_that("metric GIFTIs reject empty data", {
  expect_error(
    ciftiTools:::as.metric_gifti(matrix(numeric(), nrow = 2, ncol = 0)),
    "at least one non-empty measurement"
  )
})

test_that("barycentric remapping does not require area surfaces", {
  xifti <- as.xifti(cortexL = matrix(1:4))
  xifti$meta$cifti$intent <- 3006
  remap_args <- NULL

  local_mocked_bindings(
    write_cifti = function(...) "mock.dscalar.nii",
    separate_cifti_wrapper = function(...) c(cortexL = "original.func.gii"),
    remap_cifti_wrapper = function(...) {
      remap_args <<- list(...)
      c(cortexL = "remapped.func.gii")
    },
    write_cifti_from_separate = function(...) invisible(NULL),
    read_xifti = function(...) xifti
  )

  result <- remap_cifti(
    xifti,
    remap_method = "barycentric",
    mwall_values = NULL,
    verbose = FALSE
  )

  expect_identical(result, xifti)
  expect_identical(remap_args$remap_method, "barycentric")
  expect_null(remap_args$areaL_original_fname)
  expect_null(remap_args$areaR_original_fname)

  expect_error(
    remap_cifti(
      xifti,
      remap_method = "adaptive",
      mwall_values = NULL,
      verbose = FALSE
    ),
    "areaL_original_fname"
  )
})
