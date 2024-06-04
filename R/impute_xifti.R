#' Impute \code{"xifti"} data
#' 
#' Impute masked values using the values of adjacent, non-masked locations.
#' 
#' @inheritParams xifti_Param
#' @param mask A logical vector whose length matches the number of rows in
#'  \code{xifti} object.