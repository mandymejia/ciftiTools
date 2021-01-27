#' Apply a function to a \code{"xifti"} or pair of \code{"xifti"}s.
#' 
#' Apply a function to a \code{"xifti"} or pair of \code{"xifti"}s.
#' 
#' @param xifti The xifti
#' @param xifti2 The second xifti, if applicable. Otherwise, \code{NULL} (default)
#' @param FUN The function. If \code{xifti2} is not provided, it should be
#'  a univariate function like \code{log} or \code{sqrt}. If 
#'  \code{xifti2} is provided, it should take in two arguments, like \code{`+`}
#'  or \code{pmax}.
#' 