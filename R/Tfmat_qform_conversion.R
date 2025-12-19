#' Derive transformation matrix from \code{qform} metadata in NIFTI header
#'
#' Sources: afni.nimh.nih.gov/pub/dist/doc/nifti/nifti1_h.pdf,
#'  brainder.org/2012/09/23/the-nifti-file-format/,
#'  nipy.org/nibabel/nifti_images.html,
#'  brainvoyager.com/bv/doc/UsersGuide/CoordsAndTransforms/CoordinateSystems.html
#'
#' @param pixdim Numeric vector of length \code{8}. Only the first four entries
#'  are used.
#' @param quatern_bcd Numeric vector of length \code{3}, giving quaterns
#'  b, c, and d.
#' @param qoffset_xyz Numeric vector of length \code{3}, giving qoffsets
#'  x, y, and z.
#' @return 4 by 4 transformation matrix
#'
#' @keywords internal
Tfmat_from_qform <- function(pixdim, quatern_bcd, qoffset_xyz){
  b <- quatern_bcd[1]
  c <- quatern_bcd[2]
  d <- quatern_bcd[3]

  # Missing quaternion component
  a <- sqrt(max(0, 1-(b^2+c^2+d^2)))

  # qfac
  qfac <- ifelse(pixdim[1] >=0, 1, -1)

  # rotation
  R <- rbind(
    c(a^2+b^2-c^2-d^2, 2*(b*c-a*d),     2*(b*d+a*c)     ),
    c(2*(b*c+a*d),     a^2+c^2-b^2-d^2, 2*(c*d-a*b)     ),
    c(2*(b*d-a*c),     2*(c*d+a*b),     a^2+d^2-b^2-c^2 )
  )

  # rotation + scale
  M <- R %*% diag(c(pixdim[seq(2,4)]))
  M[,3] <- M[,3] * qfac

  # rotation + scale + translation
  M <- cbind(M, as.matrix(qoffset_xyz))
  A <- rbind(M, c(0,0,0,1))
}

#' Derive \code{qform} metadata in NIFTI header from transformation matrix
#'
#' Sources: afni.nimh.nih.gov/pub/dist/doc/nifti/nifti1_h.pdf,
#'  brainder.org/2012/09/23/the-nifti-file-format/,
#'  nipy.org/nibabel/nifti_images.html,
#'  brainvoyager.com/bv/doc/UsersGuide/CoordsAndTransforms/CoordinateSystems.html
#'
#' @param A The transformation matrix
#' @return See \code{Tfmat_from_qform} arguments
#'
#' @keywords internal
qform_from_Tfmat <- function(A) {
  # M: rotation + scale
  M <- A[seq(3), seq(3)]

  # Get scale: L2 norms and signs of the columns of M.
  pixdim_234 <- apply(M, 2, function(v){
    sqrt(sum(v^2)) * sign(v[which.max(abs(v))])
  })

  # Get pure rotation matrix. Make det(R) == +1.
  R <- M %*% diag(1/pixdim_234)
  # ## Optional: enforce strict orthonormality
  # Rsvd <- svd(R)
  # R <- Rsvd$u %*% t(Rsvd$v)
  ## det.
  detR <- det(R)
  if (abs(detR-1) < 1e-6) {
    pixdim_1 <- 1
  } else if (abs(detR+1) < 1e-6) {
    pixdim_1 <- -1
    R[,3] <- -R[,3]
  } else {
    stop("det(R) should be -1 or +1, but it's not.")
  }

  # Get quaternions.
  ## Element-wise variables.
  r11 <- R[1,1]; r12 <- R[1,2]; r13 <- R[1,3]
  r21 <- R[2,1]; r22 <- R[2,2]; r23 <- R[2,3]
  r31 <- R[3,1]; r32 <- R[3,2]; r33 <- R[3,3]
  ## Compute trace.
  trace <- r11 + r22 + r33
  ## Compute quaterions.
  if (trace > 0) {
    a <- 0.5 * sqrt(1 + trace)
    b <- (r32 - r23) / (4 * a)
    c <- (r13 - r31) / (4 * a)
    d <- (r21 - r12) / (4 * a)
  } else if (r11 > r22 && r11 > r33) {
    b <- 0.5 * sqrt(1 + r11 - r22 - r33)
    a <- (r32 - r23) / (4 * b)
    c <- (r12 + r21) / (4 * b)
    d <- (r13 + r31) / (4 * b)
  } else if (r22 > r33) {
    c <- 0.5 * sqrt(1 - r11 + r22 - r33)
    a <- (r13 - r31) / (4 * c)
    b <- (r12 + r21) / (4 * c)
    d <- (r23 + r32) / (4 * c)
  } else {
    d <- 0.5 * sqrt(1 - r11 - r22 + r33)
    a <- (r21 - r12) / (4 * d)
    b <- (r13 + r31) / (4 * d)
    c <- (r23 + r32) / (4 * d)
  }
  # Normalize quaternions.
  qnorm <- sqrt(a^2 + b^2 + c^2+ d^2)
  a <- a / qnorm
  b <- b / qnorm
  c <- c / qnorm
  d <- d / qnorm

  list(
    pixdim = c(pixdim_1, pixdim_234),
    quatern_bcd = c(b,c,d),
    qoffset_xyz = A[seq(3),4]
  )
}
