#' Surface area calculation
#' 
#' Calculate surface area of a \code{"surf"} object by vertex or face.
#' 
#' @param surf The \code{"surf"} object.
#' @param by \code{"vertex"} or \code{"face"}. For \code{"vertex"}, the result
#'  is the area associated with each vertex: the sum the area of each triangular
#'  face it is a part of, divided by three. For \code{"face"}, the result is
#'  the surface area of each face. 
#' 
#' @return Vector of surface areas \code{by} vertex or face, in the same order
#'  as how the vertices or faces are listed in \code{surf}. The units are the 
#'  square of the units of \code{surf$vertices}. 
#' @keywords export
surf_area <- function(surf, by=c("vertex", "face")) {
  by <- match.arg(by, c("vertex", "face"))
  stopifnot(is.surf(surf))
  nF <- nrow(surf$faces)
  nV <- nrow(surf$vertices)

  ### Step 1: area of each face
  # make an array that's faces by vertices by 3D coordinates
  x <- array(surf$vertices[surf$faces[],], dim=c(nF, 3, 3))

  # subtract the coordinates of the third vertex, C, from those of A and B
  x <- (x - x[,c(3,3,3),])[,c(1,2),]

  # half the magnitude of cross product is the area of the triangle
  # & the magnitude of the cross product is the sqrt of |A|^2*|B|^2 - dot(A,B)^2
  f <- (rowSums((x[,1,])^2) * rowSums((x[,2,])^2)) - rowSums((x[,1,]*x[,2,]))^2
  f <- sqrt(f) / 2
  
  if (by == "face") { return(f) }
  
  ### Step 2: Area of each vertex
  nFpV <- table(surf$faces)
  maxFpV <- max(nFpV)
  
  if (!(all(nFpV %in% c(maxFpV, maxFpV-1)))) {
    # Same result as below. Simpler code; much slower execution time.
    # But the below code assums no vert is part of less than 6 (or whatever the
    # max is) minus one faces. 
    v <- vector("numeric", length=nV)
    for (vv in seq(nV)) {
      v[vv] <- sum(f[which(rowSums(surf$faces==vv) > 0)])
    }
    return(v/3)
  }

  # helper function for below
  add_val <- function(vec, idx, val){
    nN <- length(vec)
    if (idx==1) {
      c(val, vec)
    } else if (idx==nN+1) {
      c(vec, val)
    } else {
      c(vec[seq(idx-1)], val, vec[seq(idx, nN)])
    }
  }
  
  # order face index by vert index. for tri mesh, 6 or rarely 5 faces per vert
  q <- ceiling((order(surf$faces[])-1) %% nrow(surf$faces))+1
  
  # assign dummy face (will have zero area) to vertices with 5 faces
  verts_with_less <- sort(which(nFpV == maxFpV - 1))
  for (vv in seq(length(verts_with_less))) {
    q <- add_val(q, (vv*maxFpV), nF+1)
  }
  stopifnot(length(q) == nrow(surf$vertices)*maxFpV)
  
  colSums(matrix(c(f, 0)[q], nrow=maxFpV)) / 3
}
