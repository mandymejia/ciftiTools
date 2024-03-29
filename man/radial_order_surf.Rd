% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_surf.R
\name{radial_order_surf}
\alias{radial_order_surf}
\title{Order Vertices on Circular Manifold}
\usage{
radial_order_surf(vertices)
}
\arguments{
\item{vertices}{A \eqn{V \times 3} matrix, where each row contains the Euclidean
coordinates at which a given vertex in the mesh is located. \eqn{V} is the
number of vertices in the mesh}
}
\value{
Index ordering of \code{vertices}
}
\description{
Order vertices on circular manifold by radians (after 2D CMDS projection).
}
\keyword{internal}
