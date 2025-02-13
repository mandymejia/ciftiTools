% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_names_xifti.R
\name{set_names_xifti}
\alias{set_names_xifti}
\title{Set \code{"xifti"} column names}
\usage{
set_names_xifti(xifti, names)
}
\arguments{
\item{xifti}{A \code{"dscalar"} or \code{"dlabel"} \code{"xifti"} object.}

\item{names}{The new column names, as a character vector with length
equal to the same number of columns in \code{xifti}.}
}
\value{
\code{xifti} with the new column names.
}
\description{
Change the column names of a \code{"dscalar"} or \code{"dlabel"}
\code{"xifti"} object.
}
\seealso{
Other manipulating xifti: 
\code{\link{add_surf}()},
\code{\link{apply_parc}()},
\code{\link{apply_xifti}()},
\code{\link{combine_xifti}()},
\code{\link{convert_xifti}()},
\code{\link{impute_xifti}()},
\code{\link{merge_xifti}()},
\code{\link{move_to_mwall}()},
\code{\link{move_to_submask}()},
\code{\link{newdata_xifti}()},
\code{\link{remap_cifti}()},
\code{\link{remove_xifti}()},
\code{\link{resample_cifti}()},
\code{\link{resample_cifti_from_template}()},
\code{\link{scale_xifti}()},
\code{\link{select_xifti}()},
\code{\link{smooth_cifti}()},
\code{\link{transform_xifti}()}
}
\concept{manipulating xifti}
