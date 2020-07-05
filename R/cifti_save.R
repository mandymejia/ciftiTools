#' Save cifti object as cifti file
#'
#' @param cifti Object of class cifti
#' @param fname_out Name of cifti file to write, ending in .dtseries.nii, .dscalar.nii or .dlabel.nii
#' @param wb_cmd Path to Connectome Workbench executable file, ending in 'wb_command' (Mac/linux) or 'wb_command.exe' (Windows).
#'
#' @return Logical indicating whether CIFTI file was created.
#' @export
#'
# # cifti_save <- function(cifti, fname_out, wb_cmd=NULL){
# #
# #   wb_cmd <- get_wb_cmd_path(wb_path)
# #
# #   cifti_extn <- get_cifti_extn(fname_out)
# #   if(grepl('dtseries',cifti_extn)) create_cmd <- '-cifti-create-dense-timeseries'
# #   if(grepl('dscalar',cifti_extn)) create_cmd <- '-cifti-create-dense-scalar'
# #   if(grepl('dlabel',cifti_extn)) create_cmd <- '-cifti-create-label'
# #
# #   if(!is.null(cifti$CORTEX_LEFT)) cmd_left <- paste
# #
# #   system(paste(wb_cmd, create_cmd, fname_out,
# #                '-volume', vol_orig, labels_orig,
# #                '-left-metric', surf_target_L,
# #                '-roi-left', roi_target_L,
# #                '-right-metric', surf_target_R,
# #                '-roi-right', roi_target_R, sep=' '))
# #   }
# #
# # }
