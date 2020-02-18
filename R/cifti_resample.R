## TO DO: Test this using original HCP data (see Matlab script)

#target_res target resolution (number of vertices per hemisphere)
cifti_resample <- function(cifti_orig, sphere_orig_L, sphere_orig_R, target_res, wb_cmd, make_helper_files=TRUE){

  dir <- dirname(cifti_orig)
  if(!('helper_files_resampling' %in% list.dirs(dir, full.names=FALSE))) dir.create(file.path(dir,'helper_files_resampling'))
  dir2 <- file.path(dir, 'helper_files_resampling')

  # Step 1. Create new spherical GIFTI files using wb_command -surface-create-sphere
  sphere_target_R <- file.path(dir2,'Sphere.target.R.surf.gii')
  sphere_target_L <- file.path(dir2,'Sphere.target.L.surf.gii')
  if(make_helper_files){
    system(paste(wb_cmd,'-surface-create-sphere', target_res, sphere_target_R, sep=' '))
    system(paste(wb_cmd, '-surface-flip-lr', sphere_target_R, sphere_target_L, sep=' '))
    system(paste(wb_cmd, '-set-structure', sphere_target_R, 'CORTEX_RIGHT', sep=' '))
    system(paste(wb_cmd, '-set-structure', sphere_target_L, 'CORTEX_LEFT', sep=' '))
  }

  # Step 2. Create CIFTI space with target number of vertices

  # Use -cifti-separate to get separate CORTEX_LEFT, CORTEX_RIGHT and both medial walls

  vol_orig <- file.path(dir2, 'vol.orig.nii')
  labels_orig <- file.path(dir2, 'labels.orig.nii')
  surf_orig_L <- file.path(dir2, 'surf.orig.L.func.gii')
  surf_orig_R <- file.path(dir2, 'surf.orig.R.func.gii')
  roi_orig_L <- file.path(dir2, 'roi.orig.L.func.gii')
  roi_orig_R <- file.path(dir2, 'roi.orig.R.func.gii')
  system(paste(wb_cmd, '-cifti-separate', cifti_orig, 'COLUMN -volume-all', vol_orig, '-label', labels_orig, '-metric CORTEX_LEFT', surf_orig_L, '-roi', roi_orig_L, '-metric CORTEX_RIGHT', surf_orig_R, '-roi', roi_orig_R, sep=' '))

  # b) Use wb_commmand -metric-resample to create 10k versions of these using the 32k sphere and your new 10k sphere
  validroi_target_L <- file.path(dir2, 'valid-roi.10k.L.func.gii')
  validroi_target_R <- file.path(dir2, 'valid-roi.10k.R.func.gii')
  roi_target_L <- file.path(dir2, 'roi.10k.L.func.gii')
  roi_target_R <- file.path(dir2, 'roi.10k.R.func.gii')
  system(paste(wb_cmd, '-metric-resample', surf_orig_L, sphere_orig_L, sphere_target_L, 'BARYCENTRIC', surf_target_L, '-current-roi', roi_orig_L, '-valid-roi-out', validroi_target_L, sep=' '))
  system(paste(wb_cmd, '-metric-resample', surf_orig_R, sphere_orig_R, sphere_target_R, 'BARYCENTRIC', surf_target_R, '-current-roi', roi_orig_R, '-valid-roi-out', validroi_target_R, sep=' '))
  system(paste(wb_cmd, '-metric-resample', roi_orig_L,  sphere_orig_L, sphere_target_L, 'BARYCENTRIC', roi_target_L, sep=' '))
  system(paste(wb_cmd, '-metric-resample', roi_orig_R,  sphere_orig_R, sphere_target_R, 'BARYCENTRIC', roi_target_R, sep=' '))

  ## TO DO: generalize this to dscalar, etc.

  #HERE

  # Step 3. Create a template CIFTI dense timeseries.
  cifti_template_target <- file.path(dir2, 'cifti.target.dtseries.nii')
  system(paste(wb_cmd, '-cifti-create-dense-timeseries', cifti_template_target, '-volume', vol_orig, labels_orig, '-left-metric', surf_target_L, '-roi-left', roi_target_L, '-right-metric', surf_target_R, '-roi-right', roi_target_R, sep=' '))


  ##############################################################
  # MESH RESAMPLING (gifti_resample)
  ##############################################################

  fname_gifti_orig = #cortical spherical surface in original resolution (*.surf.gii) (create in workbench??)
  fname_gifti_target = #cortical spherical surface in target resolution (*.surf.gii) (create in workbench)
  cmd = paste0(wb_cmd, '-surface-resample', fname_gifti_orig, sphere_orig, sphere_target, 'BARYCENTRIC', fname_gifti_target, sep=' ')
  system(cmd)


  ##############################################################
  # TIMESERIES RESAMPLING (cifti_resample)
  ##############################################################

  #most of these files should be created above, check
  sphere_orig_L = NULL #left cortical spherical surface in original resolution (*.surf.gii) (create in workbench)
  sphere_target_L = NULL #left cortical spherical surface in target resolution (*.surf.gii) (create in workbench)
  sphere_orig_R = NULL #right cortical spherical surface in original resolution (*.surf.gii) (create in workbench)
  sphere_target_R = NULL #right cortical spherical surface in target resolution (*.surf.gii) (create in workbench)
  cifti_template_target = NULL #template CIFTI timeseries in target resolution (create in workbench)

  cifti_orig = #fname of cifti data in original resolution (resample FROM this)
  cifti_target = #fname of cifti data in target resolution (resample TO this)

  cmd = paste(wb_cmd, '-cifti-resample', cifti_orig, 'COLUMN', cifti_template_target, 'COLUMN BARYCENTRIC CUBIC', cifti_target, '-left-spheres',  sphere_orig_L, sphere_target_L, '-right-spheres', sphere_orig_R, sphere_target_R, sep=' ')
  system(cmd)


}
