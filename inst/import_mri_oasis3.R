library(data.table)
library(purrr)

path_root <- "~/GENERIC/dementiaproject/"
path_data <- "/srv/OASIS_DATA/oasis3/"

# MRI data
mri_data <- read.csv2(file.path(path_root, "inst/extdata/oasis3/mr_sessions_data_full.csv"), header = TRUE, sep = ",")
setDT(mri_data)
str(mri_data)

#Exemple pour le téléchargement des scans via les scripts xnat
#./download_oasis_scans.sh <input_file.csv> <directory_name> <xnat_central_username> <scan_type>
# <input_file.csv> A Unix formatted, comma-separated file containing a column for experiment_id (e.g. OAS30001_MR_d0129)
# <directory_name> A directory path (relative or absolute) to save the scan files to.
#                  If this directory doesn't exist when you run the script, it will be created automatically.
# <xnat_central_username> Your XNAT Central username used for accessing OASIS data on central.xnat.org
# <scan_type> (Optional) The scan type of the scan you want to download. (e.g. T1w, angio, bold, fieldmap, FLAIR)
#                        You can also enter multiple scan types separated by a comma with no whitespace (e.g. T2w,swi,bold).
#                        Without this argument, all scans for the given experiment_id will be downloaded.

mri_subject1 <- mri_data[Subject  == "OAS30001", .(MR.ID)]
mri_subjects <- mri_data[, .(MR.ID)]

write.table(mri_subjects, file = file.path(path_root, "inst/extdata/oasis3/mri_subjects.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE)
input_file <- file.path(path_root, "inst/extdata/oasis3/mri_subjects.csv")
xnat_central_username <- "chetboun"
scan_type <- "T1w"
cmd <- paste(file.path(path_root, "inst/xnat_scripts/oasis-scripts-master/download_scans/download_oasis_scans.sh"), input_file, path_data, xnat_central_username, scan_type)

#cmd à lancer directement dans le terminal car demande de mdp obligatoire + le script ne marche pas si on est pas dans le dossier ou il se lance


# Conversion des .nii en png #oasis 3

path_nii_converter <- file.path(path_root, "/inst/nii2png.py")
# fonctionne python3 nii2png.py -i /srv/OASIS_DATA/oasis3/OAS30001_MR_d0129/anat3/sub-OAS30001_ses-d0129_run-01_T1w.nii.gz -o ~/CEPE/dementiaproject/oasis3/
list_file_niigz <- list.files(path = path_data, pattern = ".nii.gz", full.names = TRUE, recursive = TRUE)
purrr::map(seq(1, length(list_file_niigz), 1), function(x) {
  file <- list_file_niigz[x]
  cmd_gun <- paste0("gunzip ", file)
  system(cmd_gun)
  workdir <- getwd()
  nii_file <- gsub(pattern = ".gz", replacement = "", file)
  dir.create(file.path(dirname(file), "png"))
  setwd(file.path(path_root, "inst/"))
  cmd_convertion <- paste0("python3 nii2png.py -i ", nii_file, " -o ", file.path(dirname(file), "png"))
  system(cmd_convertion)
  file.remove(nii_file)
  setwd(workdir)
  print(paste0("Image converted : ", x, "/", length(list_file_niigz)))
})

#Launch CNN algorithm : nohup python3 -u cnn_algorithm.py &




# Conversion des .nii en png #oasis 1
path_data <- "/srv/OASIS_DATA/oasis1/"

path_nii_converter <- file.path(path_root, "/inst/nii2png.py")
# fonctionne python3 nii2png.py -i /srv/OASIS_DATA/oasis3/OAS30001_MR_d0129/anat3/sub-OAS30001_ses-d0129_run-01_T1w.nii.gz -o ~/CEPE/dementiaproject/oasis1/
list_dir <- list.dirs(path = path_data, full.names = TRUE, recursive = FALSE)

purrr::map(seq(1, length(list_dir), 1), function(x) {
  dir <- list_dir[x]
  print(paste0(x, " / ", length(list_dir)))

  list_raw <- list.files(path = file.path(dir, "RAW"), pattern = ".img", full.names = TRUE, recursive = FALSE)
  map(seq(1, length(list_raw), 1), function(y) {
    dir.create(file.path(dir, "RAW", paste0("anat", y)))
    workdir <- getwd()
    setwd(file.path(path_root, "inst/"))
    cmd_convertion <- paste0("python3 nii2png.py -i ", list_raw[y], " -o ", file.path(dir, "RAW", paste0("anat", y)))
    system(cmd_convertion)
    setwd(workdir)
  })
  list_raw <- list.files(path = file.path(dir, "RAW"), pattern = basename(dir), full.names = TRUE, recursive = FALSE)
  file.remove(list_raw)

  list_processed <- list.files(path = file.path(dir, "PROCESSED","MPRAGE"), pattern = ".img", full.names = TRUE, recursive = TRUE)
  list_fsl <- list.files(path = file.path(dir, "FSL_SEG"), pattern = ".img", full.names = TRUE, recursive = TRUE)

  dir.create(file.path(dir, "SBJ"))
  dir.create(file.path(dir, "GFC"))
  dir.create(file.path(dir, "MASKED_GFC"))
  dir.create(file.path(dir, "MASKED_GFC_FSEG"))

  workdir <- getwd()
  setwd(file.path(path_root, "inst/"))
  cmd_convertion <- paste0("python3 nii2png.py -i ", list_processed[grepl(pattern = "sbj", list_processed)] , " -o ", file.path(dir, "SBJ"))
  system(cmd_convertion)

  cmd_convertion <- paste0("python3 nii2png.py -i ", list_processed[grepl(pattern = "t88_gfc.", list_processed)] , " -o ", file.path(dir, "GFC"))
  system(cmd_convertion)

  cmd_convertion <- paste0("python3 nii2png.py -i ", list_processed[grepl(pattern = "masked_gfc.", list_processed)] , " -o ", file.path(dir, "MASKED_GFC"))
  system(cmd_convertion)

  cmd_convertion <- paste0("python3 nii2png.py -i ", list_fsl , " -o ", file.path(dir, "MASKED_GFC_FSEG"))
  system(cmd_convertion)
  setwd(workdir)

  list_fsl_txt <- list.files(path = file.path(dir, "FSL_SEG"), pattern = ".txt", full.names = TRUE, recursive = TRUE)
  file.copy(from = list_fsl_txt, to = file.path(dir, paste0(basename(dir), "_seg.txt")))

  unlink(file.path(dir, "PROCESSED"), recursive = TRUE)
  unlink(file.path(dir, "FSL_SEG"), recursive = TRUE)
  file.remove(list.files(dir, pattern = ".xml", full.names = TRUE))

})







# Conversion des .nii en png #oasis 2
path_data <- "/srv/OASIS_DATA/oasis2/"

path_nii_converter <- file.path(path_root, "/inst/nii2png.py")
list_dir <- list.dirs(path = path_data, full.names = TRUE, recursive = FALSE)

purrr::map(seq(1, length(list_dir), 1), function(x) {
  dir <- list_dir[x]
  print(paste0(x, " / ", length(list_dir)))
  list_raw <- list.files(path = file.path(dir, "RAW"), pattern = ".img", full.names = TRUE, recursive = FALSE)
  map(seq(1, length(list_raw), 1), function(y) {
    dir.create(file.path(dir, paste0("anat", y)))
    workdir <- getwd()
    setwd(file.path(path_root, "inst/"))
    cmd_convertion <- paste0("python3 nii2png.py -i ", list_raw[y], " -o ", file.path(dir, paste0("anat", y)))
    system(cmd_convertion)
    setwd(workdir)
  })
  unlink(file.path(dir, "RAW"), recursive = TRUE)

})

