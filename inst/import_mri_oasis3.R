library(data.table)

path_root <- "~/GENERIC/dementiaproject/"

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
mri_subjects <- mri_data[1:1000, .(MR.ID)]

write.table(mri_subjects, file = file.path(path_root, "inst/extdata/oasis3/mri_subjects.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE)
input_file <- file.path(path_root, "inst/extdata/oasis3/mri_subjects.csv")
path_dest_data <- "/srv/OASIS_DATA/oasis3/"
xnat_central_username <- "chetboun"
scan_type <- "T1w"
cmd <- paste(file.path(path_root, "inst/xnat_scripts/oasis-scripts-master/download_scans/download_oasis_scans.sh"), input_file, path_dest_data, xnat_central_username, scan_type)

#cmd à lancer directement dans le terminal car demande de mdp obligatoire + le script ne marche pas si on est pas dans le dossier ou il se lance
