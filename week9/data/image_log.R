install.packages("suntools")
install.packages("terra")
install.packages("camtrapR")
install.packages("taxize")
install.packages("overlap")

library(camtrapR)
library(taxize)
library(overlap)

dir.create("../data/station_tmp")
file.rename("../data/camera_03_85_2025",
            "../data/station_tmp/camera_03_85_2025")

imageRename(
  inDir = "../data/station_tmp",
  outDir = "../results/images",
  keepCameraSubfolders = FALSE,
  hasCameraFolders = FALSE,
  copyImages = TRUE
)

sp_table = recordTable(inDir = "../results/images/camera_03_85_2025",
IDfrom = "directory", minDeltaTime = 30,
deltaTimeComparedTo = "lastIndependentRecord")

library(utils)
write.csv(sp_table,"../results/sp_table_camera_03_85_2025.csv")
