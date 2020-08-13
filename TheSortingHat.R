### ************************************
### PREFACE ----
### ************************************

# ** note for KDP: PREFACE version 0.2 ** #

# configure the script
# NOTE: my machine runs MacOS; paths have not been tested on other platforms
# NOTE: this information and more is stored in section I below

setwd("~/Desktop/") # *gasp*
date_ofrun <- "(August 13, 2020 (08_13_20)" # date script was run
name_scrpt <- "TheSortingHat.R" # script filename
name_projd <- "TheSortingHat" # name of project directory
path_projd <- paste(sep = "", getwd(), "/", name_projd) # path to project dir
path_vault <- paste(sep = "", path_projd, "/vault") # path to vault (storage)
wksp_extsn <- paste(sep = "", name_projd, ".Rdata") # recyclable ext for outputs
info_extsn <- paste(sep = "", name_projd, ".txt") # recyclable ext for outputs
PREFACE <- c("date_ofrun","name_scrpt", "name_projd", "path_projd", 
             "path_vault", "wksp_extsn", "info_extsn")

# paths for outputs stored in the central "/vault" (ofv = output file to vault)
I.ofv_info <- paste(sep = "", path_vault, "/info_Section_I_", info_extsn)
I.ofv_wksp <- paste(sep = "", path_vault, "/WS_Section_I_", wksp_extsn)
H.ofv_list <- paste(sep = "", path_vault, "/list_Section_H_", info_extsn)
H.ofv_wksp <- paste(sep = "", path_vault, "/WS_Section_H_", wksp_extsn)
ofv_scrpt_wksp <- paste(sep = "", path_vault, "/WS_", wksp_extsn)

# save PREFACE workspace
PREFACE.lst <- c(ls(pattern = "ofv"), PREFACE, "PREFACE", "PREFACE.lst")
save(list = PREFACE.lst,
     file = paste(sep = "", path_vault, "/WS_PREFACE_", wksp_extsn))

# description of sections:
# PREFACE: configure the script
# SECTION I - Info: gathers path, machine, software, and package information
# SECTION H - Hogwarts Sorting Hat: Random number generator to create groups
# EPILOGUE: save the final workspace

### ************************************
### I - MACHINE/PACKAGE/VERSION Info ----
### ************************************

# NOTE: section I requires objects from the PREFACE to be in the environment
# ** note for KDP: section I version 0.2 ** #

# R packages accessed via require:
require(ggplot2, quietly = T)
require(ggpubr, quietly = T)

# R packages accessed via namespace:
# benchmarkme
# dplyr

# capture R package-related information:
I.Rpac_ctg <- "R package version"
I.Rpackge_a <- data.frame(info = "benchmarkme", section = "I", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_b <- data.frame(info = "dplyr", section = "I; H", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_0 <- rbind(I.Rpackge_a, I.Rpackge_b)

## *********************************** ## *********************************** ##
## *********************************** ## *********************************** ##

# ** note for KDP: captureVersions() version 0.1 ** #
# function takes an input data.frame with:
# column 'info' listing the package name and returns a data.frame with a ...
# ... new column 'value' listing the version information for the package
# NOTE: input column names are rigid; column 'info' must list package name
captureVersions <- function(data = data.frame) {
  # internal checks to ensure correct input classes
  if (!inherits(data, "data.frame")) {
    stop("input data must be class 'data.frame'")
  }
  # store original input df and format the new df
  new_dat <- data
  new_dat$value <- ""
  for(i in 1:length(new_dat$info)) {
    # create package name variable to be tested to ensure correct inputs/outputs
    pack <- unlist(
      packageDescription(
        new_dat$info[i], fields = c("Package", "Version"))[1])
    # test if package name in input data matches 'pack' variable
    # if TRUE, add package version value to the new data.frame
    if (pack == new_dat$info[i]) {
      new_dat$value[i] <- unlist(
        packageDescription(
          new_dat$info[i], fields = c("Package", "Version"))[2])
    }
    # if FALSE, print error message
    else {
      if (!pack == new_dat$info[i]) {
        stop("'pack' variable returned by packageDescription();
       does not match row value in column 'package' of input data;
       incorrect package version valuermation likely returned")
      }
    }
  }
  return(new_dat)
}
# 
# # example usage:
# new_dataframe <- captureVersions(data = dataframe)

## *********************************** ## *********************************** ##
## *********************************** ## *********************************** ##

# captureVersions() function
I.Rpackge_1 <- captureVersions(I.Rpackge_0)
I.Rpackge <- dplyr::select(I.Rpackge_1, category, info, value, script, section)

# capture Project-related information:
I.project_a <- data.frame(category = "project", 
                          info = "date script was run",
                          value = date_ofrun,
                          script = name_scrpt,
                          section = "all", stringsAsFactors = F)
I.project_b <- data.frame(category = "project", 
                          info = "name of project directory",
                          value = name_projd,
                          script = name_scrpt,
                          section = "all", stringsAsFactors = F)
I.project_0 <- rbind(I.project_a, I.project_b)
I.project <- dplyr::select(I.project_0, category, info, value, script, section)

# capture PATH-related information:
I.path_ctg <- "filepath"
I.path_sec <- "all"
I.pathsto_a <- data.frame(info = "working directory",
                          value = paste(getwd(), "/", sep = ""),
                          category = I.path_ctg, script = name_scrpt,
                          section = I.path_sec, stringsAsFactors = F)
I.pathsto_b <- data.frame(info = "path to project directory",
                          value = paste(path_projd, "/", sep = ""),
                          category = I.path_ctg, script = name_scrpt,
                          section = I.path_sec, stringsAsFactors = F)
I.pathsto_c <- data.frame(info = "path to project's central '/vault/'",
                          value = paste(path_vault, "/", sep = ""),
                          category = I.path_ctg, script = name_scrpt,
                          section = I.path_sec, stringsAsFactors = F)
I.pathsto_0 <- rbind(I.pathsto_a, I.pathsto_b, I.pathsto_c)
I.pathsto <- dplyr::select(I.pathsto_0, category, info, value, script, section)

# capture Machine-related information:
I.mach_ctg <- "machine"
I.mach_sec <- "all"
I.mach_cvr <- 1073741824 # number of bytes in 1GB (used for conversion of RAM)
I.machine_a <- data.frame(info = "OS",
                          value = sessionInfo()$running,
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_b <- data.frame(info = "processor",
                          value = benchmarkme::get_cpu()$model_name,
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_c <- data.frame(info = "number of cores",
                          value = parallel::detectCores(logical = F),
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_d <- data.frame(info = "number of threads",
                          value = parallel::detectCores(logical = T),
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_e <- data.frame(info = "RAM",
                          value = 
                            paste(as.numeric(benchmarkme::get_ram()) / I.mach_cvr,
                                  "GB", sep = ""),
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_0 <- rbind(I.machine_a, I.machine_b, I.machine_c, I.machine_d, 
                     I.machine_e)
I.machine <- dplyr::select(I.machine_0, category, info, value, script, section)

# capture R-related information:
I.rlan_ctg <- "base R"
I.rlan_sec <- "all"
I.baseRpl_a <- data.frame(info = "version",
                          value = R.Version()$version.string,
                          category = I.rlan_ctg, script = name_scrpt,
                          section = I.rlan_sec, stringsAsFactors = F)
I.baseRpl_b <- data.frame(info = "nickname",
                          value = R.Version()$nickname,
                          category = I.rlan_ctg, script = name_scrpt,
                          section = I.rlan_sec, stringsAsFactors = F)
I.baseRpl_c <- data.frame(info = "platform",
                          value = R.Version()$platform,
                          category = I.rlan_ctg, script = name_scrpt,
                          section = I.rlan_sec, stringsAsFactors = F)
I.baseRpl_0 <- rbind(I.baseRpl_a, I.baseRpl_b, I.baseRpl_c)
I.baseRpl <- dplyr::select(I.baseRpl_0, category, info, value, script, section)

# capture RStudio-related information:
I.RStudio <- data.frame(info = "version",
                        value = as.character(RStudio.Version()$version),
                        category = "R Studio", script = name_scrpt,
                        section = "all", stringsAsFactors = F)

# rbind all of the above data.frames together and write outputs
I.info <- rbind(I.project, I.machine, I.pathsto, I.baseRpl, I.RStudio, 
                I.Rpackge)

# outputs to the vault
write.table(sep = "\t", row.names = F, x = I.info, file = I.ofv_info)

I.obj <- ls(pattern = "I.")
I.lst <- c(I.obj[grep(pattern = "I.", x = I.obj, ignore.case = F, fixed = T)],
           PREFACE.lst, "captureVersions")
save(list = I.lst, file = I.ofv_wksp)

### ************************************
### H - Step 1: generate random numbers and then create the data ----
### ************************************

# for BIO204 FA2020, I have three sections with 24 students per section...
# this process occurs as follows:
# (1) set seed for random number generator (creates reproducible outputs)
# (2) create data.frames for each section
# (3) generate a vector of 12 random numbers from a set of 24 (three times)
# (4) + and - filter data.frames using the vectors of random numbers
# (5) create a vector of Hogwarts houses
# (6) randomly obtain 2 "houses" by generating two vector positions
# (7:8) copy to avoid overwriting data & assign one house to each group
# (9:10) combine by section and then combine everything together

set.seed(1234567890)

H.S601 <- data.frame(Number = c(1:24), "Section" = ("S601"), 
                     stringsAsFactors = F)
H.S602 <- data.frame(Number = c(1:24), "Section" = ("S602"), 
                     stringsAsFactors = F)
H.S603 <- data.frame(Number = c(1:24), "Section" = ("S603"), 
                     stringsAsFactors = F)

H.vran_S601 <- sample(1:24, 12, replace = F)
H.vran_S602 <- sample(1:24, 12, replace = F)
H.vran_S603 <- sample(1:24, 12, replace = F)

H.S601_pos_0 <- dplyr::filter(H.S601, Number %in% H.vran_S601)
H.S601_neg_0 <- dplyr::filter(H.S601, !Number %in% H.vran_S601)
H.S602_pos_0 <- dplyr::filter(H.S602, Number %in% H.vran_S602)
H.S602_neg_0 <- dplyr::filter(H.S602, !Number %in% H.vran_S602)
H.S603_pos_0 <- dplyr::filter(H.S603, Number %in% H.vran_S603)
H.S603_neg_0 <- dplyr::filter(H.S603, !Number %in% H.vran_S603)

H.houses <- c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")

H.vran_S601_houses <- sample(1:4, 2, replace = F)
H.vran_S602_houses <- sample(1:4, 2, replace = F)
H.vran_S603_houses <- sample(1:4, 2, replace = F)

H.S601_pos <- H.S601_pos_0
H.S601_neg <- H.S601_neg_0
H.S602_pos <- H.S602_pos_0
H.S602_neg <- H.S602_neg_0
H.S603_pos <- H.S603_pos_0
H.S603_neg <- H.S603_neg_0
H.S601_pos$House <- H.houses[H.vran_S601_houses[1]]
H.S601_neg$House <- H.houses[H.vran_S601_houses[2]]
H.S602_pos$House <- H.houses[H.vran_S602_houses[1]]
H.S602_neg$House <- H.houses[H.vran_S602_houses[2]]
H.S603_pos$House <- H.houses[H.vran_S603_houses[1]]
H.S603_neg$House <- H.houses[H.vran_S603_houses[2]]

H.S601_list <- rbind(H.S601_pos, H.S601_neg)
H.S602_list <- rbind(H.S602_pos, H.S602_neg)
H.S603_list <- rbind(H.S603_pos, H.S603_neg)
H.list <- rbind(H.S601_list, H.S602_list, H.S603_list)

### ************************************
### H - WRITE OUTPUTS ----
### ************************************

# outputs to the vault
write.table(sep = "\t", row.names = F, x = H.list, file = H.ofv_list)

H.obj <- ls(pattern = "H.")
H.lst <- c(H.obj[grep(pattern = "H.", x = H.obj, ignore.case = F, fixed = T)])
save(list = H.lst, file = H.ofv_wksp)

### ************************************
### EPILOGUE ----
### ************************************

# output to the vault
save.image(file = ofv_scrpt_wksp)
