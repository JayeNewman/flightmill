#' Import multiple files

#' @name fm_input
#' @title Importing multiple files for a flight mill run
#' @description Loads multiple files from a single directory. The files are expected to have a first cell that has the flight mill run information. This information is extracted from a selected file and new columns are created with the time information which is parsed to a POSIX format.The files are combined and tidied and extra information on species and chamber for the eight chambers id is required.
#' @param file_dir locates the specified directory
#' @param file_name choose a file in the directory that has the run values on the first line
#' @param sp_name the name of the species used in the mill. Expects only one species
#' @param id1, id2, to 1d8, is the chamber code id's of the insect that were flown for each chamber
#' @return A data frame of all the files within the directory. They will be combined
#' @export

fm_input <- function(file_dir, file_name, sp_name, id1, id2, id3, id4, id5, id6, id7, id8) {
  csv_files <- fs::dir_ls(file_dir)
  txt_row <- utils::read.csv(file_name,
                             header = FALSE, nrows = 1) %>%
    stringr::str_trunc(width = 20, side = 'left', ellipsis = '')

  csv_files <- csv_files %>%
    purrr::map_dfr(read_csv, skip = 1, .id = "chamber")
  files <- csv_files %>%
    dplyr::mutate_at("chamber", str_trunc, width = 21, side = 'left', ellipsis = '') %>%
    tidyr::separate("chamber", c('run', 'chamber', '/')) %>%
    dplyr::select(-"/") %>%
    janitor::clean_names("snake") %>%
    dplyr::mutate(starting_time = rep(dmy_hms(txt_row)),
                  across(where(is_character), as_factor),
                  time_series = starting_time + elapsed_sec,
                  species = sp_name,
                  id = case_when(chamber == "CHAMBER1" ~ id1,
                                 chamber == "CHAMBER2" ~ id2,
                                 chamber == "CHAMBER3" ~ id3,
                                 chamber == "CHAMBER4" ~ id4,
                                 chamber == "CHAMBER5" ~ id5,
                                 chamber == "CHAMBER6" ~ id6,
                                 chamber == "CHAMBER7" ~ id7,
                                 chamber == "CHAMBER8" ~ id8
                  ))
  return(files)
}
