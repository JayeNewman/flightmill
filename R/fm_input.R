#' Import multiple files

#' @name fm_input
#' @title Importing multiple files for a flight mill run
#' @description Loads multiple files from a single directory. The files are expected to have a first cell that has the flight mill run information. This information is extracted from a selected file and new columns are created with the time information which is parsed to a POSIX format.The files are combined and tidied and extra information on species and flightmill for the eight flightmills id is required.
#' @param file_dir locates the specified directory.
#' @param file_name choose a file in the directory that has the run values on the first line.
#' @param sp_name the name of the species used in the flight mill. Expects only one species.
#' @importFrom magrittr %>%
#' @importFrom stats mad median end start lag na.omit
#' @return A combined data frame of all the files within the directory.
#' @export

# To import the id_lookup dataframe with two columns: id and flight mill which contains the unique pairings
# User will need to create a lookup table to be able to use this

# An example lookup table:
#
# file_dir <- ("/data/20091703")
#
# flightmill <- c("FLIGHTMILL1", "FLIGHTMILL2", "FLIGHTMILL3", "FLIGHTMILL4",
#              "FLIGHTMILL5", "FLIGHTMILL6", "FLIGHTMILL7", "FLIGHTMILL8")
#
# id <- c("id1", "id2", "id3", "id4",
#         "id5", "id6", "id7", "id8")

fm_input <- function(file_dir, file_name, sp_name) {

  csv_files <- fs::dir_ls(paste0(getwd(),file_dir))

  csv_files <- csv_files %>%
    purrr::map_dfr(read_csv, skip = 1, .id = "flightmill")

  file_path <- paste0(getwd(),file_dir,"/",file_name)

  txt_row <- utils::read.csv(file = file_path, header = FALSE, nrows = 1)

  df <- csv_files %>%
    dplyr::mutate_at("flightmill", str_trunc, width = 21, side = 'left', ellipsis = '') %>%
    tidyr::separate("flightmill", c('run', 'flightmill', '/')) %>%
    dplyr::select(-"/") %>%
    janitor::clean_names("snake") %>%
    dplyr::mutate(starting_time = rep(dmy_hms(txt_row)),
                  time_series = starting_time + elapsed_sec,
                  species = sp_name)

  id_lookup <- data.frame(flightmill, id, stringsAsFactors = FALSE)

  n <- nrow(df)

  df <- cbind(df, id = rep(NA, n))

  #id_lookup dataframe with two columns: id and flightmill which contains the unique pairings

  for(i in 1:nrow(id_lookup))
  {
    rowInds <- which(df[, "flightmill"] == id_lookup[i, "flightmill"])
    if(length(rowInds) == 0)
    {
      #stop("One of the flightmills in the lookup does not have any corresponding rows in the dataframe.")
      cat("Warning.... \n")
    }
    df[rowInds, "id"] <- id_lookup[i, "id"]
  }

  # df <- df %>%
  #   rename(flightmill = flightmill)

  return(df)
}
