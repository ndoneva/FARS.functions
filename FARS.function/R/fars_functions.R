#' Read data from csv file
#'
#' \code{fars_read} is a function that reads data from a csv file in the CURRENT working
#' directory and returns it as an object of the "tbl_df" class.
#'
#' @details The progress is not shown while the file is being read and all messages are
#'    omitted. This function uses the \code{read_csv()} function from the \code{readr}
#'    package. After reading the data from the csv file, the \code{tbl_df()} function
#'    from the \code{dplyr} package is called to convert the data into the tbl_df format
#'    before returing it.
#'
#' @param filename A character string containing the name of the csv file to be read,
#'    which must be at the CURRENT directory.
#'
#' @return If the file exists, this function returns a data frame table. If the file doesn't
#'    exist it raises an error message.
#'
#' @note \itemize{
#'    \item If the file does not exist in the current directory, this function stops
#'    execution with an error message.
#'
#'    \item This functions is exported (users will have direct access to the funciton when
#'    they load the package containing this function).
#'    }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2014.csv.bz2") # file in current working directory
#' fars_read("data123.csv") # Error: file 'data123.csv' does not exist.
#' }
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Generate the name of the file
#'
#' \code{make_filename} is a function that generates the file name of the csv file to be
#' read.
#'
#' @details This function uses the year (input) to generate the file name for the Fatality
#'    Analysis Reporting System (FARS) data file. It converts the year into an integer and
#'    then uses the \code{sprintf()} function to format the file name.
#'
#' @param year An integer or string indicating the year that generate the data file.
#'
#' @return The name of the file as a string.
#'
#' @note The \code{year} parameter of this function must be an integer or a string with
#'    number. If \code{year} parameter is invalid, then an invalid file name will be generated
#'    containing "NA" as the year in the file name which doesn't exist in the FARS database.
#'
#' @examples
#' \dontrun{
#' make_filename(2014) #generates "accident_2014.csv.bz2"
#' make_filename("2015") #generates "accident_2015.csv.bz2"
#' make_filename("abcd") #generates "accident_NA.csv.bz2"
#' }
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Reads data for multiple years
#'
#' \code{fars_read_years} is a function that reads Fatality Analysis Reporting System (FARS)
#' data for multiple years.
#'
#' @details This function processes each element of a vector or list of years. To generate a
#'    file name with the year this function calls the supporting function \code{make_filename()},
#'    and then calls the supporting function \code{fars_read()}, to read the FARS data file
#'    with  the generated file name. If the file exists, the function creates a new column
#'    (year), then selects the month and year columns, and adds them to an R list.
#'
#' @param years A vector of years.
#'
#' @return A list of FARS data for the years in the parameter \code{years}.
#'
#' @note If the file doesn't exit, the function gives a warning message, and adds nothing
#'    (NULL) to the list.
#'
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2014, 2015, 2016))
#' fars_read_years(c("2014", "2015", "2016"))
#' }
#'
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Summarize data data for multiple years
#'
#' \code{fars_summarize_years} is a function that summarize Fatality Analysis Reporting
#' System (FARS) data for multiple years into columns.
#'
#' @details This function produces a "tidy" data frame of FARS data for multiple years. Data
#'    is summarized by each month of each year with each year in its own column. This function
#'    uses \code{\link{fars_read_years}} function to generate a list of month and year.
#'
#' @param years A vector of years.
#'
#' @return A dataframe with monthly number of accidents in each row and years as colunms.
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2014, 2015))
#' fars_summarize_years(c("2013", "2014"))
#' }
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plot the location of accidents in a given state and year
#'
#' \code{fars_map_state} is a function that creates a map with the fatalities in a US State
#' during a year according to the Fatality Analysis Reporting System (FARS) database.
#'
#' @details This function takes the state number and a year as inputs and calls the supporting
#'    functions \code{make_filename()} and \code{fars_read()}.
#'
#' @param state.num An integer that represents the state number.
#' @param year An integer or string indicating the year.
#'
#' @return Plots a map of the state, where the fatalities are the dots on the map.
#'
#' @note The \code{state.num} parameter of this function must be an integer or a string with
#'    number. This function stops execution with an error message if \code{state.num} is
#'    invalid. If there hasn't been any accident in that year, this function produces a message.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(30, 2013)
#' fars_map_state(56, 2013)
#' fars_map_state(70, 2014) ## Error: "invalid STATE number: 70"
#' }
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
