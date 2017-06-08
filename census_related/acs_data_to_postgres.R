

# get_connection(dbname = "acs", password = "spatial", port = 5433, host = "localhost")


acs_define_schema <- function(schemaname = "acs_tabular"){
  RPostgreSQL::dbSendQuery(.connection$con,
    paste("CREATE SCHEMA IF NOT EXISTS", schemaname))
  RPostgreSQL::dbSendQuery(.connection$con,
    sprintf("SET search_path = '%s', public", schemaname))

  schema <<- schemaname
}


#




#' Check if ACS table already exists in Postgres
#'
#' @param tablename Name of table for Postgres db
#' @param reload If the table exists should it be dropped and re-loaded
#' @param schema Schema name
#'
#' @return
#' @export
#' @details Function used in acs_retrieve_data(). Function will prompt
#' users to be sure they want to overwrite table if it already exists.
#' @examples
acs_table_check <- function(tablename, reload = FALSE){

  # make_schema_public(schema)

  # RPostgreSQL::dbSendQuery(.connection$con,
  #   paste("CREATE SCHEMA IF NOT EXISTS", schema))
  # RPostgreSQL::dbSendQuery(.connection$con,
  #   sprintf("SET search_path = '%s', public", schema))

  tableexists <- dplyr::db_has_table(.connection$con, tablename)

  if (tableexists & !reload) stop(paste("Table", tablename, "already exists in the database"))
  if (tableexists & reload){
    n <- readline("Are you sure you want to drop the existing table: Y or N?")
    n <- as.character(unlist(strsplit(n, " or ")))
    n <- tolower(n)

    if (n == "y") {
      dplyr::db_drop_table(.connection$con, tablename)
      print (paste("Table", tablename, "dropped from database"))
    }
    if (n == "n") stop(paste("Table", tablename, "already exists in the database"))

  }

  if (!tableexists) print ("Table does not exist")
}










#' Title
#'
#' @param year End year of data
#' @param geolevel Geography/summary level of data (i.e. state, county)
#' @param span Number of years of data (1, 3 or 5)
#' @param table Table ID
#'
#' @return
#' @export
#'
#' @examples
#' acs_retrieve_dat(year = 2014, geolevel = "state", span = 5, table = "B17026")
acs_retrieve_dat <- function(
  year = NULL,
  geolevel = NULL,
  span = 5,
  table = NULL){

  if (is.null(year)) stop("Year must be supplied")
  if (is.null(geolevel)) stop("Geolevel must be supplied (i.e. state, county, tract or blockgroup)")
  if (!span %in% c(1, 3, 5)) stop("Span can be 1, 3 or 5. Default is 5.")
  if (is.null(table)) stop("Table id must be supplied (i.e. B01003 for Total Population)")


  tablename <- paste0("acs_", geolevel, "_", year, "_", span, "yr_", table)


  # Prior to getting the data check if the table exists
  acs_table_check(tablename = tablename, reload = TRUE)


  # fips.state <<- acs::fips.state
  # fips.county <<- acs::fips.county


  dat <- switch(geolevel,
    state = datstate(year, span, table),
    county = datcounty(year, span, table),
    tract = dattract(year, span, table),
    blockgroup = datbg(year, span, table)
  )


  add_data_to_postgres(dat = dat, tablename = tablename)

  # return(dat)

}



# dat<-acs_retrieve_dat(year = 2015, geolevel = "state", span = 5, table = "B17026")

# sapply(2010:2015, function(year) acs_retrieve_dat(year = year, geolevel = "county", span = 5, table = "B17026"))




#' Send data to Postgres
#'
#' @param dat Dataframe generated from acs_retrieve_data()
#' @param tablename Tablename generated from acs_retrieve_data()
#'
#' @return
#' @export
#'
#' @examples
add_data_to_postgres<-function(dat = NULL, tablename = NULL){

  RPostgreSQL::dbWriteTable(.connection$con, tablename, dat, row.names = FALSE)

}








# acs::acs.tables.install()




# ACS fetch ---------------------------
#' Get data for supplied geography from ACS
#'
#' @param year
#' @param span
#' @param acsgeo Geography type
#' @param table
#'
#' @return
#' @export
#'
#' @examples
acs_fetch_data <- function(
  year = NULL,
  span = NULL,
  acsgeo = NULL,
  table = NULL){

  acs::acs.fetch(
    endyear = year,
    span = span,
    geography = acsgeo,
    table.number = table,
    col.names = "auto")

}




# State data ---------------------------
#' Retrieves and formats data for state geography
#'
#' @param year
#' @param span
#' @param table
#'
#' @return
#' @export
#'
#' @examples
datstate <- function(year = NULL, span = NULL, table = NULL){

  acsgeo <- acs::geo.make(state = "*", check = TRUE)
  acsdat <- acs_fetch_data(year, span, acsgeo, table)

  dat <- cbind(acsdat@geography, acsdat@estimate)
  rownames(dat) <- 1:nrow(dat)

  dat <- dplyr::mutate(dat,
    state = stringr::str_pad(state, 2, "left", "0")) %>%
    dplyr::rename(GEOID = state) %>%
    dplyr::select(GEOID, NAME, dplyr::everything())

  return(dat)
}





# County data ---------------------------
#' Retrieves and formats data for county geography
#'
#' @param year
#' @param span
#' @param table
#'
#' @return
#' @export
#'
#' @examples
datcounty <- function(year = NULL, span = NULL, table = NULL){

  acsgeo <- acs::geo.make(state = "*", county = "*", check = TRUE)
  acsdat <- acs_fetch_data(year, span, acsgeo, table)

  dat <- cbind(acsdat@geography, acsdat@estimate)
  rownames(dat) <- 1:nrow(dat)

  dat <- dplyr::mutate(dat,
    state = stringr::str_pad(state, 2, "left", "0"),
    county = stringr::str_pad(county, 3, "left", "0"),
    GEOID = paste0(state, county)) %>%
    dplyr::select(GEOID, NAME, state, county, dplyr::everything())

  return(dat)
}










# Tract data ---------------------------
#' Retrieves and formats data for tract geography
#'
#' @param year
#' @param span
#' @param table
#'
#' @return
#' @export
#'
#' @examples
dattract <- function(year = NULL, span = NULL, table = NULL) {

  fips.state <- acs::fips.state
  fips.county <- acs::fips.county


  dat <- lapply(fips.state$STATE_NAME, function(state) {
    print(paste0("Beginning ", state, ": ", Sys.time()))

    tryCatch({
      acsgeo <- acs::geo.make(state = state, county = "*", tract = "*", check = TRUE)
      acsdat <- acs_fetch_data(year, span, acsgeo, table)

      dat <- cbind(acsdat@geography, acsdat@estimate)
      rownames(dat) <- 1:nrow(dat)

      dat <- dplyr::mutate(dat,
        state = stringr::str_pad(state, 2, "left", "0"),
        county = stringr::str_pad(county, 3, "left", "0"),
        tract = stringr::str_pad(tract, 6, "left", "0"),
        GEOID = paste0(state, county, tract)) %>%
        dplyr::select(GEOID, NAME, state, county, tract, dplyr::everything())
    }, error = function(e) {
      print(paste("No data for", state))
      return(NULL)
    }, warning = function(e) {
      print(paste("No data for", state))
      return(NULL)
    })
  })

  dat <- do.call(rbind, lapply(dat, function(x) x[!is.null(x)]))

  return(dat)
}








# Blockgroup data ---------------------------
#' Retrieves and formats data for blockgroup geography
#'
#' @param year
#' @param span
#' @param table
#'
#' @return
#' @export
#'
#' @examples
datbg <- function(year = NULL, span = NULL, table = NULL) {

  dat <- lapply(acs::fips.state$STUSAB[1:2], function(state) {

    tryCatch({

      print(paste0("Beginning ", state, ": ", Sys.time()))
      st <- dplyr::filter(acs::fips.county, State == state)

      dat <- lapply(seq_along(st$County.Name), function(i) {

        tryCatch({
          acsgeo <- acs::geo.make(
            state = state,
            county = st$County.Name[i],
            tract = "*",
            block.group = "*",
            check = TRUE)
          acsdat <- acs_fetch_data(year, span, acsgeo, table)

          print(paste(st$County.Name[i], state, sep = ", "))

          dat <- cbind(acsdat@geography, acsdat@estimate)
          rownames(dat) <- 1:nrow(dat)

          dat <- dplyr::mutate(dat,
            state = stringr::str_pad(state, 2, "left", "0"),
            county = stringr::str_pad(county, 3, "left", "0"),
            tract = stringr::str_pad(tract, 6, "left", "0"),
            GEOID = paste0(state, county, tract, blockgroup)) %>%
            dplyr::select(GEOID, NAME, state, county,
              tract, blockgroup, dplyr::everything())
        }, error = function(e) {
          print(paste0("No data for ", st$County.Name[i],
            ", ", state))
          return(NULL)
        }, warning = function(e) {
          print(paste0("No data for ", st$County.Name[i],
            ", ", state))
          return(NULL)
        })

      })

      dat <- do.call(rbind, lapply(dat, function(x) x[!is.null(x)]))
      return(dat)

    }, error = function(e) {
      print(paste0("No data for ", state))
      return(NULL)
    }, warning = function(e) {
      print(paste0("No data for ", state))
      return(NULL)
    })

  })

  dat <- do.call(rbind, lapply(dat, function(x) x[!is.null(x)]))
  return(dat)
}










##












##
