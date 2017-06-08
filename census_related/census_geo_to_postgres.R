#' Download and unzip Census geography
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @details
#' Data comes from \url{https://www2.census.gov/geo/tiger/}, is downloaded and unzipped
#' @examples
#' add(1, 1)
#' add(10, 1)
download_tiger_zip <- function(year = year, type = type, directory = NULL) {
  # year <- "2016"
  # type <- "CD"
  url1 <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/%s/", year, type)
  cat("Downloading from ", url1)
  zips_avail <- find_zips_at_URL(url1)

  if(length(zips_avail)>1) stop("There is more than one zip file")

  url_to_file <- paste0(url1, zips_avail)

  directory <- ifelse(is.null(directory), tempdir(), directory)
  directory <- gsub("//", "/", directory)

  filedir <- paste0(directory, "/", zips_avail)
  download.file(url_to_file, filedir, mode = 'wb')
  unzip(filedir, exdir = directory)
  shape <- gsub(".zip", ".shp", filedir)

  cat("File written to:", filedir)

  shape

}



#' Find hrefs that have ".zip" in them
#'
#' @param url A url
#' @return A list of hrefs that have zips.
#' @export
#' @details
#' This function will find and return the href tags that have a '.zip' in them
#' @examples
#' add(1, 1)
#' add(10, 1)

find_zips_at_URL <- function(url1){

  website <- read_html(url1)
  hrefs <- website %>%
    html_nodes('a') %>%
    html_attr('href')

  hrefs[grepl(".zip", hrefs, fixed = TRUE)]

}



#' Take input file path and upload to postgres using ogr2ogr in background
#'
#' @param filedir path to a shapefile
#' @param reload If an existing table exists should it be dropped
#' @return A list of hrefs that have zips.
#' @export
#' @examples
#' add_geo_to_postgres(path, reload = TRUE)
#'

add_geo_to_postgres <- function(filedir, dbname = "usa_data", schema = "census_geography", reload = FALSE){
  # filedir <- shape
  #DROP TABLE "census_geography"."tl_2016_us_state";
  con <- .connection$con
  make_schema_public(schema)

  basename <- gsub(".shp", "", basename(filedir), fixed = TRUE)
  schema_table <- paste(schema, basename, sep = ".")
  tableexists <- dplyr::db_has_table(.connection$con, basename)


  if(tableexists & !reload) stop(paste("Table", basename, "already exists in the database"))

  if(tableexists & reload){
    dplyr::db_drop_table(con, basename)
  }

  cmd <- sprintf('ogr2ogr -f "PostgreSQL" PG:"host=192.168.1.4 port=5432 user=postgres dbname=%s password=spatial" "%s" -nln "%s.%s" -nlt PROMOTE_TO_MULTI -lco "GEOM_TYPE=geometry" -lco "GEOMETRY_NAME=geom"',
                 dbname, filedir, schema, basename)

  cat("About to add to database using command\n", cmd)

  system(cmd)
  invisible()

}



#' Run SQL to make a schema part of public so no dot syntax is required
#'
#' @param url A url
#' @return A list of hrefs that have zips.
#' @export
#' @examples
#'
#'

make_schema_public <- function(schemaname){

  RPostgreSQL::dbSendQuery(.connection$con, sprintf("SET search_path = '%s', public", schemaname))
  invisible()
}



#' Find hrefs that have ".zip" in them
#'
#' @param url A url
#' @return A list of hrefs that have zips.
#' @export
#' @examples
#' get_table_insert_pg(2016, "CD")


get_table_insert_pg <- function(year, type, reload = FALSE){

  path <- download_tiger_zip(year, type)
  add_geo_to_postgres(path, reload = reload)


}
