# *****************************************************************************
#  ---------------------------
# *****************************************************************************


#' Download pesticide data and unzip
#'
#' @param yr
#' @param destdir
#'
#' @return
#' @export
#'
#' @examples
#'
pesticide_data_download<-function(yr, url = NULL, destdir = NULL){

  if(is.null(url)) url <- "ftp://pestreg.cdpr.ca.gov/pub/outgoing/pur_archives/"

  #yr<-2014
  # url <- "ftp://pestreg.cdpr.ca.gov/pub/outgoing/pur_archives/"
  # destdir <- NULL
  file<-paste('pur', yr, '.zip', sep="")
  urlfile<-paste(url,file, sep="")
  #destdir<-"D:/work/big_current_work/cehtp_pesticide/raw_pur_data/"

  if(is.null(destdir)) destdir <- tempdir()
  destdir <- gsub("//", "/", destdir)

  destdiryr<-paste(destdir,"_", yr, sep="")

  if(!file.exists(destdiryr)){
    dir.create(destdiryr)
  }

  destfile<-paste(destdiryr, "/", file, sep="")
  download.file(urlfile, destfile)
  unzip(destfile, exdir = destdiryr)


  # allfiles<-list.files(destdiryr)
  # files<-allfiles[grep("udc", allfiles)]
  # paste(destdiryr,"/", files, sep="")
  destdiryr
}


# *****************************************************************************
# Run psql command ---------------------------
# *****************************************************************************


#' Insert data into the postgresql database
#'
#' This function will take the path to the raw data downloaded with the pesticide_data_download()
#' function and import the data into the pesticide_data.pesticide_data database. Year is determined
#' using the last four digits of the input path.
#'
#' @param datapath this is the path to the unzipped raw data
#' @param user
#' @param host
#' @param dbname
#'
#' @return
#' @export
#'
#' @examples
pesticide_copy_to_db<-function(datapath, user, host, dbname){
  #datapath <- "/var/folders/67/5936qfdd7fb2rtrxbm19bmnw0000gn/T/RtmpvFkN3k_2013"
  # user <- "postgres"
  # host <- "localhost"
  # dbname <- "pesticide"
  yr <- substring(datapath, nchar(datapath)-3, nchar(datapath))
  cat(sprintf("Working on year %s", yr))
  allfiles<-list.files(datapath)
  files<-allfiles[grep("udc", allfiles)]
  county_files<-paste(datapath,"/", files, sep="")

  if(length(county_files)!=58) stop("There should be 58 county files, double check the number of files")

  if(yr%in%c(1991:2001)){xtr<-"document_no,summary_cd,record_id) FROM '"}
  if(yr%in%c(2002:2004)){xtr<-"document_no,summary_cd,record_id, error_flag) FROM '"}
  if(yr%in%c(2005:2020)){xtr<-"document_no,summary_cd,record_id,comtrs, error_flag) FROM '"}


  lapply(county_files, function(x){
    cmd <- sprintf("COPY pesticide_data.pesticide_data(use_no,prodno,chem_code,prodchem_pct,
               lbs_chm_used,lbs_prd_used,amt_prd_used,unit_of_meas,acre_planted,unit_planted,acre_treated,unit_treated,applic_cnt,
               applic_dt,applic_time,county_cd,base_ln_mer,township,tship_dir,range,range_dir,section,site_loc_id,grower_id,
               license_no,planting_seq,aer_gnd_ind,site_code,qualify_cd,batch_no, %s%s' DELIMITER ',' CSV HEADER",
               xtr, x)

    run_psql_cmd(cmd, host = host, user = user, dbname = dbname)

    invisible()
  })


  run_psql_cmd("VACUUM ANALYZE pesticide_data.pesticide_data", host = host, user = user, dbname = dbname)

  invisible()
}

#' Title
#'
#' @param yr year of data to be downloaded
#' @param user user name
#' @param host host name
#' @param dbname database name
#' @param cleanup if true the directory will be deleted
#' @param url the URL to download data from, fed to pesticide_data_download where the default is specified at DPR
#'
#' @return
#' @export
#'
#' @examples
pesticide_download_plus_copy <- function(yr, user, host, dbname, cleanup = TRUE, url = NULL){

  # yr <- 2012
  # url <- NULL

  destloc <- pesticide_data_download(yr, url = url)
  pesticide_copy_to_db(destloc, user, host, dbname )

  if(cleanup) unlink(destloc, recursive = TRUE)
  print(Sys.time())
  print(yr)
  invisible()
}



# *****************************************************************************
# Load auxiliary tables ---------------------------
# *****************************************************************************


#' Takes the raw text files with information on sites, groups, chemicals and
#' inputs them into the database. These raw text files should be in pesticide/data
#' and were created using the script process_raw_pesticide_data. The only parameter
#' is the list of tables to truncate in case you just want to truncate one
#'
#' @param whichones which table do you want to load
#'
#'
#' @return
#' @export
#'
#' @examples
#' pesticide_load_auxiliary_tables()
pesticide_load_auxiliary_tables <- function(whichones = c("chemical", "chemical_groups", "site", "site_groups",
                                                          "chemicals_to_groups", "sites_to_groups")){

  # KEEP this in case you need to use an alternative to psql
  # lapply(tbls, function(x){
  #   #x <- tbls[1]
  #       dat <- readr::read_csv()
  #       RPostgreSQL::dbWriteTable(.connection$con,
  #                                 name = c("pesticide_auxtables", x),
  #                                 value = data.frame(dat),
  #                                 append = TRUE,
  #                                 row.names = FALSE)
  # })

  path <- system.file("pesticide/data", package = "zrsamisc")
  cmd <- paste(sprintf("COPY pesticide_auxtables.%2$s FROM '%1$s/%2$s.txt' DELIMITER '|' CSV HEADER;",
                       path, whichones), collapse = " ")


 run_psql_cmd(cmd, "localhost", "postgres", "pesticide")

}


# *****************************************************************************
# Change years in the insert templates ---------------------------
# *****************************************************************************



change_insert_years <- function(begyear = 1991, endyear){

  path <- system.file("pesticide/sql", package = "zrsamisc")
  files <- list.files(path, full.names = TRUE)
  files <- files[grep("insert.*template.sql$", files)]

  lapply(files, function(x){
    #x <- files[1]
    dat <- readLines(x)
    dat <- gsub("XXXX..YYYY", paste0(begyear, "..", endyear), dat)
    cat(dat, file=gsub("template", "activeDO_NOT_EDIT", x), sep="\n")
    invisible()
  })

  invisible()

}





