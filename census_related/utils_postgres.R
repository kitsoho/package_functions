#' This function creates the connection to a database
#'
#' @family postgresql functions
#' @param dbname the database.
#' @param host database host, usually 'localhost'
#' @return .connection -- which is a global variable
#' @examples
#' get_connection(dbname="columbiaBike", host="localhost",
#' password="spatial", port=5433, user="postgres")
#' @export

get_connection<-function(dbname,
                         password = "spatial",
                         host="192.168.1.4",
                         port=5432,
                         user="postgres"){


  # note the double arrow to make global
  .connection<<-try({dplyr::src_postgres(dbname=dbname,
                                         host=host,
                                         password=password,
                                         port=port,
                                         user=user)}, silent=TRUE)
}



#' This function creates a new postgreSQL database .
#'
#' \code{create_postgres_db} will create a new postgresql database.
#'
#' Create a new postgres database. This function first tests if the
#' database already exists in which case it asks the user if they
#' want to delete it first.
#'
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param port. You likely don't need to change this.
#' @return user.
#' @examples
#' create_postgres_db(template = "zevross", dbname = "deletethis")
#' @export
#'
create_postgres_db<-function(dbname, host = "localhost", user = "postgres", template, dropfirst = FALSE){

  # TODO, make a default template, should it be postgis?

  existing_db <- get_list_db(host, user)
  if(dbname%in%existing_db){
    cat("That db name exists do you want to drop it first?")
    choice <- menu(c("No, don't delete the database", "Yes, completely delete the database"))
    if(choice == 1){
      invisible()
    }else{
      q <- sprintf('dropdb -h %s -U %s %s', host, user,  dbname)
      system(q)
    }

  }

  q <- sprintf('createdb -h %s -U %s -T %s %s', host, user, template, dbname)
  system(q)

}



#' Function to get list of databases
#'
#' \code{get_list_db} will return a vector of databases
#' @family postgresql functions
#' @param dbname Give the database a name.
#' @param port. You likely don't need to change this.
#' @return user.
#' @examples
#' create_database("columbiaBike", port=5432)
#' @export
#'
get_list_db<-function(host = "localhost", user = "postgres"){
  #host <- "localhost"
  #user <- "postgres"
  cmd <- sprintf("psql -h %s -U %s -lqt | cut -d \\| -f 1", host, user)
  #https://stackoverflow.com/questions/14549270/check-if-database-exists-in-postgresql-using-shell
  trimws(system(cmd, intern = TRUE))

}



# *****************************************************************************
# Add tables ---------------------------
# *****************************************************************************

#' This function adds tables to a postgreSQL database.
#'
#' This was originally created with the pesticide database in mind which has some
#' code to create key tables. As part of this code there is a schema name so
#' this function has a couple of arguments for dealing with this. So in the pesticide
#' table we have schemaname.sites_to_groups etc. A warning about table not existing, skipping
#' is probably fine because this refers to the DROP IF EXISTS. Note that the SQL files used
#' as input can only have comments at the beginning of lines because this looks for ^--
#'
#' @family postgresql functions
#' @param host
#' @param user
#' @param dbname
#' @param path
#' @param filename
#' @param use_schema refers to whether there is a schemaname.tablename convention and you can use the schemaname
#' argument to set a schemaname
#' @param schemaname only relevant if use_schema is TRUE
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' get_connection("deletethis", "postgres", "localhost")
#' add_tables_db("pesticide/sql", "create_pesticide_db_tables.sql")
#' add_tables_db("pesticide/sql", "create_pesticide_auxiliary_tables.sql", use_schema = TRUE, schemaname = "pesticide_auxtables")
#' @export

run_sql_script<-function(path, filename, host, user, dbname, use_schema = FALSE, schemaname = "public", escape_dollar = FALSE){

  # host <- "localhost"
  # dbname <- "pesticide"
  # path <- "pesticide/sql"
  # filename <- "create_pesticide_auxiliary_tables.sql"
  if(use_schema){
    existing_schema <- get_list_schema(host, user, dbname)
    if(!schemaname%in%existing_schema){
      cat("That schema does not exist, do you want to create it first?")
      choice <- menu(c("No, do not create a new schema", sprintf("Yes, please create the new schema with name %s", schemaname)))
      if(choice == 2){
        #RPostgreSQL::dbSendQuery(.connection$con, sprintf("CREATE SCHEMA %s", schemaname))
        run_psql_cmd(sprintf("CREATE SCHEMA %s", schemaname), host, user, dbname)
      }


    }
  }

  sqlfile<-system.file(path, filename, package = "zrsamisc")
  sqlcode <- readLines(sqlfile)
  sqlcode <- gsub("schemaname", schemaname, sqlcode)


  sqlcode <- gsub("\\t", "", sqlcode)
  sqlcode <- sqlcode[sqlcode!=""]
  cmd <- paste(sqlcode[!grepl("^--", sqlcode)], collapse = " ")

  if(escape_dollar){
    cmd <- gsub("$", "\\$", cmd, fixed = TRUE)
  }

  #RPostgreSQL::dbSendQuery(.connection$con, cmd)

  run_psql_cmd(cmd, host, user, dbname)


}


# *****************************************************************************
# Run psql command ---------------------------
# *****************************************************************************

#' This function runs a command from psql, mainly for longer commands or
#' commands that are for creating or dropping dbs. Otherwise use dplyr or
#' RPostreSQL
#'
#' This was originally created with the pesticide database in mind which has some
#' code to create key tables. As part of this code there is a schema name so
#' this function has a couple of arguments for dealing with this. So in the pesticide
#' table we have schemaname.sites_to_groups etc
#'
#' @family postgresql functions
#' @param host
#' @param user
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' run_psql_cmd("select * from blah10", "localhost", "postgres", "deletethis")
#' @export

run_psql_cmd <- function(cmd, host, user, dbname, addl_flags = c("-a", "-c"), intern = FALSE){

  # This was used for an external SQL file
  #cmd<-paste0('psql -U ', user,' -d ', dbname,' -a -f "', sqlfile, '"')
  #cmd <- "select * from blah10"
  # cmd <- "\\dn"

  # The -a I forget, the -c is a command
  addl_flags <-
    cmd2<-paste0('psql -U ', user,' -d ', dbname, " ", paste(addl_flags, collapse = " "), ' "', cmd, '"')
  system(cmd2, intern = intern)
}




# *****************************************************************************
# Get a db schema ---------------------------
# *****************************************************************************

#' Grab the schema names from a specific database
#'
#'
#' @family postgresql functions
#' @param host
#' @param user
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add_tables_db("columbiaBike", port=5432)
#' @export

get_list_schema <- function(...){

  #get_connection("deletethis", "postgres", "localhost")
  #RPostgreSQL::dbGetQuery(.connection$con, "select catalog_name,schema_name from information_schema.schemata;")$schema_name

  # This explains the -t
  #https://dba.stackexchange.com/questions/24215/how-to-turn-off-header-only-in-psql-postgresql
  vals <- run_psql_cmd(cmd = "select schema_name from information_schema.schemata;", host, user, dbname,
                       addl_flags = c("-t",  "-c"), intern = TRUE)

  vals <- trimws(vals)
  vals <- vals[vals!=""]

}


# *****************************************************************************
# Get a db schema ---------------------------
# *****************************************************************************



#' Title
#'
#' @return
#' @export
#'
#' @examples
kill_postgres_connections <- function () {

  all_cons <- RPostgreSQL::dbListConnections(PostgreSQL())

  print(all_cons)

  for(con in all_cons)
    +  RPostgreSQL::dbDisconnect(con)

  print(paste(length(all_cons), " connections killed."))

}



# *****************************************************************************
# Truncate tables ---------------------------
# *****************************************************************************

#' Truncate tables in a PostgreSQL database using psql. Originally this was designed
#' for the pesticide project specifically so the default tables to truncate are those
#' six tables, but otherwise this is flexible and accepts db, schema etc.
#'
#' @param dbname database housing tables to truncate
#' @param schema schema with tables to trucate
#' @param whichones tables to trucate
#' @param host host
#' @param user user name
#'
#' @return
#' @export
#'
#' @examples
truncate_postgres_tables <- function(dbname, schema,
                                     whichones = c("chemical", "chemical_groups", "site", "site_groups",
                                                   "chemicals_to_groups", "sites_to_groups"),
                                     host = "localhost", user = "postgres"){

  cmd <- paste(sprintf("TRUNCATE TABLE %s.%s;",schema, whichones), collapse = " ")
  run_psql_cmd(cmd, host, user, dbname)

}


# *****************************************************************************
# Drop tables ---------------------------
# *****************************************************************************

#' Truncate tables in a PostgreSQL database using psql. Originally this was designed
#' for the pesticide project specifically so the default tables to truncate are those
#' six tables, but otherwise this is flexible and accepts db, schema etc.
#'
#' @param dbname database housing tables to truncate
#' @param schema schema with tables to trucate
#' @param whichones tables to trucate
#' @param host host
#' @param user user name
#'
#' @return
#' @export
#'
#' @examples
drop_postgres_tables <- function(dbname, schema,
                                 whichones = c("chemical", "chemical_groups", "site", "site_groups",
                                               "chemicals_to_groups", "sites_to_groups"),
                                 host = "localhost", user = "postgres"){

  cmd <- paste(sprintf("DROP TABLE %s.%s;",schema, whichones), collapse = " ")
  run_psql_cmd(cmd, host, user, dbname)

}





