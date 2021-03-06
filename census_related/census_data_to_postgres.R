#' Download and unzip Census data using the R acs library
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @details
#' Data comes from \url(https://www2.census.gov/geo/tiger/), is downloaded and unzipped
#' @examples
#' add(1, 1)
#' add(10, 1)

library(acs)
library(dplyr)
# api.key.install("c913bb3a3b1069c0bb0ed15a3773819082faf3a9")
# suppressWarnings(library(acs))



# year = 2015
# geolevel = "state"
# table = "B17010"
# span = 5



get_connection<-function(dbname = "testing",
        password = "spatial",
        host="localhost",
        port=5433,
        user="postgres"){
        
        
        # note the double arrow to make global
        .connection<<-try({dplyr::src_postgres(dbname=dbname,
                host=host,
                password=password,
                port=port,
                user=user)}, silent=TRUE)
}


get_connection()






#' Title
#'
#' @param schema name of schema within db
#'
#' @return
#' @export
#'
#' @examples



acs_pg_check_table<-function(schema = "acs_census_tabular", reload = FALSE){
	
	make_schema_public(schema)
	
	tablename <<- paste0("acs_", year, "_", geolevel, "_", span, "yr_", table)

	tableexists <- dplyr::db_has_table(.connection$con, tablename)
	
	if(tableexists & !reload) stop(paste("Table", tablename, "already exists in the database"))
	if(tableexists & reload) dplyr::db_drop_table(con, tablename)
	
	# RPostgreSQL::dbWriteTable(.connection$con, pgtable, dat, row.names = FALSE)
	
}





acs_send_data_to_postgres<-function(){
	
	acs_pg_check_table(reload = TRUE)
	
	# dat<-acs_get_data(year = 2015, geolevel = "county", table = "B17010", span = 5)
	dat<-acs_get_data(tablename)

	RPostgreSQL::dbWriteTable(.connection$con, tablename, dat, row.names = FALSE)
	
	
}


acs_send_data_to_postgres()




#' Title
#'
#' @param year indicates the last year included in the dataset, default 2015
#' @param geolevel geography level (i.e. state, county, tract, blovkgroup), default state
#' @param table name of Census variable (i.e. B17010), default B17010
#' @param span number of years the data should span (i.e. 1, 3 or 5), default 5
#' @return
#' @export
#' @examples
#' acs_get_data(year = 2015, geolevel = "state", table = "B17010", span = 5)


acs_get_data<-function(year = 2015, geolevel = "state", table = "B17010", span = 5){

    # pgtable <<- paste0("acs_", year, "_", geolevel, "_", span, "yr_", table)
      
      
    datstate <- function(){
    	
    	acsgeo<-geo.make(state = "*")
    	acsdat<-acs.fetch(endyear = year, span = span, geography = acsgeo,
    		table.number = table, col.names = "auto")
    	
    	dat<-cbind(acsdat@geography, acsdat@estimate)
    	rownames(dat)<-1:nrow(dat)	
    	dat<-mutate(dat, state = str_pad(state, 2, "left", "0")) %>%
    		rename(GEOID = state) %>%
    		select(GEOID, NAME, everything())
    	
    	return(dat)
    	
    }
    
    
    datcounty <- function(){
    	
    	acsgeo<-geo.make(state = "*", county = "*")
    	acsdat<-acs.fetch(endyear = year, span = span, geography = acsgeo,
    		table.number = table, col.names = "auto")
    	
    	dat<-cbind(acsdat@geography, acsdat@estimate)
    	rownames(dat)<-1:nrow(dat)	
    	dat<-mutate(dat, state = str_pad(state, 2, "left", "0"),
    		county = str_pad(county, 3, "left", "0"),
    		GEOID = paste0(state, county)) %>%
    		select(GEOID, NAME, state, county, everything())
    	
    	return(dat)
    	
    }
    
    
    dattract <- function() {
    	
    	dat <- lapply(fips.state$STATE_NAME, function(state) {
    		print(paste0("Beginning ", state, ": ", Sys.time()))
    		
    		tryCatch({
    			acsgeo <- geo.make(state = state, county = "*", tract = "*")
    			acsdat <- acs.fetch(endyear = year, span = span, 
    				geography = acsgeo, table.number = table, 
    				col.names = "auto")
    			
    			dat <- cbind(acsdat@geography, acsdat@estimate)
    			rownames(dat) <- 1:nrow(dat)
    			dat <- mutate(dat, state = str_pad(state, 2, 
    				"left", "0"), county = str_pad(county, 3, 
    					"left", "0"), tract = str_pad(tract, 6, "left", 
    						"0"), GEOID = paste0(state, county, tract)) %>% 
    				select(GEOID, NAME, state, county, tract, everything())
    		}, error = function(e) {
    			print(paste("No data for", state, sep = ", "))
    			return(NULL)
    		}, warning = function(e) {
    			print(paste("No data for", state, sep = ", "))
    			return(NULL)
    		})
    	})
    	
    	dat <- do.call(rbind, lapply(dat, function(x) x[!is.null(x)]))
    	
    	return(dat)
    }
    
    
    datbg <- function() {
    	dat <- lapply(unique(fips.state$STUSAB)[1:4], function(state) {
    		
    		tryCatch({
    			
    			print(paste0("Beginning ", state, ": ", Sys.time()))
    			st <- filter(fips.county, State == state)
    			
    			dat <- lapply(seq_along(st$County.Name), function(i) {
    				
    				tryCatch({
    					acsgeo <- geo.make(state = state, county = st$County.ANSI[i], 
    						tract = "*", block.group = "*")
    					acsdat <- acs.fetch(endyear = year, span = span, 
    						geography = acsgeo, table.number = table, 
    						col.names = "auto")
    					
    					print(paste(st$County.Name[i], state, sep = ", "))
    					
    					dat <- cbind(acsdat@geography, acsdat@estimate)
    					rownames(dat) <- 1:nrow(dat)
    					dat <- mutate(dat, state = str_pad(state, 2, 
    						"left", "0"), county = str_pad(county, 3, 
    							"left", "0"), tract = str_pad(tract, 6, "left", 
    								"0"), GEOID = paste0(state, county, tract, 
    									blockgroup)) %>% select(GEOID, NAME, state, 
    										county, tract, blockgroup, everything())
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
    
    
    dat <- switch(geolevel,
		state = datstate(),
		county = datcounty(),
		tract = dattract(),
		blockgroup = datbg()
	)
	

	return(dat)

}
	
	


dat<-acs_get_data(year = 2015, geolevel = "state", table = "B17010", span = 5)



	




acs_add_data_to_postgres <- function(filedir, 
        dbname = "testing", 
        schema = "census_geography", 
        reload = FALSE){
        
        

        con <- .connection$con
        make_schema_public(schema)
        
        tablename <- paste0("acs_", year, "_", geolevel, "_", span, "yr_", table)
        
                
        # basename <- gsub(".shp", "", basename(filedir), fixed = TRUE)
        schema_table <- paste(schema, basename, sep = ".")
        tableexists <- dplyr::db_has_table(.connection$con, tablename)
        
        
        if(tableexists & !reload) stop(paste("Table", basename, "already exists in the database"))
        
        if(tableexists & reload){
                dplyr::db_drop_table(con, basename)
        }
        
        
        RPostgreSQL::dbWriteTable(con, tablename, dat, row.names = FALSE)
        
        
        # cmd <- sprintf('ogr2ogr -f "PostgreSQL" PG:"host=192.168.1.4 port=5432 user=postgres dbname=%s password=spatial" "%s" -nln "%s.%s" -nlt PROMOTE_TO_MULTI -lco "GEOM_TYPE=geometry" -lco "GEOMETRY_NAME=geom"',
        #         dbname, filedir, schema, basename)
        # 
        # cat("About to add to database using command\n", cmd)
        # 
        # system(cmd)
        # invisible()
        
}









add_data_to_postgres <- function(){
}
        
        

tableexists <- dplyr::db_has_table(.connection$con, "hollie333")
        
if(tableexists & !reload) stop(paste("Table", basename, "already exists in the database"))

if(tableexists & reload){
        dplyr::db_drop_table(con, basename)
}





make_schema_public <- function(schemaname){
        
        RPostgreSQL::dbSendQuery(.connection$con, paste("CREATE SCHEMA IF NOT EXISTS", schemaname))
        RPostgreSQL::dbSendQuery(.connection$con,
                sprintf("SET search_path = '%s', public", schemaname))
        invisible()
}



make_schema_public("temp333")


	
	var_description<-function(table){
		
		dat_auto<-acs.fetch(endyear = year, span = span,
			geography = geo.make(state = "*"),
			table.number = table, col.names = "auto")
		
		dat_pretty<-acs.fetch(endyear = year, span = span, 
			geography = geo.make(state = "*"),
			table.number = table, col.names = "pretty")
		
		dat<-data.frame(name = table, 
			id = dat_auto@acs.colnames,
			desc = dat_pretty@acs.colnames)
		
		return(dat)
	}

	
		
	
	
	
	
# 	dat<-acs.fetch(endyear = year, span = 5, geography = geo,
# 		table.number = table, col.names = "auto")
# 	
# 	
# 	datgeoid<-switch(type,
# 		state = str_pad(dat@geography$state, 2, "left", pad="0"),
# 		county = paste0(str_pad(dat@geography$state, 2, "left", pad="0"), 
# 			str_pad(dat@geography$county, 3, "left", pad="0")),
# 		tract = paste0(str_pad(dat@geography$state, 2, "left", pad="0"), 
# 			str_pad(dat@geography$county, 3, "left", pad="0"), 
# 			str_pad(dat@geography$tract, 6, "left", pad="0")),
# 		blockgroup = 
# 	)
# 	
# }













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