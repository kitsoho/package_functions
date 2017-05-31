library(RPostgreSQL)
library(acs)
library(dplyr)




# Quick summary of functions ---------------------------
# get_connection(): connects to the user-defined postgres database (Zev)
# make_schema_public(): creates a schema and makes public (Zev)
# acs_table_check(): checks to see if table exists in postgres
# acs_retrieve_data: grabs data from ACS using user-defined settings
# acs_send_data_postgres: sends the ACS data to postgres db




# get_connection() ---------------------------
#' Title
#'
#' @param dbname 
#' @param password 
#' @param host 
#' @param port 
#' @param user 
#'
#' @return
#' @export
#'
#' @examples

get_connection <- function(
	dbname = "testing",
	password = "spatial",
	host = "localhost",
	port = 5433,
	user = "postgres"){
	
	
	# note the double arrow to make global
	.connection <<- try({dplyr::src_postgres(dbname = dbname,
		host = host,
		password = password,
		port = port,
		user = user)}, silent = TRUE)
}




# make_schema_public() ---------------------------
#' Title
#'
#' @param schemaname 
#'
#' @return
#' @export
#'
#' @examples

make_schema_public <- function(schemaname){
	
	RPostgreSQL::dbSendQuery(.connection$con, paste("CREATE SCHEMA IF NOT EXISTS", schemaname))
	RPostgreSQL::dbSendQuery(.connection$con,
		sprintf("SET search_path = '%s', public", schemaname))
	# schemaname <<- schemaname
	invisible()
}





# acs_table_check() ---------------------------

acs_table_check <- function(tablename, reload = FALSE){
	
	tableexists <- dplyr::db_has_table(.connection$con, tablename)
	
	if(tableexists & !reload) stop(paste("Table", tablename, "already exists in the database"))
	
	if(tableexists & reload){
		n <- readline("Are you sure you want to drop the existing table: Y or N?")
		
		n <- as.character(unlist(strsplit(n, " or ")))
		n <- tolower(n)
		
		if (n == "y") dplyr::db_drop_table(.connection$con, tablename) 
		if (n == "n") stop(paste("Table", tablename, "already exists in the database"))

	}
	
}




acs_fetch_data <- function(acsgeo){
	acs::acs.fetch(
		endyear = year, 
		span = span, 
		geography = acsgeo,
		table.number = table, 
		col.names = "auto")
}



# acs_retrieve_data() ---------------------------
# year = 2015, 
# geolevel = "state",
# span = 5,
# table ="B01003"

acs_retrieve_dat <- function(
	year = NULL, 
	geolevel = NULL,
	span = 5,
	table = NULL){
	
	if (is.null(year)) stop("Year must be supplied")
	if (is.null(geolevel)) stop("Geolevel must be supplied (i.e. state, county, tract or blockgroup)")
	if (!span %in% c(1, 3, 5)) stop("Span can be 1, 3 or 5. Default is 5.")
	if (is.null(table)) stop("Table id must be supplied (i.e. B01003 for Total Population)")
	
	tablename <- paste0("acs_", year, "_", geolevel, "_", span, "yr_", table)

	
	# Prior to getting the data check if the table exists
	acs_table_check(tablename, reload = TRUE)
	

	fips.state <- acs::fips.state
	fips.county <- acs::fips.county
	
	
	datstate <- function(){
		
		acsgeo <- acs::geo.make(state = "*")
		acsdat<-acs_fetch_data(acsgeo)
		
		dat <- cbind(acsdat@geography, acsdat@estimate)
		rownames(dat) <- 1:nrow(dat)	
		
		dat <- dplyr::mutate(dat, 
			state = stringr::str_pad(state, 2, "left", "0")) %>%
			dplyr::rename(GEOID = state) %>%
			dplyr::select(GEOID, NAME, dplyr::everything())
		
		return(dat)
		
	}
	
	
	datcounty <- function(){
		
		acsgeo <- acs::geo.make(state = "*", county = "*")
		acsdat <- acs_fetch_data(acsgeo)
		
		dat <- cbind(acsdat@geography, acsdat@estimate)
		rownames(dat) <- 1:nrow(dat)	
		
		dat <- dplyr::mutate(dat, 
			state = stringr::str_pad(state, 2, "left", "0"),
			county = stringr::str_pad(county, 3, "left", "0"),
			GEOID = paste0(state, county)) %>%
			dplyr::select(GEOID, NAME, state, county, dplyr::everything())
		
		return(dat)
		
	}
	
	

	
	dattract <- function() {
		
		dat <- lapply(acs::fips.state$STATE_NAME, function(state) {
			print(paste0("Beginning ", state, ": ", Sys.time()))
			
			tryCatch({
				acsgeo <- acs::geo.make(state = state, county = "*", tract = "*")
				acsdat <- acs_fetch_data(acsgeo)
				
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
	
	
	datbg <- function() {
		dat <- lapply(fips.state$STUSAB[10:12], function(state) {
			
			tryCatch({
				
				print(paste0("Beginning ", state, ": ", Sys.time()))
				st <- dplyr::filter(fips.county, State == state)
				
				dat <- lapply(seq_along(st$County.Name), function(i) {
					
					tryCatch({
						acsgeo <- acs::geo.make(
							state = state, 
							county = st$County.ANSI[i], 
							tract = "*", 
							block.group = "*")
						acsdat <- acs_fetch_data(acsgeo)
						
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
	
	
	dat <- switch(geolevel,
		state = datstate(),
		county = datcounty(),
		tract = dattract(),
		blockgroup = datbg()
	)
	
	
	return(dat)
	
}


# acs_retrieve_dat <- function(tablename = tablename){
# 	
# 	year <- as.numeric(sapply(strsplit(tablename, "_"), "[", 2))
# 	geolevel <- sapply(strsplit(tablename, "_"), "[", 3)
# 	span <- as.numeric(gsub("yr", "", sapply(strsplit(tablename, "_"), "[", 4)))
# 	table <- sapply(strsplit(tablename, "_"), "[", 5)
# 	
# 	
# 	datstate <- function(){
# 		
# 		acsgeo<-geo.make(state = "*")
# 		acsdat<-acs.fetch(endyear = year, span = span, geography = acsgeo,
# 			table.number = table, col.names = "auto")
# 		
# 		dat<-cbind(acsdat@geography, acsdat@estimate)
# 		rownames(dat)<-1:nrow(dat)	
# 		dat<-mutate(dat, state = str_pad(state, 2, "left", "0")) %>%
# 			rename(GEOID = state) %>%
# 			select(GEOID, NAME, everything())
# 		
# 		return(dat)
# 		
# 	}
# 	
# 	
# 	datcounty <- function(){
# 		
# 		acsgeo<-geo.make(state = "*", county = "*")
# 		acsdat<-acs.fetch(endyear = year, span = span, geography = acsgeo,
# 			table.number = table, col.names = "auto")
# 		
# 		dat<-cbind(acsdat@geography, acsdat@estimate)
# 		rownames(dat)<-1:nrow(dat)	
# 		dat<-mutate(dat, state = str_pad(state, 2, "left", "0"),
# 			county = str_pad(county, 3, "left", "0"),
# 			GEOID = paste0(state, county)) %>%
# 			select(GEOID, NAME, state, county, everything())
# 		
# 		return(dat)
# 		
# 	}
# 	
# 	
# 	dattract <- function() {
# 		
# 		dat <- lapply(fips.state$STATE_NAME, function(state) {
# 			print(paste0("Beginning ", state, ": ", Sys.time()))
# 			
# 			tryCatch({
# 				acsgeo <- geo.make(state = state, county = "*", tract = "*")
# 				acsdat <- acs.fetch(endyear = year, span = span, 
# 					geography = acsgeo, table.number = table, 
# 					col.names = "auto")
# 				
# 				dat <- cbind(acsdat@geography, acsdat@estimate)
# 				rownames(dat) <- 1:nrow(dat)
# 				dat <- mutate(dat, state = str_pad(state, 2, 
# 					"left", "0"), county = str_pad(county, 3, 
# 						"left", "0"), tract = str_pad(tract, 6, "left", 
# 							"0"), GEOID = paste0(state, county, tract)) %>% 
# 					select(GEOID, NAME, state, county, tract, everything())
# 			}, error = function(e) {
# 				print(paste("No data for", state, sep = ", "))
# 				return(NULL)
# 			}, warning = function(e) {
# 				print(paste("No data for", state, sep = ", "))
# 				return(NULL)
# 			})
# 		})
# 		
# 		dat <- do.call(rbind, lapply(dat, function(x) x[!is.null(x)]))
# 		
# 		return(dat)
# 	}
# 	
# 	
# 	datbg <- function() {
# 		dat <- lapply(unique(fips.state$STUSAB), function(state) {
# 			
# 			tryCatch({
# 				
# 				print(paste0("Beginning ", state, ": ", Sys.time()))
# 				st <- filter(fips.county, State == state)
# 				
# 				dat <- lapply(seq_along(st$County.Name), function(i) {
# 					
# 					tryCatch({
# 						acsgeo <- geo.make(state = state, county = st$County.ANSI[i], 
# 							tract = "*", block.group = "*")
# 						acsdat <- acs.fetch(endyear = year, span = span, 
# 							geography = acsgeo, table.number = table, 
# 							col.names = "auto")
# 						
# 						print(paste(st$County.Name[i], state, sep = ", "))
# 						
# 						dat <- cbind(acsdat@geography, acsdat@estimate)
# 						rownames(dat) <- 1:nrow(dat)
# 						dat <- mutate(dat, state = str_pad(state, 2, 
# 							"left", "0"), county = str_pad(county, 3, 
# 								"left", "0"), tract = str_pad(tract, 6, "left", 
# 									"0"), GEOID = paste0(state, county, tract, 
# 										blockgroup)) %>% select(GEOID, NAME, state, 
# 											county, tract, blockgroup, everything())
# 					}, error = function(e) {
# 						print(paste0("No data for ", st$County.Name[i], 
# 							", ", state))
# 						return(NULL)
# 					}, warning = function(e) {
# 						print(paste0("No data for ", st$County.Name[i], 
# 							", ", state))
# 						return(NULL)
# 					})
# 					
# 				})
# 				
# 				dat <- do.call(rbind, lapply(dat, function(x) x[!is.null(x)]))
# 				return(dat)
# 				
# 			}, error = function(e) {
# 				print(paste0("No data for ", state))
# 				return(NULL)
# 			}, warning = function(e) {
# 				print(paste0("No data for ", state))
# 				return(NULL)
# 			})
# 			
# 		})
# 		
# 		dat <- do.call(rbind, lapply(dat, function(x) x[!is.null(x)]))
# 		return(dat)
# 	}
# 	
# 	
# 	dat <- switch(geolevel,
# 		state = datstate(),
# 		county = datcounty(),
# 		tract = dattract(),
# 		blockgroup = datbg()
# 	)
# 	
# 	
# 	return(dat)
# 	
# }




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








#' Title
#'
#' @param schema name of schema within db
#'
#' @return
#' @export
#'
#' @examples



acs_table_check<-function(schema = "acs_census_tabular", tablename = tablename, reload = FALSE){
	
	make_schema_public(schema)

	tableexists <- dplyr::db_has_table(.connection$con, tablename)
	
	if(tableexists & !reload) stop(paste("Table", tablename, "already exists in the database"))
	if(tableexists & reload) dplyr::db_drop_table(con, tablename)
	
	# RPostgreSQL::dbWriteTable(.connection$con, pgtable, dat, row.names = FALSE)
	
}





acs_send_data_to_postgres<-function(year = NULL, geolevel = NULL, span = NULL, table = NULL){
	
	tablename <- paste0("acs_", year, "_", geolevel, "_", span, "yr_", table)
	
	acs_pg_check_table(tablename = tablename)
	
	# dat<-acs_get_data(year = 2015, geolevel = "county", table = "B17010", span = 5)
	dat<-acs_get_data(tablename)

	RPostgreSQL::dbWriteTable(.connection$con, tablename, dat, row.names = FALSE)
	
	
}


acs_send_data_to_postgres(year = 2011, geolevel = "blockgroup", span = 5, table = "B17010")




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


acs_get_data<-function(tablename = tablename){
	
	year <- as.numeric(sapply(strsplit(tablename, "_"), "[", 2))
	geolevel <- sapply(strsplit(tablename, "_"), "[", 3)
	span <- as.numeric(gsub("yr", "", sapply(strsplit(tablename, "_"), "[", 4)))
	table <- sapply(strsplit(tablename, "_"), "[", 5)
	

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
    	dat <- lapply(unique(fips.state$STUSAB), function(state) {
    		
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
	
	


# dat<-acs_get_data(year = 2015, geolevel = "state", table = "B17010", span = 5)







acs_create_table_varnames<-function(){
	
	acsgeo<-geo.make(state = "*")
	acsdat<-acs.fetch(endyear = year, span = span, geography = acsgeo,
		table.number = table, col.names = "auto")
	
}



	




make_schema_public <- function(schemaname){
        
        RPostgreSQL::dbSendQuery(.connection$con, paste("CREATE SCHEMA IF NOT EXISTS", schemaname))
        RPostgreSQL::dbSendQuery(.connection$con,
                sprintf("SET search_path = '%s', public", schemaname))
        invisible()
}




		
	
	









