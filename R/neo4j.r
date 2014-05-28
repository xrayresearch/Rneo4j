####################################################################
# Neo4j R Client
# Programmer: Vera Friederichs
# Purpose: interface with Neo4j
# Date: 2014-05-18
require(bitops)
require(RCurl)
require(RJSONIO)
#'@import bitops
#'@import RCurl
#'@import RJSONIO


#' @title Connection to Neo4j
#' @description Creates a connection object on the host and ports provided
#' @param host The IP address of the Neo4J instance
#' @param port The port to connect to
#' @examples \dontrun{ 
#'    myConn <- neo4j_http_connection(host="localhost");
#' } 
#' @export
neo4j_http_connection <- function(host, port=7474) {
  
  conn <- list(neo4j_http_host = host, neo4j_http_port = port);
  class(conn) <- "neo4j_connection"
  conn
  
}


neo4j_base_url <- function(conn) {
  
  base_url <- paste0("http://", conn$neo4j_http_host, ":", conn$neo4j_http_port, "/db/data/cypher")
  base_url
  
}


#' @title Ping connection
#' @description Check connection to the database.
#' @param conn A neo4j connection object.
#' @export
neo4j_ping <- function(conn) {
  url.exists(conn$neo4j_http_host)
  
}

#' @title Send queries
#' @description Send queries to db. Result is a raw JSON list.
#' @param con A neo4j connection object.
#' @param querystring The string in double qotes to be passed on to neo4j.
#' @export
neo4j_query_raw <- function(con, querystring) {
  h = basicTextGatherer()
  curlPerform(url=neo4j_base_url(con),
              postfields=paste('query',curlEscape(querystring), sep='='),
              writefunction = h$update,
              verbose = FALSE
  )
  result <- fromJSON(h$value())
  if(!is.null(result$message)) print(result$message)
  result
}

#' @title Send queries
#' @description Send queries to db. Result is a dataframe.
#' @param con A neo4j connection object.
#' @param querystring The string in double qotes to be passed on to neo4j.
#' @export
neo4j_query_df <- function(con, querystring) {
  h = basicTextGatherer()
  curlPerform(url=neo4j_base_url(con),
              postfields=paste('query',curlEscape(querystring), sep='='),
              writefunction = h$update,
              verbose = FALSE
  )
  result <- fromJSON(h$value())
  if(!is.null(result$message)) print(result$message)
  data <- data.frame(t(sapply(result$data, unlist)))
  names(data) <- result$columns
  data
}
#data <- query("START n = node(*) RETURN COUNT(n) AS count")

#results <- data.frame(table(data$count))
