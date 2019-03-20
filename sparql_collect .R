library(dplyr)
library(SPARQL)




# Load Open Data Platform data into memory

endpoint <- 'http://statistics.gov.scot/sparql'

query <-"PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> 
select ?refArea ?refPeriod ?measureType ?industrySectorsic07 ?value
where { ?data qb:dataSet <http://statistics.gov.scot/data/gross-domestic-product-quarterly-output-by-industry>. 
?data <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?refAreaURI.
?refAreaURI rdfs:label ?refArea.
?data <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?refPeriodURI.
?refPeriodURI rdfs:label ?refPeriod.
?data <http://purl.org/linked-data/cube#measureType> ?measureTypeURI.
?measureTypeURI rdfs:label ?measureType.
?data <http://statistics.gov.scot/def/dimension/industrySector(sic07)> ?industrySectorsic07URI.
?industrySectorsic07URI rdfs:label ?industrySectorsic07. 
?data ?measureTypeURI ?value. }"

gdp_raw <- SPARQL(endpoint,query)$results

current_quarter <- gdp_raw$refPeriod %>%
  last()

