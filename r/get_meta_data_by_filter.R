saving_path <- file.path(
  "D:/06_git/", 
  "ad4gd_lakes/r/search_meta_data"
)

if(FALSE){
  meta_request <- build_meta_data_request(
    collection = "SENTINEL-2", 
    tBeg = "2022-04-20T00:00:00.000Z", 
    tEnd = "2022-05-30T00:00:00.000Z", 
    lat_lon_polygon = c(
      52.536296452480926, 13.306371130791012,
      52.54442040411344, 13.348659604234282,
      52.5571841917795, 13.33164980325073,
      52.55279838139395, 13.290245175027593
    ))
  
  filename <- "ploetze"
  download.file(
    url = meta_request, 
    destfile = file.path(saving_path, paste0(filename, ".json")))
}

# functions --------------------------------------------------------------------

build_meta_data_request <- function(
  program_name = NULL, 
  collection = NULL,
  tBeg = NULL, 
  tEnd = NULL,
  lat_lon_polygon = NULL,
  max_cloud = NULL
){
 
  if(!is.null(program_name))
    program_filter <- paste0("contains(Name,'", program_name, "')")
  if(!is.null(collection))
    collection_filter <- paste0("Collection/Name eq '", collection, "'")
  if(!is.null(tBeg))
    start_filter <- paste("ContentDate/Start gt", tBeg)
  if(!is.null(tEnd))
    end_filter <- paste("ContentDate/Start lt", tEnd)
  if(!is.null(lat_lon_polygon)){
    polygon_matrix <- matrix(
      data = lat_lon_polygon, 
      ncol = 2, 
      byrow = TRUE
    )
    # close polygon
    polygon_matrix <- rbind(polygon_matrix, polygon_matrix[1,])
    
    
    lake_polygon <- paste(apply(polygon_matrix, MARGIN = 1, function(x){
      paste(x[2], x[1])
    }), collapse = ",")
    polygon_filter <-
      paste0("OData.CSC.Intersects(area=geography'SRID=4326;POLYGON((",
             lake_polygon, "))')")
  }
 

  # max_cloud_cover (funktioniert noch nicht)
  if(!is.null(max_cloud)){
    cloud_filter <- paste0(
      "Attributes/OData.CSC.DoubleAttribute/any(",
      "att:att/Name eq 'cloudCover'", " and",
      "att/OData.CSC.DoubleAttribute/Value le", max_cloud,
      ")"
    )
  }
 
  
  all_filters <- paste(c(collection_filter, polygon_filter, start_filter, end_filter), 
        collapse = " and ")
  
  paste0(
    "https://catalogue.dataspace.copernicus.eu/odata/v1/Products?$filter=",
    all_filters
  )
}
