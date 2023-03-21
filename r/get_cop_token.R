# get_copernicus_authorization_info --------------------------------------------
get_copernicus_authorization_info <- function(refresh_token = NULL)
{
  body_start <- list(client_id = "cdse-public")
  
  body_end <- if (is.null(refresh_token)) {
    
    username <- Sys.getenv("COPERNICUS_LOGIN")
    password <- Sys.getenv("COPERNICUS_PASSWORD")
    
    if (username == "" || password == "") {
      stop(
        "Please set envirionment variables 'COPERNICUS_LOGIN' and ", 
        "'COPERNICUS_PASSWORD' appropriately.", 
        call. = FALSE
      )
    }
    
    list(
      grant_type = "password",
      username = username,
      password = password
    )
    
  } else {
    
    list(
      grant_type = "refresh_token",
      refresh_token = refresh_token
    )
  }
  
  response <- httr::POST(
    url = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token", 
    body = c(body_start, body_end), 
    encode = "form", 
    headers = c("Content-Type" = "application/x-www-form-urlencoded")
  )
  
  httr::content(response)
}

# First time:

access_info <- get_copernicus_authorization_info()
usethis::edit_r_environ()
access_info$access_token
access_info$refresh_token


# After that
access_info <- get_copernicus_authorization_info(
  refresh_token = Sys.getenv("COPERNICUS_REFRESH")
  )
