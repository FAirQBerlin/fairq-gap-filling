mode <- modules::module({
  
  env <- function() {
    switch(Sys.getenv("MODE", unset = "DEV"),
        DEV = "",
        PROD = "prod_",
    )
  }
  
})
