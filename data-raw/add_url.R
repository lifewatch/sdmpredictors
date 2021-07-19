# add download url
# repeat for layers_future.csv and layers_paleo.csv
df <- read.csv2("./data-raw/layers.csv")
df$layer_url <- NA

for (i in 1:nrow(df)){
  if(df$dataset_code[i] != "Bio-ORACLE"){
    df$layer_url[i] = paste0("https://www.lifewatch.be/sdmpredictors/", df$layer_code[i], "_lonlat.tif")
  }else if(df$dataset_code[i] == "Bio-ORACLE"){
    if(df$version[i] == 1){
      df$layer_url[i] = paste0("https://bio-oracle.org/data/1.0/", df$layer_code[i], "_lonlat.zip")
    }else if(df$version[i] == 2.0){
      df$layer_url[i] = paste0("https://bio-oracle.org/data/2.0/", sdm_to_bo(df$layer_code[i]))
    }else if(df$version[i] == 2.1){
      df$layer_url[i] = paste0("https://bio-oracle.org/data/2.1/", sdm_to_bo(df$layer_code[i]))
    }else{
      stop(paste0("Traceback: ", df$layer_code[i]))
    }
  }
}

View(df)

write.csv2(df, "./data-raw/layers.csv", na = NA_character_, 
           row.names = FALSE, fileEncoding = "UTF-8", 
           quote = FALSE)


