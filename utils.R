library(sf)

generate_buffered_country_boundary <- function(
    shape_path,
    file_name = "Country_Shapefile_Buffer_10km.shp"
){
  #' Produce buffered country boundary shapefile by dissolving EA geometries and
  #' adding 10km buffer
  #' 
  ea_geoms <- st_read(file.path(shape_path, "2018_MPHC_EAs_Final_for_Use.shp"))
  country_buffer <- ea_geoms %>% 
    st_union() %>% 
    st_buffer(10E3)
  st_write(country_buffer, file.path(shape_path, file_name))
  return(country_buffer)
}
