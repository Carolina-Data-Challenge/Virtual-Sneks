library(dplyr)

durh_clean = read.csv("OpenAQDurham.csv") %>%
  cleaning.function(AQDurham)
  
durh_clean$local = as.POSIXct(durh_clean$local, format="%Y-%m-%dT%H:%M")

cranberry_clean = read.csv("cranberry_allvars.csv") %>%
  cleaning.function()

bryson_clean = read.csv("bryson_allvars.csv") %>%
  cleaning.function(bryson)

greensboro.o3_clean = read.csv("greensboro_o3.csv") %>%
  cleaning.function()
greensboro.pm_clean = read.csv("greensboro_pm.csv") %>% 
  cleaning.function()
greensboro_cleanall = greensboro.pm_clean %>% left_join(greensboro.o3_clean, by = local)

morehead_clean = read.csv("moreheadcity_allvars.csv") %>%
  cleaning.function()

raleigh_clean = read.csv("raleigh.csv") %>%
  cleaning_function()


cleaning.function <- function(df) {
  
  test1 <- df %>% group_by(location, local) %>%
    filter(parameter=="pm10") %>%
    summarise(pm10 = value)
  
  test2 <- df %>% group_by(location, local) %>%
    filter(parameter=="pm25") %>%
    summarise(pm25 = value)
  
  test3 <- df %>% group_by(location, local) %>%
    filter(parameter=="so2") %>%
    summarise(so2 = value)
  
  test4 <- df %>% group_by(location, local) %>%
    filter(parameter=="o3") %>%
    summarise(o3 = value)
  
  test5 <- df %>% group_by(location, local) %>%
    filter(parameter=="co") %>%
    summarise(co = value)
  
  test6 <- df %>% group_by(location, local) %>%
    filter(parameter=="no2") %>%
    summarise(no2 = value)
  
  test7 <- df %>% group_by(location,local) %>% 
    summarise(latitude = mean(latitude), longitude = mean(longitude))
  
  df_final <- merge(test1, test2, by = c("location", "local"), all = TRUE) %>%
    merge(test3, by = c("location", "local"), all = TRUE) %>%
    merge(test4, by = c("location", "local"), all = TRUE) %>%
    merge(test5, by = c("location", "local"), all = TRUE) %>%
    merge(test6, by = c("location", "local"), all = TRUE) %>%
    merge(test7, by = c("location", "local"), all = TRUE)
  
  df_final$local = as.POSIXct(df_final$local, format="%Y-%m-%dT%H:%M")
  
  return(df_final)
}
