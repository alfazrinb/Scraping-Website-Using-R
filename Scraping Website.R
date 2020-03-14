library(rvest)
library(furrr)
library(tidyverse)
library(tidyr)

mobil_scrap <- function(x){
  carmudi <- read_html(x)
  car_id <- 
    carmudi %>% 
    html_nodes(".title-blue") %>% 
    html_text() %>% 
    enframe(name = "id", value = "car")
  
  harga <- 
    carmudi %>% 
    html_nodes(".price a") %>% 
    html_text() %>% 
    enframe(name = "id", value = "pricw")
  
  car_profile <- 
    carmudi %>%
    {
      tibble(
        value = html_nodes(., ".medium-3 span") %>%
          html_text(),
        parameter = rep(
          c("distance", "transmision", "fuel", "city"),
          times = length(value) / 4
        ),
        id = rep(seq_len(length(value) / 4), each = 4)
      )
    } %>%
    pivot_wider(names_from = parameter, values_from = value)
  
  car_id %>% 
    left_join(harga) %>% 
    mutate(harga = parse_number(pricw)) %>% 
    left_join(car_profile) %>% 
    mutate(
      car = str_remove_all(car, "\\n|\\s$"),
      distance = parse_number(distance),
      city = str_remove_all(city, "\\n|\\s$")
    ) %>% 
    extract(car, into = c("year", "car"), regex = "(\\d{4})\\s(.*)")
  
}
mobil_scrap(x = "https://www.carmudi.co.id/cars/used/?page=2")

op_url <- paste0("https://www.carmudi.co.id/cars/used/?page=", seq_len(1000))
op_raw <- 
  future_map_dfr(op_url, possibly(mobil_scrap, otherwise= NULL),
                 .progress = TRUE)
write.csv(op_raw, "D:\\Alfa\\Skripsi\\Flask\\mobil.csv")


# Manual
carmudi <- read_html("https://www.carmudi.co.id/cars/used/?page=1")
car_id <- 
  carmudi %>% 
  html_nodes(".title-blue") %>% 
  html_text() %>% 
  enframe(name = "id", value = "car")

harga <- 
  carmudi %>% 
  html_nodes(".price a") %>% 
  html_text() %>% 
  enframe(name = "id", value = "pricw")

car_profile <- 
  carmudi %>%
  {
    tibble(
      value = html_nodes(., ".medium-3 span") %>%
        html_text(),
      parameter = rep(
        c("distance", "transmision", "fuel", "city"),
        times = length(value) / 4
      ),
      id = rep(seq_len(length(value) / 4), each = 4)
    )
  } %>%
  pivot_wider(names_from = parameter, values_from = value)

car_id %>% 
  left_join(harga) %>% 
  mutate(harga = parse_number(pricw)) %>% 
  left_join(car_profile) %>% 
  mutate(
    car = str_remove_all(car, "\\n|\\s$"),
    distance = parse_number(distance),
    city = str_remove_all(city, "\\n|\\s$")
  ) %>% 
  extract(car, into = c("year", "car"), regex = "(\\d{4})\\s(.*)")
View(car_id)
