pacman::p_load(tidyverse, here)

# Carteras

carteras_01 <- read_rds(here("data", "previa", "product_01_bags.rds")) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description)

carteras_02 <- read_rds(here("data", "previa", "product_02_bags.rds")) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description)

carteras_03 <- read_rds(here("data", "previa", "product_03_bags.rds")) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description)

carteras_04 <- read_rds(here("data", "previa", "product_04_bags.rds")) %>% 
  distinct(name, price, brand, type, category, characteristics, sizes, colors, description)

clean_carteras <- carteras_01 %>% 
  bind_rows(carteras_02) %>% 
  bind_rows(carteras_03) %>% 
  bind_rows(carteras_04) %>% 
  mutate(
    
    image_name = name %>%
      str_to_lower() %>%                         # lowercase
      str_replace_all("[^a-z0-9]+", "_") %>%     # replace anything that's not a-z, 0-9 with _
      str_replace_all("_+", "_") %>%             # collapse multiple underscores
      str_remove("^_|_$"),                       # remove leading or trailing underscores
    
    # Paso 1: limpiar price
    price = price %>%
      str_remove_all("\\.") %>% # eliminar puntos
      str_remove_all("\\$") %>% # eliminar $
      str_trim() %>%
      as.numeric(),
    
    # Paso 2: limpiar brand
    
    brand = brand %>%
      str_remove("^by\\s+") %>% # eliminar "by " y espacios
      str_squish(), # eliminar espacios extra
    
    # Paso 3: crear uruguayan_made y sustainable
    
    uruguayan_made = if_else(str_detect(characteristics, regex("Hecho en Uruguay", ignore_case = TRUE)), 1, 0),
    sustainable = if_else(str_detect(characteristics, regex("Sustentable", ignore_case = TRUE)), 1, 0)
  ) %>%
    distinct(name, price, brand, type, category, characteristics, sizes, colors, description, uruguayan_made, sustainable) %>%
    select(- characteristics, - description) %>% 
  filter(!(name %in% c("Short Slouchy Leather Jacket - Black",  
                       "Short Slouchy Leather Jacket - Gold", 
                       "Short Slouchy Leather Jacket - Silver", 
                       "Short Slouchy Leather Jacket - Suela", 
                       "Slouchy Leather Jacket - Black",     
                       "Slouchy Leather Jacket - Camel",     
                       "Slouchy Leather Jacket - Gold",     
                       "Slouchy Leather Jacket - Silver",      
                       "Slouchy Leather Jacket - Suela",   
                       "Slouchy Leather Pant - Black",     
                       "Slouchy Leather Pant - Camel",     
                       "Slouchy Leather Pant - Nude",          
                       "Slouchy Leather Pant - Oliva",     
                       "Slouchy Leather Pant - Silver",    
                       "Slouchy Leather Pant - Suela",     
                       "Slouchy Leather Pant - Verde Inglés",  
                       "Slouchy Leather Pant Bordeaux",    
                       "Top Rocket  Lurex & Leather",        
                       "Top Rocket Lurex & Leather Gold",
                       "Leather Country Vest",                 
                       "Leather Mini Flecos",
                       "Leather Pant Gold",
                       "Leather Top Camel",
                       "Leather Top Gold",                     
                       "Leather Top Negro",
                       "Leather Top Silver",
                       "Leather Vest Tachas",
                       "Leather Vest Tachas Camel",            
                       "Leather Vest Tachas Gold",
                       "Leather Vest Tachas Silver",
                       "Leather Vest Tachas Suela",
                       "Maxi Leather Skirt Black",             
                       "Mini Leather Skirt",
                       "Mini Leather Skirt Camel",
                       "Mini Leather Skirt Gold",
                       "Mini Leather Skirt Silver",
                       "New - Slouchy Leather Pant Chocolate",
                       "Roll Leather Jacket - Black",
                       "Elvis Leather Jacket Black",
                       "Gabardina Leather Jacket - Camel",
                       "Gabardina Leather Jacket - Chocolate" , 
                       "Gabardina Leather Jacket - Suela",     
                       "House Shoe",
                       "Leather Cargo  Pant - Black",
                       "Leather Cargo Pant - Suela",
                       "Country Leather Jacket",
                       "Cow Print Leather Vest",
                       "Aw Leather Cap",
                       "Cangaroo Leather & Cotton",
                       "Botitas"))) %>% 
  mutate(category = case_when(
    name == "Arar Bag" ~ "Carteras",
    name == "Bandolera Ginebra" ~ "Bandoleras",
    name == "Bandolera Samba"  ~ "Bandoleras",                     
    name == "Bolso Bucket" ~ "Bolsos",
    name == "Bolso Clarea" ~ "Bolsos",
    name == "Bolso Leather Bag Chocolate" ~ "Bolsos",
    name == "Cartera Madoz" ~ "Carteras",
    name == "Cartera Martha" ~ "Carteras",
    name == "Cartera Rosilla" ~ "Carteras",                      
    name == "Chic Bag Cow Print" ~ "Carteras",
    name == "Dolly Carry Bag" ~ "Bolsos",                      
    name == "Leather City Bag" ~ "Bolsos",                     
    name == "Leather City Bag Burgundy" ~ "Bolsos",
    name == "Leather Convertible Backpack" ~ "Mochilas",
    name == "Leather Convertible Backpack Black" ~ "Mochilas",
    name == "Mochila Brook" ~ "Mochilas",
    name == "Mochila Escala" ~ "Mochilas",
    name == "Mochila Malva" ~ "Mochilas",
    name == "Mochila Nikka" ~ "Mochilas",
    name == "Mochila Senna" ~ "Mochilas",
    name == "Mochila Toscana" ~ "Mochilas",
    name == "Riñonera Dual" ~ "Riñoneras",
    name == "Riñonera Enjambre" ~ "Riñoneras",
    name == "Riñonera Maui" ~ "Riñoneras",
    name == "Sembra Bag" ~ "Carteras",                           
    name == "Tote Elegante" ~ "Carteras",                        
    name == "Tote Urbana" ~ "Carteras",
    .default = category
    
  ))

# Paso 4: separar colors
colors_split <- clean_carteras$colors %>%
  map(~ .x %>%
        str_split("\\n") %>%
        unlist() %>%
        str_trim() %>%
        discard(~ .x == ""))

# ¿Cuál es el máximo número de colores en una fila?
max_colors <- max(map_int(colors_split, length))

# Crear columnas color_1, color_2, ..., color_n
colors_df <- colors_split %>%
  map(~ c(.x, rep(NA, max_colors - length(.x)))) %>%
  map_dfr(~ set_names(as.list(.x), paste0("color_", 1:max_colors)))

# Paso 5: 
#### - Juntar todo,
#### - Eliminar filas repetidas
final_data <- clean_carteras %>%
  select(-sizes, -colors) %>% # borrar las originales
  bind_cols(colors_df) %>%
  distinct(
    name, price, brand, type, category, uruguayan_made, sustainable,
    color_1, color_2, color_3, color_4, color_5, color_6, color_7, color_8,
    color_9, color_10, color_11, color_12
  )     

write_rds(final_data, here("data", "previa", "clean", "product_clean_bags.rds"))
