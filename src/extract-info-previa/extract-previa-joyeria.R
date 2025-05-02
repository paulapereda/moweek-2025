scrape_category <- function(category_path, type_label, file_stub) {
  
  # Set the base URL
  base_url <- "https://moweek.com.uy"
  
  # Read the main page
  main_page <- read_html(base_url)
  
  # Get main category links (Vestimenta, Calzado, etc.)
  main_categories <- main_page %>%
    html_nodes(".headerOptions a.headerOption") %>%
    html_attr("href")
  
  # Function to get subcategory links for a main category
  get_subcategories <- function(main_categories) {
    
    full_url <- paste0(base_url, main_categories)
    cat_page <- read_html(full_url)
    
    cat_page %>%
      html_nodes(".expandedCategory a.categoryLevelTwoTitle") %>%
      html_attr("href") %>%
      unique()
  }
  
  # Build full subcategories list
  subcategory_links <- map(main_categories, get_subcategories) %>%
    flatten_chr() %>%
    unique() %>%
    str_replace("/1$", "/50")
  
  subcategory_links <- subcategory_links[str_starts(subcategory_links, category_path)]
  
  subcategory_links <- subcategory_links[!(subcategory_links %in% c(
    
    "/joyeria-y-bijou/50",
    "/joyeria-y-bijou/1?filters=170",
    "/joyeria-y-bijou/1?filters=171"
    
  ))]
  
  product_data <- tibble()
  for (url in main_categories) {
    cat_url <- paste0(base_url, url)
    page <- read_html(RETRY("GET", cat_url) %>% content("text"))
    cat_name <- html_text(html_node(page, "title")) %>%
      str_to_title() %>%
      str_remove(" - Moweek - Encontrá Lo Mejor De La Moda Local")
    
    for (tag in html_nodes(page, ".productViewContainer")) {
      tryCatch(
        {
          name <- html_text(html_node(tag, ".productViewName"))
          price <- html_text(html_node(tag, ".productViewPrice"))
          product_url <- html_attr(tag, "href")
          
          product_page <- read_html(RETRY("GET", paste0(base_url, product_url)) %>% content("text"))
          
          brand <- html_nodes(product_page, ".productInfoTitle.brandName") %>%
            html_text() %>%
            paste(collapse = ", ") %>%
            str_trim()
          
          characteristics <- html_nodes(product_page, "#cocardasContainer div[class= 'filterCocarda shown']") %>%
            html_text() %>%
            paste(collapse = ", ") %>%
            str_trim()
          
          sizes <- html_nodes(product_page, ".specGroupTitle:contains('Talle') + .specs") %>%
            html_text() %>%
            paste(collapse = ", ") %>%
            str_trim()
          
          colors <- html_nodes(product_page, ".specGroupTitle:contains('Color') + .specs") %>%
            html_text() %>%
            paste(collapse = ", ") %>%
            str_trim()
          
          description <- html_nodes(product_page, ".productGroupInfo") %>%
            html_text() %>%
            paste(collapse = ", ") %>%
            str_trim()
          
          product_data <- bind_rows(product_data, tibble(
            name = str_trim(name),
            price = str_trim(price),
            brand = brand,
            type = type_label,
            category = cat_name,
            characteristics = characteristics,
            sizes = sizes,
            colors = colors,
            description = description
  
           ))
        },
        error = function(e) {
          message("Error at product: ", product_url, " | ", e$message)
        }
      )
    }
  }
  
  write_rds(product_data, here("data", "previa", paste0("product_06_", file_stub, ".rds")))
}

scrape_category("/joyeria-y-bijou/", "Joyería y bijou", "jewelry")

