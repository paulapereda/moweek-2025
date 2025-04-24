pacman::p_load(tidyverse, rvest, httr)

category_path <- "/vestimenta/"
type_label <- "Vestimenta"
file_stub <- "clothing"

scrape_category <- function(category_path, type_label, file_stub) {
  base_url <- "https://moweek.com.uy"

  # Load page and get matching category URLs
  webpage <- read_html(GET(base_url) %>% content("text"))
  category_urls <- html_nodes(webpage, ".expandedCategory a") %>%
    html_attr("href") %>%
    str_subset(category_path) %>%
    str_replace_all("1", "100") %>%
    discard(~ .x %in% c(
      paste0(category_path, "100"),
      paste0(category_path, "100?filters=10070"),
      paste0(category_path, "100?filters=1007100")
    ))

  product_data <- tibble()
  for (url in category_urls) {
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

  write_rds(product_data, here("data", "previa", paste0("product_", file_stub, ".rds")))
}

scrape_category("/vestimenta/", "Vestimenta", "clothing")
scrape_category("/calzado/", "Calzado", "shoes")
scrape_category("/carteras-y-bolsos/", "Carteras y bolsos", "bags")
scrape_category("/joyeria-y-bijou/", "Joyería y bijou", "jewelry")
scrape_category("/accesorios/", "Accesorios", "accesories")
