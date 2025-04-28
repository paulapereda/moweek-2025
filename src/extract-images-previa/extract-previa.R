# Load required libraries
pacman::p_load(tidyverse, rvest, httr, fs)

# Set the base URL
base_url <- "https://moweek.com.uy"

# Read the main page
main_page <- read_html(base_url)

# Get main category links (Vestimenta, Calzado, etc.)
main_categories <- main_page %>%
  html_nodes(".headerOptions a.headerOption") %>%
  html_attr("href") %>%
  discard(is.na) 

# Function to get subcategory links for a main category
get_subcategories <- function(category_url) {
  
  full_url <- paste0(base_url, category_url)
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

subcategory_links <- subcategory_links[!(subcategory_links %in% c(
  
  # Vestimenta
  "/vestimenta/50", 
  "/vestimenta/activewear/50",
  "/vestimenta/bodywear/50",
  "/vestimenta/chalecos-y-kimonos/50",
  "/vestimenta/leggings-y-bikers/50",
  "/vestimenta/lenceria/50",
  "/vestimenta/pijamas-y-camisones/50",
  "/vestimenta/trajes-de-bano/50",
  "/vestimenta/hombres/50",
  "/vestimenta/ninos/50",
  "/vestimenta/1?filters=170",
  "/vestimenta/1?filters=171",
  
  # Calzado
  "/calzado/50",
  "/calzado/ninos/50",
  "/calzado/hombres/50",
  "/calzado/1?filters=170",
  "/calzado/1?filters=171",
  
  # Carteras y bolsos
  "/carteras-y-bolsos/50",
  "/carteras-y-bolsos/1?filters=170",
  "/carteras-y-bolsos/1?filters=171",
  
  # Joyería y bijou
  "/joyeria-y-bijou/50",
  "/joyeria-y-bijou/1?filters=170",
  "/joyeria-y-bijou/1?filters=171",
  
  
  # Accesorios
  "/accesorios/50", 
  "/accesorios/billeteras/50",
  "/accesorios/bufandas/50",                  
  "/accesorios/cinturones/50",
  "/accesorios/deco/50",
  "/accesorios/espejos/50",                   
  "/accesorios/guantes/50",
  "/accesorios/lentes-de-receta/50",
  "/accesorios/lentes-de-sol/50",             
  "/accesorios/lonas-y-pareos/50",
  "/accesorios/mantas/50",
  "/accesorios/mates-y-materas/50",           
  "/accesorios/medias/50",
  "/accesorios/necessaries/50",
  "/accesorios/panuelos/50",                  
  "/accesorios/pelo/50",
  "/accesorios/sombreros-y-gorros/50",
  "/accesorios/otros/50",                     
  "/accesorios/hombres/50",
  "/accesorios/1?filters=170",
  "/accesorios/1?filters=171",
  
  # Beauty
  "/beauty/50",
  "/beauty/cuerpo/50",
  "/beauty/dermocosmetica/50",
  "/beauty/fragancias/50",
  "/beauty/pelo/50",
  "/beauty/onerique/50",
  "/beauty/redken/50",
  "/beauty/kerastase/50",
  "/beauty/l-rsquo-oreal-professionnel/50",   
  "/beauty/matrix/50",
  "/beauty/1?filters=171"
))]

# Create main folder for images
dir_create("images/images/previa-03")

# Function to scrape and download images from a subcategory
scrape_images_subcategory <- function(subcat_url) {
  
  full_url <- paste0(base_url, subcat_url)
  subcat_page <- read_html(full_url)
  
  # Extract image URLs
  image_links <- subcat_page %>%
    html_nodes("img.productViewHoverImage.desktopElement") %>%
    html_attr("data-src") %>%
    unique()
  
  if (length(image_links) == 0) return(NULL)
  
  # Parse category and subcategory
  # Remove the initial "/" and final "/50"
  clean_path <- subcat_url %>%
    str_remove("^/") %>%
    str_remove("/50$") 
  
  # Split into parts
  parts <- str_split(clean_path, "/", simplify = TRUE)
  
  # Main category (e.g., "vestimenta")
  main_cat <- parts[1]
  
  # Subcategory (if any)
  if (ncol(parts) > 1) {
    sub_cat <- parts[2]
    folder_path <- file.path("images/previa-03", main_cat, sub_cat)
  } else {
    folder_path <- file.path("images/previa-03", main_cat)
  }
  
  # Create the folder
  dir_create(folder_path, recurse = TRUE)
  
  # Download each image into the correct folder
  walk2(image_links, seq_along(image_links), function(img_link, i) {
    
    if (!is.na(img_link) && img_link != "") {
      
      img_ext <- tools::file_ext(img_link)
      img_ext <- tolower(img_ext)
      
      if (img_ext == "") img_ext <- "jpg"
      
      img_name <- paste0("img_", i, ".", img_ext)
      destfile <- file.path(folder_path, img_name)
      
      tryCatch({
        download.file(img_link, destfile, mode = "wb", quiet = TRUE)
      }, error = function(e) {
        message("Failed to download: ", img_link)
      })
    }
    
  })
}

# Apply the scraping function to each subcategory
walk(subcategory_links, scrape_images_subcategory)
