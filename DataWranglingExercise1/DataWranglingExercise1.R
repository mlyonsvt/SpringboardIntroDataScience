# Data Wrangling Exercise 1
# Room for improvement: working directory, method of changing company name

# Set working directory to the desktop, the location of my file
setwd("C:/Users/mwlyo/Desktop")

# Import the file as a data frame called "refine"
refine <- (read.csv("refine_original.csv", header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE))

# Turns all capital letters from the "company" to lowercase letters
refine$company <- tolower(refine$company)

# Changes the company name to its proper entry based on the first character of the current entry
for (i in 1:length(refine$company)) {
  if (startsWith(refine$company[i], "p") == TRUE | startsWith(refine$company[i], "f") == TRUE) {
    refine$company[i] <- "Philips"
  }
  else if (startsWith(refine$company[i], "a") == TRUE) {
    refine$company[i] <- "Akzo"
  }
  else if (startsWith(refine$company[i], "v") == TRUE) {
    refine$company[i] <- "Van Houten"
  }
  else if (startsWith(refine$company[i], "u") == TRUE) {
    refine$company[i] <- "Unilever"
  }
}

# Modifications to the table, starting with separating the "Product.code...number" variable
# into two new variables, "product_code" and "product_ name"
refine <- separate(refine, Product.code...number, c("product_code", "product_name"), sep = "-") %>%
  
  # Creates the new variable "product_ category" and bases its entry off "product_code"
  mutate(product_category = ifelse(product_code == "p", "Smartphone",
                                   ifelse(product_code == "v", "TV",
                                          ifelse(product_code == "x", "Laptop",
                                                 ifelse(product_code == "q","Tablet","NA"))))) %>%
  
  # Unites the "address", "city", and "country" variables as a single variable called "full_address" with
  # entries separated by commas
  unite(full_address, address, city, country, sep = ",", remove = TRUE) %>%
  
  # Creates the 8 binary columns to identify the company name and product category
  mutate(company_philips = ifelse(company == "Philips", 1, 0),
         company_akzo = ifelse(company == "Akzo", 1, 0),
         company_van_houten = ifelse(company == "Van Houten", 1, 0),
         company_unilever = ifelse(company == "Unilever", 1, 0),
         product_smartphone = ifelse(product_category == "Smartphone", 1, 0),
         product_tv = ifelse(product_category == "TV", 1, 0),
         product_laptop = ifelse(product_category == "Laptop", 1, 0),
         product_tablet = ifelse(product_category == "Tablet", 1, 0))

# Writes the new table as a .csv file called "refine_ clean.csv"
write.csv(refine, "refine_clean.csv")
