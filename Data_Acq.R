library(rvest)
library(stringr)
library(lubridate)
library(dplyr)

######################## Production (Tonne) Of Cocoa Beans By Region in Malaysia

region_prod <- read_html("https://www.koko.gov.my/lkm/industry/statistic/p_cocoabean.cfm")
# 2022: estimate only

# Year of production
year_nodes <- html_nodes(region_prod, 'tr:nth-child(4) tr~ tr+ tr td:nth-child(1)')
year_texts <- html_text(year_nodes)
# take only the digit and change the structure
year_texts <- str_extract(year_texts, "\\d+")
year <- year(as.Date(year_texts, format = "%Y"))

# Production Of Cocoa Beans By Peninsular Malaysia
wm_nodes <- html_nodes(region_prod, 'tr:nth-child(4) tr~ tr+ tr td:nth-child(2)')
wm_texts <- html_text(wm_nodes)
wm <- as.numeric(str_remove_all(wm_texts, ","))

# Production Of Cocoa Beans By Sabah
sbh_nodes <- html_nodes(region_prod, 'td tr~ tr+ tr td:nth-child(3)')
sbh_texts <- html_text(sbh_nodes)
sbh <- as.numeric(str_remove_all(sbh_texts, ","))

# Production Of Cocoa Beans By Sarawak
swk_nodes <- html_nodes(region_prod, 'td tr~ tr+ tr td:nth-child(4)')
swk_texts <- html_text(swk_nodes)
swk <- as.numeric(str_remove_all(swk_texts, ","))

# Total Production Of Cocoa Beans By Malaysia
my_nodes <- html_nodes(region_prod, 'td:nth-child(5)')
my_texts <- html_text(my_nodes)
my <- as.numeric(str_remove_all(my_texts, ","))

prod_lists <- list(Year = year, West_Msia = wm, 
                   Sabah = sbh, Sarawak = swk, Total = my)
reg_data <- as.data.frame(do.call(cbind, prod_lists))
#write.csv(reg_data, "region_data.csv", row.names = FALSE)


############################################## Export of Cocoa Beans by Malaysia

export <- read_html("https://www.koko.gov.my/lkm/industry/statistic/export.cfm")
# 2010-2022: re-export

# Export Year
year_nodes <- html_nodes(export, '#AutoNumber1 tr~ tr+ tr td:nth-child(1)')
year_texts <- html_text(year_nodes)
year <- year(as.Date(year_texts, format = "%Y"))

# Export Quantity (Tonne)
qty_nodes <- html_nodes(export, '#AutoNumber1 tr~ tr+ tr td:nth-child(2)')
qty_texts <- html_text(qty_nodes)
qty_texts <- str_remove_all(qty_texts, ",")
qty_texts <- str_extract(qty_texts, "\\d+")
qty <- as.numeric(qty_texts)

# Export Value (RM'000)
value_nodes <- html_nodes(export, '#AutoNumber1 tr~ tr+ tr td:nth-child(3)')
value_texts <- html_text(value_nodes)
value <- as.numeric(str_remove_all(value_texts, ","))

# Export FOB (RM/Tonne)
fob_nodes <- html_nodes(export, '#AutoNumber1 tr~ tr+ tr td:nth-child(4)')
fob_texts <- html_text(fob_nodes)
fob <- as.numeric(str_remove_all(fob_texts, ","))

export_lists <- list(Year = year, Quantity = qty, Value = value, FOB = fob)
export_data <- as.data.frame(do.call(cbind, export_lists))
#write.csv(export_data, "export_beans.csv", row.names = FALSE)


############################################## Import of Cocoa Beans by Malaysia

import <- read_html("https://www.koko.gov.my/lkm/industry/statistic/import.cfm")

# Import Year
year_nodes <- html_nodes(import, '.title~ tr+ tr td:nth-child(1)')
year_texts <- html_text(year_nodes)
year <- year(as.Date(year_texts, format = "%Y"))

# Import Quantity (Tonne)
qty_nodes <- html_nodes(import, '.title~ tr+ tr td:nth-child(2)')
qty_texts <- html_text(qty_nodes)
qty <- as.numeric(str_remove_all(qty_texts, ","))

# Import Value (RM'000)
value_nodes <- html_nodes(import, '.title~ tr+ tr td:nth-child(3)')
value_texts <- html_text(value_nodes)
value <- as.numeric(str_remove_all(value_texts, ","))

# Import CIF (RM/Tonne)
cif_nodes <- html_nodes(import, '.title~ tr+ tr td:nth-child(4)')
cif_texts <- html_text(cif_nodes)
cif <- as.numeric(str_remove_all(cif_texts, ","))

import_lists <- list(Year = year, Quantity = qty, Value = value, CIF = cif)
import_data <- as.data.frame(do.call(cbind, import_lists))
#write.csv(import_data, "import_beans.csv", row.names = FALSE)

###################################################### Chocolate Nutrition Facts

choc_nutri <- read_html("https://www.chocolate-advisor.com/guides/chocolate-nutrition-facts")
# Nutritional value per 100 g (3.5 oz)

# Nutrients
nutrient_nodes <- html_nodes(choc_nutri, 'td:nth-child(1)')
nutrient_texts <- html_text(nutrient_nodes)
nutrient_texts <- str_remove_all(nutrient_texts, "\\n")
nutrient_texts <- str_remove_all(nutrient_texts, "\\t")
nutrient <- str_trim(nutrient_texts)

# Black chocolate 40% cocoa
B40_nodes <- html_nodes(choc_nutri, 'p+ .table-responsive td:nth-child(3) , h2+ .table-responsive td:nth-child(2)')
B40_texts <- html_text(B40_nodes)
B40_texts <- str_remove_all(B40_texts, "\\n")
B40_texts <- str_remove_all(B40_texts, "\\t")
B40_texts <- gsub(",", ".", B40_texts)
B40 <- as.numeric(str_trim(B40_texts))
# traces will become NA

# Black chocolate 70% cocoa
B70_nodes <- html_nodes(choc_nutri, 'p+ .table-responsive td:nth-child(4) , h2+ .table-responsive td:nth-child(3)')
B70_texts <- html_text(B70_nodes)
B70_texts <- str_remove_all(B70_texts, "\\n")
B70_texts <- str_remove_all(B70_texts, "\\t")
B70_texts <- gsub(",", ".", B70_texts)
B70 <- as.numeric(str_trim(B70_texts))
# traces will become NA

# Milk chocolate
milk_nodes <- html_nodes(choc_nutri, 'p+ .table-responsive td:nth-child(5) , h2+ .table-responsive td:nth-child(4)')
milk_texts <- html_text(milk_nodes)
milk_texts <- str_remove_all(milk_texts, "\\n")
milk_texts <- str_remove_all(milk_texts, "\\t")
milk_texts <- gsub(",", ".", milk_texts)
milk <- as.numeric(str_trim(milk_texts))
# traces will become NA

# White chocolate
white_nodes <- html_nodes(choc_nutri, 'td:nth-child(6) , h2+ .table-responsive td:nth-child(5)')
white_texts <- html_text(white_nodes)
white_texts <- str_remove_all(white_texts, "\\n")
white_texts <- str_remove_all(white_texts, "\\t")
white_texts <- gsub(",", ".", white_texts)
white <- as.numeric(str_trim(white_texts))
# traces will become NA

nutrient_lists <- list(Nutrients = nutrient, Black_40 = B40, Black_70 = B70, 
                       Milk_Choco = milk, White_Choco = white)
nutrient_data <- as.data.frame(do.call(cbind, nutrient_lists))
#write.csv(nutrient_data, "nutrients.csv", row.names = FALSE)


######################################### Product with Chocolate Nutrition Facts

# As of 11 July 2023, there are 21 pages
# for each a 100 gram serving
pages<-paste0("https://www.selinawamucii.com/nutrition/page/", c(1:21), "/?s=chocolate&post_type=product")
# function to scrap relevant info
# page = pages[1|2|3]

# Name of products
name <- function(page){
  url<-read_html(page)
  nodes<-html_nodes(url, ".grippy-host , #food_title")
  html_text(nodes)
}
name <- do.call(c, lapply(pages, name))

# Fat (g) of products
fat <- function(page){
  url<-read_html(page)
  nodes<-html_nodes(url, ".grippy-host , .main_row td:nth-child(2)")
  html_text(nodes)
}
fat <- do.call(c, lapply(pages, fat))
fat <- as.numeric(fat)

# Carbs (g) of products
carbs <- function(page){
  url<-read_html(page)
  nodes<-html_nodes(url, ".main_row td:nth-child(3)")
  html_text(nodes)
}
carbs <- do.call(c, lapply(pages, carbs))
carbs <- as.numeric(carbs)

# Protein (g) of products
protein <- function(page){
  url<-read_html(page)
  nodes<-html_nodes(url, ".main_row td:nth-child(4)")
  html_text(nodes)
}
protein <- do.call(c, lapply(pages, protein))
protein <- as.numeric(protein)

# Calories of products
calories <- function(page){
  url<-read_html(page)
  nodes<-html_nodes(url, ".main_row td:nth-child(5)")
  html_text(nodes)
}
calories <- do.call(c, lapply(pages, calories))
calories <- as.numeric(calories)

product_lists <- list(Product = name, Fat_g = fat, Carbs_g = carbs,
                      Protein_g = protein, Calories = calories)
product_data <- as.data.frame(do.call(cbind, product_lists))
# List the product: "Not Chocolate", "Other Than Chocolate"
nc <- grep(pattern = "Not Chocolate",x = product_data$Product,ignore.case = T,value = T)
otc <- grep(pattern = "Other Than Chocolate",x = product_data$Product,ignore.case = T,value = T)
# Remove rows with the product name in list nc and list otc
#filtered_pdata <- product_data[!product_data$Product %in% c(nc,otc),]
data_rm <- product_data[product_data$Product %in% c(nc,otc),]
product_fdata <- anti_join(product_data,data_rm)

#write.csv(product_fdata, "choc_products.csv", row.names = FALSE)
# Calories, Carbs, and Proteins in 100g of ...

############################################## Worldwide cocoa production (tons)

w_prod_full <- read.csv("FAOSTAT_data_en_7-10-2023.csv")
#unique(w_prod_full$Flag.Description)
# A - "Official figure", T - "Unofficial figure", E - "Estimated value",
# I - "Imputed value", M - "Missing value (data cannot exist, not applicable)"
w_prod <- w_prod_full[c("Area", "Year", "Value")]
w_prod_data <- as.data.frame(w_prod %>% group_by(Year) %>% 
                               summarise(Total = sum(Value)))
#write.csv(w_prod_data, "cocoa.csv", row.names = FALSE)


############################################################## from wpd_choc.csv
# Flavors of Cacao: Plain Dark Chocolate Bars
# Updated on: June 17, 2023
chocolate <- read.csv("wpd_choc.csv")
chocolate <- chocolate[c("Country.of.Bean.Origin", 
                         "Cocoa.Percent", "Ingredients", 
                         "Most.Memorable.Characteristics", "Rating")]
chocolate$Ingredients <- as.integer(str_extract(chocolate$Ingredients, "\\d"))
colnames(chocolate) <- c("Origin", "Cocoa.Percent", "Ingredients", 
                         "Characteristics", "Rating")
#write.csv(chocolate, "choco_bar.csv", row.names = FALSE)
