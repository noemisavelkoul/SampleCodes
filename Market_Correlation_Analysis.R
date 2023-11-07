install.packages("readr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("xlsx")
install.packages("writexl")
install.packages("dplyr")
library("readxl")
library(readr) 
library(ggplot2) 
library("xlsx")
library("writexl")
library(dplyr)

# ==== Download and modify data set ====

# Download and modify monthly level data (actually used in the analysis)

## Downloading data and specifying countries 

df <- read_csv("Pivot_2020_2022_monthly (full).csv")
df <- df[df$Brand != "Grand Total", ] # Removing Grand Total since it is the sum
countries_to_include <- c("Country_1", "Country_2", "Country_3", "Country_4", "Country_5") # List of countries to include in the analysis 
df <- df[df$Country %in% countries_to_include, ] # Modifying data frame to only include countries to include

## Including product line line_7 in product line line_1

df <- df %>%
  mutate(Line = ifelse(Line == "line_7", "line_1", Line))

df <- df %>%
  group_by(Year, Month, Country, Line, Groups_Affiliation, Brand) %>%
  summarise(UNITS = sum(UNITS, na.rm = TRUE),
            VALUE = sum(VALUE, na.rm = TRUE)) %>%
  ungroup()

df <- df[!duplicated(df[c("Year", "Month", "Country", "Line", "Groups_Affiliation", "Brand")]), ]

## Computing yearly market shares for each country-product line combination (to display 2022 market shares on correlation tables as requested)

### Computing total units and value for each year, product line, country combination 

total_data <- aggregate(cbind(UNITS, VALUE) ~ Year + Country + Line, data = df, sum)
df <- merge(df, total_data, by = c("Year", "Line", "Country"), suffixes = c("", "_sum"))

## Computing total units and value for each year, line, country and brand combination 

total_data <- aggregate(cbind(UNITS, VALUE) ~ Year + Country + Line + Brand, data = df, sum)
df <- merge(df, total_data, by = c("Year", "Line", "Country", "Brand"), suffixes = c("", "_brand_sum"))

## Computing market shares

df$UNITS_brand_sum <- df$UNITS_brand_sum/df$UNITS_sum
df$VALUE_brand_sum <- df$VALUE_brand_sum/df$VALUE_sum

## Renaming and excluding columns 

df <- df[, !names(df) %in% c("UNITS_sum", "VALUE_sum")]
colnames(df)[colnames(df) == "UNITS_brand_sum"] <- "VOLUME_SHARE"
colnames(df)[colnames(df) == "VALUE_brand_sum"] <- "VALUE_SHARE"

# ==== Group data frame per country ====

country_list <- list() # Initializing list of country names 

for (i in 1:(nrow(as.matrix(countries_to_include)))) { # Loop over each country
  
  new_object_name <- paste0("df_", countries_to_include[i])
  new_object <- df[!(df$Country!=countries_to_include[i]),]
  assign(new_object_name, new_object)
  country_list[[i]] <- countries_to_include[i] # Assigning country to list of countries 
  
}

## Unnecessary for the code, but for ease, you can access the data frame for each of the chosen countries through
## df_C, where C is the country as written in the original data set 

# ==== Group data frame per product line, for each country ==== 

line_list <- country_list #list of product lines for each country, initialized with country_list

for (i in 1:(length(country_list))) { # Loop over each country
  
  new_object_name <- paste0("unique_line_", country_list[i])
  new_object <- df[!(df$Country!=country_list[i]),]
  new_object <- unique(new_object$Line)
  assign(new_object_name, new_object)
  line_list[[i]] <- as.matrix(new_object) # Assigning product line to list
  
}

## Unnecessary for the code, but for ease, you can access the list of unique product lines per country through unique_line_C, 
## where C is the country as written in the original data set

# ==== Grouping data frame per brand, for each country—product line combination ====

brand_list <- line_list # List of brand names for each country and product line, initialized with line_list 

for (i in 1:length(country_list)) { # Loop over each country 
  
  current_country_df <- df[!(df$Country!=country_list[i]),] # Storing data frame for current country 
  new_object_name_1 <- paste0("unique_brand_", countries_to_include[i])
  brand_list[[i]] <- vector("list", length = length(line_list[[i]])) # Accessing list of lines for the current country
  
  for (j in 1:length(brand_list[[i]])) { # Loop over each product line for the current country
    
    new_object_name_2 <- paste0("_",line_list[[i]][j])
    new_object_name <- paste0(new_object_name_1, new_object_name_2)
    new_object <- unique(current_country_df[!(current_country_df$Line!=line_list[[i]][[j]]),]$Brand)
    assign(new_object_name, new_object)
    brand_list[[i]][[j]] <- new_object # Assigning list of brands to list, for the current country-product line combination
    
    
  }
}

## Unnecessary for the code, but, for ease, you can access the list of unique brands for each country—product Line combination using unique_brand_C_L
## where C is the country and L is the product line as written in the original data set 


# ==== Market Share Matrix pt.1: Compute, for each period, the total units sold for each country—product Line combination, in terms of value ====

total_value <- list() # List to store the sum of values, for each period, for each country-product Line combination, in terms of value

for (i in 1:length(country_list)) { # Loop over each country 
  
  current_country_df <- df[!(df$Country!=countries_to_include[i]),] # Storing data frame for current country 
  
  m <- matrix(0,36, length(brand_list[[i]])) # Initializing matrix that stores total values, for each country-product  line combination
  
  for (j in 1:length(brand_list[[i]])) { # Loop over each product line for the current country
    
    current_country_df_2 <- current_country_df[!(current_country_df$Line!=line_list[[i]][j]),] # restricting the data frame for current country to a specific product line
    
    
    for (k in 1:nrow(as.matrix(current_country_df_2))) { # Loop over each brand available for current country-product—Line combination
      
      year <- as.integer(current_country_df_2$Year[k]) # Year of the current observation 
      month <- as.integer(current_country_df_2$Month[k]) # Month of the current observation 
      value <- as.integer(current_country_df_2$VALUE[k]) # Value of the current year—month observation, for the current country—product Line combination
      
      # Find the index for the corresponding month-year combination in the vector
      index <- (year - 2020) * 12 + month 
      
      # Add the value to the corresponding position in the vector
      m[index,j] <- m[index,j] + value
      total_value[[i]] <- m
    }
    
  }
  
}

## GENERAL COMMENT: As requested, computed total values using ALL brands, not just brands for which no observation missing

# ==== Market Share Matrix pt.2: Construct the changes in market share matrices for each country-product—line combination ====

# Computing changes in market share matrices 

for (i in 1:length(country_list)) { # Loop over each country 
  
  current_country_df <- df[!(df$Country!=country_list[i]),] # storing data frame for current country 
  new_object_name_1 <- paste0("market_share_", country_list[i],"_")
  
  for (j in 1:length(brand_list[[i]])) {  # Loop over each product line for the current country
    
    new_object_name_2 <- paste0(line_list[[i]][j])
    new_object_name <- paste0(new_object_name_1, new_object_name_2)
    current_country_df_2 <- current_country_df[!(current_country_df$Line!=line_list[[i]][j]),] # Restricting the data frame for current country to a specific product line
    
    # Creating an empty matrix (market share matrix) where the number of rows is the number of observations (36 since there are 3 years and 12 months in each year)
    ## and the number of columns is the number of unique brands associated with the current country—product—line combination 
    current_market_share <- data.frame(matrix(nrow = 36, ncol = length(brand_list[[i]][[j]]))) 
    colnames(current_market_share) <- brand_list[[i]][[j]] # Naming the columns of the matrix as the brand names
    C <- current_country_df_2 
    C <- C %>%
      arrange(as.numeric(Year), as.numeric(Month))
    
    for (k in 1:length(brand_list[[i]][[j]])) { # Loop over each brand belonging to current country—product Line combination
      
      D <-  C[!(C$Brand!=brand_list[[i]][[j]][k]),] # Restricting data frame of current country—product Line combination to current brand 
      
      if (nrow(as.matrix(D)) == 36) {  # As requested, ensuring only brands for which no observation is missing are included in the correlation matrix
        
        current_market_share[,k] <- D$VALUE/as.matrix(total_value[[i]])[,j] # Computing market shares, per brand, for each of the 36 periods
        
      } else {
        
        current_market_share[,k] <- current_market_share[,k] # Ensuring columns associated with brands for which observations are missing retain NA for every entry
        
      }
      
      current_market_share[,k] <- c(NA,diff(as.matrix(current_market_share[,k]))) # Taking the first difference of the market shares (changes in market share), replacement length must be equal to 36 
      
    }
    
    assign(new_object_name, current_market_share) # Creating a changes in market share object for each country—product line combination 
    
  }
  
}

# You can access the changes in market share matrix for each country—product line combination through market_share_C_L, where 
## C is the country and L is the line as written in the original data set 

# ==== Calculate changes in market share correlation matrix (change inputs accordingly) ====

# Change inputs into desired country—product line combination

Country <- "Country_2" #country
Line <-  "line_4" #line 
ms <-  market_share_Country_2_line_4 #changes in market share matrix for the country and line 

# Constructing data frame necessary to compute changes in market share correlation matrix 

ms <- as.matrix(ms)
chosen_df <- df[df$Country %in% Country, ] # Updating data frame to adhere to chosen country
chosen_df <- chosen_df[chosen_df$Line %in% Line, ] # Updating data frame to adhere to chosen country—product line combination
ms_not_na <- ms[, colSums(!is.na(ms)) > 0] # Removing columns for which all entries are NA

# Constructing correlation matrix (using regression)

correlation <- matrix(NA, nrow = ncol(ms_not_na), ncol = ncol(ms_not_na)) # Initializing correlation matrix
colnames(correlation) <- colnames(ms_not_na)
rownames(correlation) <- colnames(ms_not_na)

# Regressing each row of the market share matrix on each other row

for (i in 1:ncol(ms_not_na)) { 
  
  for (j in 1:ncol(ms_not_na)) { 
    
    ## Fitting a linear model for column i regressed on column j, with constant, rounding of coefficients to three decimals 
    
    if (i==j) {
      
      correlation[i, j] <- 1 
      
    } else {
      
      model <- lm(ms_not_na[2:36, (i)] ~ ms_not_na[2:36, (j)])
      if (summary(model)$coefficients[2,4] <= 0.1 && summary(model)$coefficients[2,4] >= 0.05 ) {
        correlation[i, j] <- paste0((round(coef(model)[2], 3)), "*") # "*", significant at 10%
      } else if (summary(model)$coefficients[2,4] <= 0.05 && summary(model)$coefficients[2,4] >= 0.01 ) {
        correlation[i,j] <- paste0((round(coef(model)[2], 3)), "**") # "**", significant at 5%
      } else if (summary(model)$coefficients[2,4] <= 0.01 ){
        correlation[i,j]<- paste0((round(coef(model)[2], 3)), "***") # "***", significant at 1%
      } else {
        correlation[i, j] <- round(coef(model)[2],3)
      }
      
    }
  }
}

# Creating correlation object 

new_name <- paste0("correlation_",as.character(Country), "_", as.character(Line))
assign(new_name, correlation)

## Chosen country—product—line combination changes in market share correlation matrix saved as correlation_C_L, where C is the country and L 
## the product—line chosen as in the original data set to export as necessary.

# ==== Creating output (different options) ====

# Append 2022 market shares to the correlation matrix 

## Gathering brand names for the current Country-product Line combination 
brands <- rownames(correlation)
value_shares <- rep(0, nrow(as.matrix(brands)))

## Looping through each brand in the current Country-product Line combination and gathering corresponding 2022 market share values 
for (i in 1:nrow(as.matrix(brands))) {
  brand <- brands[i]
  
  # Filter the data for the specific brand, year, month, country, and line
  filtered_data <- df[df$Brand == brand & df$Year == "2022" & df$Month == "1" & df$Country == Country & df$Line == Line, ]
  
  # Check if any rows match the criteria
  if (nrow(filtered_data) > 0) {
    value_shares[i] <- filtered_data$VALUE_SHARE
  }
}

## Creating correlation object 

correlation <- cbind(correlation, "MARKET_SHARE_2022" = value_shares)
assign(new_name, correlation)

# Only include brands for which the 2022 market shares exceed a specific percentage 

## Creating new correlation matrix to address the problem of comparing objects of type "character." Because of the ***, in correlation matrix 
## entries are characters. Since the function as.numeric will return NA for entries with ** a new object is necessary. 

threshold <- 0.00
correlation_new <- correlation 
correlation_new <- apply(correlation_new, 2, function(x) as.numeric(as.character(x)))
rows_to_delete <- which(correlation_new[,ncol(correlation_new)] <= threshold)

## Deleting rows that do not meet the criteria

correlation <- correlation[-rows_to_delete, -rows_to_delete]

## Creating correlation object 

assign(new_name, correlation)

# ==== Printing output ====

# Country_1

out <- list(
  "Country_1_line_1" = data.frame(correlation_Country_1_line_1),
  "Country_1_line_2" = data.frame(correlation_Country_1_line_2), 
  "Country_1_line_3" = data.frame(correlation_Country_1_line_3),
  "Country_1_line_4" = data.frame(correlation_Country_1_line_4),
  "Country_1_line_5" = data.frame(correlation_Country_1_line_5)
)
write_xlsx(out, "complete_Country_1.xlsx")

# Country_2

out <- list(
  "Country_2_line_1" = data.frame(correlation_Country_2_line_1),
  "Country_2_line_2" = data.frame(correlation_Country_2_line_2), 
  "Country_2_line_3" = data.frame(correlation_Country_2_line_3),
  "Country_2_line_6" = data.frame(correlation_Country_2_line_6),
  "Country_2_line_4" = data.frame(correlation_Country_2_line_4),
  "Country_2_line_5" = data.frame(correlation_Country_2_line_5)
)
write_xlsx(out, "complete_Country_2.xlsx")

# Country_3

out <- list(
  "Country_3_line_1" = data.frame(correlation_Country_3_line_1),
  "Country_3_line_2" = data.frame(correlation_Country_3_line_2), 
  "Country_3_line_3" = data.frame(correlation_Country_3_line_3), 
  "Country_3_line_5" = data.frame(correlation_Country_3_line_5)
)
write_xlsx(out, "complete_Country_3.xlsx")

# Country_4
out <- list(
  "Country_4_line_1" = data.frame(correlation_Country_4_line_1),
  "Country_4_line_2" = data.frame(correlation_Country_4_line_2), 
  "Country_4_line_3" = data.frame(correlation_Country_4_line_3),
  "Country_4_line_6" = data.frame(correlation_Country_4_line_6),
  "Country_4_line_5" = data.frame(correlation_Country_4_line_5)
)
write_xlsx(out, "complete_Country_4.xlsx")

# Country_5
out <- list(
  "Country_5_line_1" = data.frame(correlation_Country_5_line_1),
  "Country_5_line_2" = data.frame(correlation_Country_5_line_2), 
  "Country_5_line_3" = data.frame(correlation_Country_5_line_3), 
  "Country_5_line_5" = data.frame(correlation_Country_5_line_5)
)
write_xlsx(out, "complete_Country_5.xlsx")










