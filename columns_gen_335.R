# installing packages
require(stringr)

# setting the work directory
setwd("R:/Project/EPAR/Working Files/RA Working Folders/Rohit/")

# reading the input csv file
df <- read.csv("335 estimate template.csv")

# saving the initial columns
init_cols <- colnames(df)

# starting data manipulation

# getting the main string that has all the encoded information
input_string_vector <- df$Variable.Name..in.the..dta.file.

# identifying all 4 dimensions as specified by Isabella

# dimension1: currency
df$curr_1 <- str_detect(df$Variable.Name..in.the..dta.file., "1ppp")
df$curr_2 <- str_detect(df$Variable.Name..in.the..dta.file., "2ppp")
df$curr_3 <- str_detect(df$Variable.Name..in.the..dta.file., "loc")

# creating currency column
df$pred_curr <- ifelse(df$curr_1 == TRUE, "Private Consumption PPP", 'N/A')
df$pred_curr <- ifelse(df$curr_2 == TRUE, "GDP PPP", df$pred_curr)
df$pred_curr <- ifelse(df$curr_3 == TRUE, "Local Currency", df$pred_curr)

# dimension2: farm size
df$farm_1 <- str_detect(df$Variable.Name..in.the..dta.file., "0Ha")
df$farm_2 <- str_detect(df$Variable.Name..in.the..dta.file., "01Ha")
df$farm_3 <- str_detect(df$Variable.Name..in.the..dta.file., "12Ha")
df$farm_4 <- str_detect(df$Variable.Name..in.the..dta.file., "24Ha")
df$farm_5_1 <- str_detect(df$Variable.Name..in.the..dta.file., "4Ha")
df$farm_5_2 <- (!str_detect(df$Variable.Name..in.the..dta.file., "24Ha"))
df$farm_5 <- (df$farm_5_1 & df$farm_5_2)

# identifying all string patterns which usually have farm size encoded
df$farm_pattern <- (df$farm_1 | df$farm_2 | df$farm_3 | df$farm_4 | df$farm_5)
farm_strings <- df[which(df$farm_pattern == TRUE),]
farm_strings <- farm_strings$Variable.Name..in.the..dta.file.

# loop to isolate all string parts before "Ha" mention
# results store all the string before "Ha" and drops any numeric value
result_farm <- vector(mode = "character")
for (i in 1:length(farm_strings)){
  curr_elem <- as.character(farm_strings[i])
  # checking for the "Ha" pattern
  res <- gsub( "Ha.*$", "", curr_elem)
  # getting only that part of string which is before any digit
  res <- gsub("[[:digit:]]+.*$", "", res)
  # adding the element to the empty vector
  result_farm <- append(result_farm, res)
}

# detecting if any of the above pattern exists in the dataframe (for all versus N/A)
pattern_pool <- unique(result_farm) 
detect_str_pattern <- function(string_check){
  found_flag <- 0
  # going through all the patterns in the pool
  for (i in 1:length(pattern_pool)){
    curr_pattern <- pattern_pool[i]
    detect_flag <- str_detect(string_check, curr_pattern)
    if (detect_flag == TRUE){
      found_flag <- 1
    }
  }
  return (found_flag)
}

# getting the result for detecting if farm pattern exists
pattern_pool <- unique(result_farm)
df$farm_pattern <- sapply(df$Variable.Name..in.the..dta.file., detect_str_pattern)

# creating farm size column
df$pred_farm <- ifelse(df$farm_pattern == TRUE, "All", "N/A")
df$pred_farm <- ifelse(df$farm_1 == TRUE, "0 ha", df$pred_farm)
df$pred_farm <- ifelse(df$farm_2 == TRUE, "0<ha<=1", df$pred_farm)
df$pred_farm <- ifelse(df$farm_3 == TRUE, "1<ha<=2", df$pred_farm)
df$pred_farm <- ifelse(df$farm_4 == TRUE, "2<ha<=4", df$pred_farm)
df$pred_farm <- ifelse(df$farm_5 == TRUE, ">4 ha", df$pred_farm)

# dimension3: gender disaggregation
df$gender_1 <- str_detect(df$Variable.Name..in.the..dta.file., "_fhh")
df$gender_2 <- str_detect(df$Variable.Name..in.the..dta.file., "_mhh")
df$gender_3 <- str_detect(df$Variable.Name..in.the..dta.file., "wage_paid_aglabor_all")
df$gender_4 <- str_detect(df$Variable.Name..in.the..dta.file., "wage_paid_aglabor_female")
df$gender_5 <- str_detect(df$Variable.Name..in.the..dta.file., "wage_paid_aglabor_male")
df$gender_6 <- str_detect(df$Variable.Name..in.the..dta.file., "_female")
df$gender_7 <- str_detect(df$Variable.Name..in.the..dta.file., "_male")
df$gender_8 <- str_detect(df$Variable.Name..in.the..dta.file., "_mixed")
df$gender_9 <- str_detect(df$Variable.Name..in.the..dta.file., "all_vac_animal")
df$gender_10 <- str_detect(df$Variable.Name..in.the..dta.file., "female_vac_animal")
df$gender_11 <- str_detect(df$Variable.Name..in.the..dta.file., "male_vac_animal")
df$gender_12_1 <- str_detect(df$Variable.Name..in.the..dta.file., "^female")
df$gender_12_2 <- (! str_detect(df$Variable.Name..in.the..dta.file., "vac_animal"))
df$gender_12 <- (df$gender_12_1 & df$gender_12_2)
df$gender_13_1 <- str_detect(df$Variable.Name..in.the..dta.file., "^male")
df$gender_13_2 <- (! str_detect(df$Variable.Name..in.the..dta.file., "vac_animal"))
df$gender_13 <- (df$gender_13_1 & df$gender_13_2)
df$gender_14_1 <- str_detect(df$Variable.Name..in.the..dta.file., "^all")
df$gender_14_2 <- (! str_detect(df$Variable.Name..in.the..dta.file., "vac_animal"))
df$gender_14 <- (df$gender_14_1 & df$gender_14_2)

# logic for "all households"
df$all_gender_pattern <- (df$gender_1 | df$gender_2)
gender_strings <- df[which(df$all_gender_pattern == TRUE),]
gender_strings <- gender_strings$Variable.Name..in.the..dta.file.

# loop to isolate all string parts before "mhh/fhh" mention
# results store all the string before keywords and drops any numeric value
result_gender <- vector(mode = "character")
for (i in 1:length(gender_strings)){
  curr_elem <- as.character(gender_strings[i])
  # checking for both the patterns
  res1 <- gsub( "fhh.*$", "", curr_elem)
  res2 <- gsub( "mhh.*$", "", curr_elem)
  if (str_length(res1) > str_length(res2)){
    res <- res2
  }
  else if (str_length(res1) < str_length(res2)){
    res <- res1
  } 
  else if (str_length(res1) == str_length(res2)){
    res <- res1
  }
  # getting only that part of string which is before any digit
  res <- gsub("[[:digit:]]+.*$", "", res)
  # removing "_" if present at the end of string
  res <- sub("_$", "", res)
  # adding the element to the empty vector
  result_gender <- append(result_gender, res)
}

# detecting if any of the above pattern exists in the dataframe (for all households)
pattern_pool <- unique(result_gender)
df$gender_headed_pattern <- sapply(df$Variable.Name..in.the..dta.file., detect_str_pattern)

# loop to isolate all string parts before "female/male/mixed managed plots"
df$all_plots_pattern <- (df$gender_6 | df$gender_7 | df$gender_8)
gender2_strings <- df[which(df$all_plots_pattern == TRUE),]
gender2_strings <- gender2_strings$Variable.Name..in.the..dta.file.
# results store all the string before keywords and drops any numeric value
result_gender2 <- vector(mode = "character")
for (i in 1:length(gender2_strings)){
  curr_elem <- as.character(gender2_strings[i])
  # checking for all the patterns
  res1 <- gsub( "_female.*$", "", curr_elem)
  res2 <- gsub( "_male.*$", "", curr_elem)
  res3 <- gsub( "_mixed.*$", "", curr_elem)
  if (str_length(res1) > str_length(res2)){
    res <- res2
  }
  else if (str_length(res1) < str_length(res2)){
    res <- res1
  } 
  else if (str_length(res) > str_length(res3)){
    res <- res3
  }
  # getting only that part of string which is before any digit
  res <- gsub("[[:digit:]]+.*$", "", res)
  # removing "_" if present at the end of string
  res <- sub("_$", "", res)
  # adding the element to the empty vector
  result_gender2 <- append(result_gender2, res)
}

# detecting if any of the above pattern exists in the dataframe (for all plots)
pattern_pool <- unique(result_gender2)
df$gender_plots_pattern <- sapply(df$Variable.Name..in.the..dta.file., detect_str_pattern)

# creating gender column
df$pred_gender <- ifelse(df$gender_headed_pattern == TRUE, "All households", "N/A")
df$pred_gender <- ifelse(df$gender_plots_pattern == TRUE, "All plots", df$pred_gender)
df$pred_gender <- ifelse(df$gender_9 == TRUE, "All livestock managers", df$pred_gender)
df$pred_gender <- ifelse(df$gender_11 == TRUE, "Male livestock managers", df$pred_gender)
df$pred_gender <- ifelse(df$gender_10 == TRUE, "Female livestock managers", df$pred_gender)
df$pred_gender <- ifelse(df$gender_13 == TRUE, "Male plot managers", df$pred_gender)
df$pred_gender <- ifelse(df$gender_14 == TRUE, "All plot managers", df$pred_gender)
df$pred_gender <- ifelse(df$gender_12 == TRUE, "Female plot managers", df$pred_gender)
df$pred_gender <- ifelse(df$gender_1 == TRUE, "Female-headed households", df$pred_gender)
df$pred_gender <- ifelse(df$gender_2 == TRUE, "Male-headed households", df$pred_gender)
df$pred_gender <- ifelse(df$gender_7 == TRUE, "Male only-managed plots", df$pred_gender)
df$pred_gender <- ifelse(df$gender_6 == TRUE, "Female only-managed plots", df$pred_gender)
df$pred_gender <- ifelse(df$gender_8 == TRUE, "Mixed gender-managed plots", df$pred_gender)
df$pred_gender <- ifelse(df$gender_3 == TRUE, "All individual laborers", df$pred_gender)
df$pred_gender <- ifelse(df$gender_5 == TRUE, "Male laborers", df$pred_gender)
df$pred_gender <- ifelse(df$gender_4 == TRUE, "Female laborers", df$pred_gender)

# dimension4: crop disaggregation
df$crop_1 <- str_detect(df$Variable.Name..in.the..dta.file., "maize")
df$crop_2 <- str_detect(df$Variable.Name..in.the..dta.file., "rice")
df$crop_3 <- str_detect(df$Variable.Name..in.the..dta.file., "wheat")
df$crop_4_1 <- str_detect(df$Variable.Name..in.the..dta.file., "sorghum")
df$crop_4_2 <- str_detect(df$Variable.Name..in.the..dta.file., "sorgum")
df$crop_4 <- (df$crop_4_1 | df$crop_4_2)
df$crop_5 <- str_detect(df$Variable.Name..in.the..dta.file., "millet")
df$crop_6 <- str_detect(df$Variable.Name..in.the..dta.file., "cowpea")
df$crop_7_1 <- str_detect(df$Variable.Name..in.the..dta.file., "grdnt")
df$crop_7_2 <- str_detect(df$Variable.Name..in.the..dta.file., "groundnut")
df$crop_7 <- (df$crop_7_1 | df$crop_7_2)
df$crop_8 <- str_detect(df$Variable.Name..in.the..dta.file., "bean")
df$crop_9 <- str_detect(df$Variable.Name..in.the..dta.file., "yam")
df$crop_10_1 <- str_detect(df$Variable.Name..in.the..dta.file., "swtptt")
df$crop_10_2 <- str_detect(df$Variable.Name..in.the..dta.file., "sweet potato")
df$crop_10 <- (df$crop_10_1 | df$crop_10_2)
df$crop_11 <- str_detect(df$Variable.Name..in.the..dta.file., "cassav")
df$crop_12 <- str_detect(df$Variable.Name..in.the..dta.file., "banana")
df$crop_13 <- str_detect(df$Variable.Name..in.the..dta.file., "cotton")
df$crop_14_1 <- str_detect(df$Variable.Name..in.the..dta.file., "sunflr")
df$crop_14_2 <- str_detect(df$Variable.Name..in.the..dta.file., "sunflower")
df$crop_14 <- (df$crop_14_1 | df$crop_14_2)
df$crop_15 <- str_detect(df$Variable.Name..in.the..dta.file., "pigeon pea")

# creating crop column
df$pred_crop <- ifelse(df$crop_1 == TRUE, "Maize", "N/A")
df$pred_crop <- ifelse(df$crop_2 == TRUE, "Rice", df$pred_crop)
df$pred_crop <- ifelse(df$crop_3 == TRUE, "Wheat", df$pred_crop)
df$pred_crop <- ifelse(df$crop_4 == TRUE, "Sorghum", df$pred_crop)
df$pred_crop <- ifelse(df$crop_5 == TRUE, "Millet", df$pred_crop)
df$pred_crop <- ifelse(df$crop_6 == TRUE, "Cowpea", df$pred_crop)
df$pred_crop <- ifelse(df$crop_7 == TRUE, "Groundnut", df$pred_crop)
df$pred_crop <- ifelse(df$crop_8 == TRUE, "Beans", df$pred_crop)
df$pred_crop <- ifelse(df$crop_9 == TRUE, "Yam", df$pred_crop)
df$pred_crop <- ifelse(df$crop_10 == TRUE, "Sweet Potato", df$pred_crop)
df$pred_crop <- ifelse(df$crop_11 == TRUE, "Cassava", df$pred_crop)
df$pred_crop <- ifelse(df$crop_12 == TRUE, "Banana", df$pred_crop)
df$pred_crop <- ifelse(df$crop_13 == TRUE, "Cotton", df$pred_crop)
df$pred_crop <- ifelse(df$crop_14 == TRUE, "Sunflower", df$pred_crop)
df$pred_crop <- ifelse(df$crop_15 == TRUE, "Pigeon Pea", df$pred_crop)

# creating the final file
cols <- append(init_cols, "pred_curr")
cols <- append(cols, "pred_farm")
cols <- append(cols, "pred_gender")
cols <- append(cols, "pred_crop")

# subsetting the final dataframe
final_df <- df[cols]

#writing to disk
write.csv(final_df, file = "335 estimate template (Automated).csv")
