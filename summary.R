
spl_data <- read.csv("~/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

library("dplyr")
library("stringr")

# created a dataset of all Lemony Snicket book checkouts in 2022-23
ls_book_df <- spl_data %>% filter(str_detect(Creator, "Lemony")) %>% filter(str_detect(Creator, "Snicket")) 
View(ls_book_df)

# created a dataset for all Series of Unfortunate Events book checkouts in 2022-23 (all eBooks)
SOUE_df <- ls_book_df %>% filter(str_detect(Title, "Series")) %>% filter(str_detect(Title, "Unfortunate")) %>% filter(MaterialType == "EBOOK")
View(SOUE_df)

# make a data table with checkouts for each book in the series in 2022 & 2023
checkouts <- SOUE_df %>% group_by(Title)  %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))
View(checkouts)

# Find most checked out book
most_checkouts <- checkouts %>% filter(checkout_count == max(checkout_count, na.rm = TRUE)) %>% pull(Title)
most_checkouts

# highest total checkout count
highest_checkouts <- checkouts %>% filter(checkout_count == max(checkout_count, na.rm = TRUE)) %>% pull(checkout_count)
highest_checkouts

# make a data table with checkouts for each audiobook in the series in 2022 & 2023
SOUE_audio_df <- ls_book_df %>% filter(str_detect(Title, "Series")) %>% filter(str_detect(Title, "Unfortunate")) %>% filter(MaterialType == "AUDIOBOOK")
View(SOUE_audio_df)

audio_checkouts <- SOUE_audio_df %>% group_by(Title)  %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))
View(audio_checkouts)

# first book(The Bad Beginning) month/year with highest ebook checkouts
bb_highest_month <- SOUE_df %>% filter(str_detect(Title, "Beginning")) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutMonth)
bb_highest_year <- SOUE_df %>% filter(str_detect(Title, "Beginning")) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)
bb_highest_month
bb_highest_year

 
#first book(The Bad Beginning) month/year with highest audiobook checkouts 
bb_highest_audio_month <- SOUE_audio_df %>% filter(str_detect(Title, "Beginning")) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutMonth)
bb_highest_audio_year <- SOUE_audio_df %>% filter(str_detect(Title, "Beginning")) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)
bb_highest_audio_month
bb_highest_audio_year

# Total checkouts in 2022
total_checkouts_2022 <- SOUE_df %>% filter(CheckoutYear == "2022") %>% summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>% pull(Checkouts)

# Total checkouts in 2023
total_checkouts_2023 <- SOUE_df %>% filter(CheckoutYear == "2023") %>% summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>% pull(Checkouts)

# change in (The Bad Beginning) ebook checkouts over time
SOUE_df <- SOUE_df %>% filter(str_detect(Title, "Beginning")) %>% mutate(checkout_change = Checkouts - lag(Checkouts))



