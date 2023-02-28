
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

# make a data table with checkouts for each audiobook in the series in 2022 & 2023
SOUE_audio_df <- ls_book_df %>% filter(str_detect(Title, "Series")) %>% filter(str_detect(Title, "Unfortunate")) %>% filter(MaterialType == "AUDIOBOOK")
View(SOUE_audio_df)

audio_checkouts <- SOUE_audio_df %>% group_by(Title)  %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))
View(audio_checkouts)

# first book (The Bad Beginning) month with highest checkouts
bb_highest_month <- SOUE_df %>% filter(str_detect(Title, "Beginning")) %>% summarize(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutMonth)
bb_highest_month
