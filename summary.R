
spl_data <- read.csv("~/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

library("dplyr")
library("stringr")

# created a dataset of all Lemony Snicket ebook + audiobook checkouts in 2022-23
ls_book_df <- spl_data %>% filter(str_detect(Creator, "Lemony")) %>% filter(str_detect(Creator, "Snicket"))
ls_book_df <- ls_book_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
ls_book_df$date <- as.Date(ls_book_df$date, format = "%Y-%m-%d")

# created a dataset for all Series of Unfortunate Events eBook checkouts in 2022-23
SOUE_df <- ls_book_df %>% filter(str_detect(Title, "Series")) %>% filter(str_detect(Title, "Unfortunate")) %>% filter(MaterialType == "EBOOK")
checkouts <- SOUE_df %>% group_by(Title)  %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))
checkouts_bb_e <- SOUE_df %>% group_by(Title, date, CheckoutMonth)  %>% filter(str_detect(Title, "Beginning")) %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))

# make a dataset for or all Series of Unfortunate Events audiobook checkouts in 2022-23
SOUE_audio_df <- ls_book_df %>% filter(str_detect(Title, "Series")) %>% filter(str_detect(Title, "Unfortunate")) %>% filter(MaterialType == "AUDIOBOOK")
audio_checkouts <- SOUE_audio_df %>% group_by(Title)  %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))
checkouts_bb_a <- SOUE_audio_df %>% group_by(Title, date, CheckoutMonth)  %>% filter(str_detect(Title, "Beginning")) %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))

# Find most checked out ebook/audiobook + num checkouts
most_checkouts_ebook <- checkouts %>% filter(checkout_count == max(checkout_count, na.rm = TRUE)) %>% pull(Title)
amount_checkouts <- checkouts %>% filter(checkout_count == max(checkout_count, na.rm = TRUE)) %>% pull(checkout_count)

most_checkouts_audiobook <- audio_checkouts  %>% filter(checkout_count == max(checkout_count, na.rm = TRUE)) %>% pull(Title)
amount_audio_checkouts <- audio_checkouts  %>% filter(checkout_count == max(checkout_count, na.rm = TRUE)) %>% pull(checkout_count)

# first book(The Bad Beginning) month/year with highest ebook checkouts
bb_highest_month <- SOUE_df %>% filter(str_detect(Title, "Beginning")) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutMonth)
bb_highest_year <- SOUE_df %>% filter(str_detect(Title, "Beginning")) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)
 
#first book(The Bad Beginning) month/year with highest audiobook checkouts 
bb_highest_audio_month <- SOUE_audio_df %>% filter(str_detect(Title, "Beginning")) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutMonth)
bb_highest_audio_year <- SOUE_audio_df %>% filter(str_detect(Title, "Beginning")) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)

# Total checkouts for audiobooks and ebooks per month, table + total num
total_checkouts <- SOUE_df %>% group_by(date) %>% summarize(Checkouts = sum(Checkouts, na.rm = TRUE))
total_checkout_num <- SOUE_df %>% summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>% pull(Checkouts)

total_audio_checkouts <- SOUE_audio_df %>% group_by(date) %>% summarize(Checkouts = sum(Checkouts, na.rm = TRUE))
total_audio_checkout_num <- SOUE_audio_df %>% summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>% pull(Checkouts)

# change in (The Bad Beginning) ebook checkouts over time
SOUE_df <- SOUE_df %>% filter(str_detect(Title, "Beginning")) %>% mutate(checkout_change = Checkouts - lag(Checkouts))
change_in_checkouts_e <- SOUE_df %>% summarize(checkout_change = mean(checkout_change, na.rm = TRUE)) %>% pull(checkout_change)

SOUE_audio_df <- SOUE_audio_df %>% filter(str_detect(Title, "Beginning")) %>% mutate(checkout_change = Checkouts - lag(Checkouts))
change_in_checkouts_a <- SOUE_audio_df %>% summarize(checkout_change = mean(checkout_change, na.rm = TRUE)) %>% pull(checkout_change)
