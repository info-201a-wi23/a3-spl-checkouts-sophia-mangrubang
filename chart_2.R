spl_data <- read.csv("~/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

library("dplyr")
library("stringr")
library("ggplot2")

SOUE_df <- ls_book_df %>% filter(str_detect(Title, "Series")) %>% filter(str_detect(Title, "Unfortunate")) %>% filter(MaterialType == "EBOOK")
SOUE_audio_df <- ls_book_df %>% filter(str_detect(Title, "Series")) %>% filter(str_detect(Title, "Unfortunate")) %>% filter(MaterialType == "AUDIOBOOK")

total_checkouts <- SOUE_df %>% group_by(date) %>% summarize(Checkouts = sum(Checkouts, na.rm = TRUE))

total_audio_checkouts <- SOUE_audio_df %>% group_by(date) %>% summarize(Checkouts = sum(Checkouts, na.rm = TRUE))

total_checkouts_ae <- left_join(total_checkouts, total_audio_checkouts, by = "date")

colnames(total_checkouts_ae)[2] = "total_eBooks_checkouts"
colnames(total_checkouts_ae)[3]= "total_audiobooks_checkouts"

ggplot(total_checkouts_ae) +
  geom_line(aes(x = date, y = total_eBooks_checkouts, color = "eBooks from the series")) + geom_line(aes(x = date, y = total_audiobooks_checkouts, color = "audiobooks from the series")) + labs(title = "Overall Checkouts of All eBooks vs Audiobooks in the Series 2022-2023", x = "Month (2022-2023)", y = "Number of Checkouts") + scale_x_date(date_labels= month.abb, date_breaks = "1 month") + scale_y_continuous(breaks = seq(0, 100, 10)) + labs(colour= "Version of Books")
