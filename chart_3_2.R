spl_data <- read.csv("~/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

library("dplyr")
library("stringr")
library("ggplot2")

SOUE_audio_df <- ls_book_df %>% filter(str_detect(Title, "Series")) %>% filter(str_detect(Title, "Unfortunate")) %>% filter(MaterialType == "AUDIOBOOK")

audio_checkouts <- SOUE_audio_df %>% group_by(Title)  %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))
audio_checkouts_barchart <- audio_checkouts %>% mutate(short_name = gsub("^.*?, ", "", audio_checkouts$Title))
View(audio_checkouts_barchart)

ggplot(audio_checkouts_barchart) + geom_col(aes(x = short_name, y = checkout_count, fill = Title )) + labs(title = "All Audiobooks in the Series and Their Total Checkouts in 2022-2023", x = "Book Number", y = "Number of Checkouts" ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + scale_y_continuous(breaks = seq(0, 250, 10)) + scale_fill_discrete(name = "Title of Audiobook")
