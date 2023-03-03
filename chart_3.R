spl_data <- read.csv("~/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

library("dplyr")
library("stringr")
library("ggplot2")

ls_book_df <- spl_data %>% filter(str_detect(Creator, "Lemony")) %>% filter(str_detect(Creator, "Snicket"))
ls_book_df <- ls_book_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
ls_book_df$date <- as.Date(ls_book_df$date, format = "%Y-%m-%d")

SOUE_df <- ls_book_df %>% filter(str_detect(Title, "Series")) %>% filter(str_detect(Title, "Unfortunate")) %>% filter(MaterialType == "EBOOK")

# create a dataset of total checkouts for each book in the series (eBooks)
checkouts <- SOUE_df %>% group_by(Title)  %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))
checkouts_barchart <- checkouts %>% mutate(short_name = gsub("^.*?, ", "", checkouts$Title))

ggplot(checkouts_barchart) + geom_col(aes(x = short_name, y = checkout_count, fill = Title )) + labs(title = "All eBooks in the Series and Their Total Checkouts in 2022-2023", x = "Book", y = "Number of Checkouts" ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + scale_y_continuous(breaks = seq(0, 80, 10)) + scale_fill_discrete(name = "Title of eBook")

