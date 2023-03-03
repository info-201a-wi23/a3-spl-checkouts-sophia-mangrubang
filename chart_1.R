spl_data <- read.csv("~/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

library("dplyr")
library("stringr")
library("ggplot2")

ls_book_df <- spl_data %>% filter(str_detect(Creator, "Lemony")) %>% filter(str_detect(Creator, "Snicket"))
ls_book_df <- ls_book_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
ls_book_df$date <- as.Date(ls_book_df$date, format = "%Y-%m-%d")

# Create a dataset with total checkouts per month for the first eBook and audiobook in the series (The Bad Beginning)
checkouts_bb_all <- ls_book_df %>% group_by(Title, date, CheckoutMonth)  %>% filter(str_detect(Title, "Beginning")) %>% summarize(checkout_count = sum(Checkouts, na.rm = TRUE))

ggplot(checkouts_bb_all) +
  geom_line(aes(x = date, y = checkout_count, color = Title)) + labs(title = "Checkouts of Bad Beginning: Series of Unfortunate Events Book 1, eBook vs Audiobook 2022-2023", x = "Month (2022-2023)", y = "Number of Checkouts") + scale_x_date(date_labels= month.abb, date_breaks = "1 month") + scale_y_continuous(breaks = seq(0, 20, 1)) + labs(colour= "Version of Books")
