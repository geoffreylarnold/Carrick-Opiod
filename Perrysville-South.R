require(dplyr)
require(readr)

perry <- read_csv("perrysville-south.csv")

# zack @ stackoverflow
not_all_na <- function(x) any(!is.na(x))

# Filter to streets that are near north charles
pf <- perry %>%
  # filter(PROPERTYADDRESS__asmt %in% c("ELLIS ST", "ELLZEY ST", "LEGION WAY", "LELAND ST",  "MAPLE AVE", "N CHARLES ST",  "NORWOOD AVE",  "PERRYSVILLE AVE", "SHELTON AVE")) %>%
  select_if(not_all_na) %>%
  mutate(YEAR = format(SALEDATE__sales, "%Y"),
         CLOSE_TO_SITE = ifelse((PROPERTYHOUSENUM__asmt > 2800 & PROPERTYHOUSENUM__asmt < 2950 & PROPERTYADDRESS__asmt == "N CHARLES ST") | (PROPERTYHOUSENUM__asmt > 2618 & PROPERTYHOUSENUM__asmt < 2638 &  PROPERTYADDRESS__asmt == "ELLIS ST") | (PROPERTYHOUSENUM__asmt > 3 & PROPERTYHOUSENUM__asmt < 25 &  PROPERTYADDRESS__asmt == "ELLEZY ST") | (PROPERTYHOUSENUM__asmt > 11 & PROPERTYHOUSENUM__asmt < 24 &  PROPERTYADDRESS__asmt == "LEGION WAY") | (PROPERTYHOUSENUM__asmt > 2606 & PROPERTYHOUSENUM__asmt < 2729 &  PROPERTYADDRESS__asmt == "LELAND ST") | (PROPERTYHOUSENUM__asmt > 2499 & PROPERTYHOUSENUM__asmt < 2648 &  PROPERTYADDRESS__asmt == "MAPLE AVE") | (PROPERTYHOUSENUM__asmt > 2600 & PROPERTYHOUSENUM__asmt < 2663 &  PROPERTYADDRESS__asmt == "NORWOOD AVE") | (PROPERTYHOUSENUM__asmt > 2449 & PROPERTYHOUSENUM__asmt < 2659 &  PROPERTYADDRESS__asmt == "PERRYSVILLE AVE") | (PROPERTYHOUSENUM__asmt > 2629 & PROPERTYHOUSENUM__asmt < 2662 &  PROPERTYADDRESS__asmt == "SHELTON AVE"), TRUE, FALSE))

# Table of Sales by Year
sales_tbl1 <- pf %>%
  filter(!is.na(YEAR) & PRICE__sales != 1) %>%
  group_by(YEAR, CLOSE_TO_SITE) %>%
  summarise(avg_price = mean(PRICE__sales, na.rm = T), sales_count = n(),
            avg_assessment = mean(COUNTYTOTAL__asmt, na.rm = T))

assessment_tbl <- pf %>%
  group_by(CLOSE_TO_SITE) %>%
  summarise(overall_assessment = mean(COUNTYTOTAL__asmt, na.rm = T))

joined <- sales_tbl1 %>%
  left_join(assessment_tbl, by = "CLOSE_TO_SITE")

write.csv(joined, "comparison_sales.csv")
write.csv(assessment_tbl, "assessment12.scv")

require(ggplot2)

ggplot(data = sales_tbl1, aes(x = YEAR, y = avg_price, group = CLOSE_TO_SITE, colour = CLOSE_TO_SITE)) +
  geom_point() +
  geom_line()


ggsave("plot", width = 5, height = 5, 
       units = "in", # other options c("in", "cm", "mm"), 
       dpi = 300)
