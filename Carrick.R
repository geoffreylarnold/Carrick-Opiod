require(dplyr)
require(readxl)

carrick <- read_excel("Parcel Data Carrick.xlsx")

carrickf <- carrick %>%
  mutate(YEAR = format(SALEDATE__sales, "%Y"),
         CLOSE_TO_SITE = (`PROPERTYHOUSENUM__asmt` >= 2500 & `PROPERTYHOUSENUM__asmt` <= 2714 & `PROPERTYADDRESS__asmt` == "BROWNSVILLE RD") |
           (`PROPERTYHOUSENUM__asmt` >= 117 & `PROPERTYHOUSENUM__asmt` <= 302 & `PROPERTYADDRESS__asmt` == "SPENCER AVE") |
           (`PROPERTYHOUSENUM__asmt` >= 101 & `PROPERTYHOUSENUM__asmt` <= 307 & `PROPERTYADDRESS__asmt` == "KIRK AVE") |
           (`PROPERTYHOUSENUM__asmt` >= 2551 & `PROPERTYHOUSENUM__asmt` <= 2767 & `PROPERTYADDRESS__asmt` == "CHURCHVIEW AVE") |
           (`PROPERTYHOUSENUM__asmt` >= 1014 & `PROPERTYHOUSENUM__asmt` <= 1048 & `PROPERTYADDRESS__asmt` == "SANKEY CT") |
           (`PROPERTYHOUSENUM__asmt` >= 296 & `PROPERTYHOUSENUM__asmt` <= 448 & `PROPERTYADDRESS__asmt` == "RUTHWOOD AVE"))

sales_tbl1 <- carrickf %>%
  filter(!is.na(YEAR) & PRICE__sales != 1) %>%
  group_by(YEAR, CLOSE_TO_SITE) %>%
  summarise(avg_price = mean(PRICE__sales, na.rm = T), sales_count = n(),
            avg_assessment = mean(COUNTYTOTAL__asmt, na.rm = T))

assessment_tbl <- carrickf %>%
  group_by(CLOSE_TO_SITE) %>%
  summarise(overall_assessment = mean(COUNTYTOTAL__asmt, na.rm = T))

joined <- sales_tbl1 %>%
  left_join(assessment_tbl, by = "CLOSE_TO_SITE")

write.csv(joined, "carrick_comparison_sales.csv")
write.csv(assessment_tbl, "carrick_assessment12.csv")
