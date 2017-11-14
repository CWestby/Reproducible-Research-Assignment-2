# Read In Data
fileURL   <-  'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download.file(fileURL, destfile="StormData.csv.bz2", method = "curl")
Storm_data    <-  read.csv("StormData.csv.bz2")
Storm_data$BGN_DATE <- as.Date(Storm_data$BGN_DATE, "%m/%d/%Y %H:%M:%S")


Top_20_Fatality_Event <- Storm_data %>%
  arrange(desc(FATALITIES)) %>%
  head(20)

Top_Fatality_Event <- Top_Fatality_Event[1:20, c("FATALITIES", "EVTYPE")]

Top_Injury_Event <- Storm_data %>%
  arrange(desc(INJURIES))

Top_Injury_Event[1:20, c("INJURIES", "EVTYPE")]

Top_injury <- Storm_data %>%
  group_by(EVTYPE) %>%
  summarize(Total_injury = sum(INJURIES)) %>%
  arrange(desc(Total_injury)) %>%
  head(10)

ggplot(Top_injury, aes(x = EVTYPE, y = Total_injury)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90))


Storm_data_subset <- Storm_data %>%
  filter(EVTYPE %in% Average_fatality$EVTYPE)

head(Average_injury, 10)

Top_fatality <- Storm_data %>%
  group_by(EVTYPE) %>%
  summarize(Total_fatality = sum(FATALITIES)) %>%
  arrange(desc(Total_fatality)) %>%
  head(10)

ggplot(Top_fatality, aes(x = EVTYPE, y = Total_fatality)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90))

head(Average_fatality, 10)


get_exponent <- function(exponent) {
exponent     <-  as.character(exponent)
exponent[toupper(exponent) == "H"]    <-  "2"
exponent[toupper(exponent) == "K"]    <-  "3"
exponent[toupper(exponent) == "M"]    <-  "6"
exponent[toupper(exponent) == "B"]    <-  "9"
exponent[is.na(exponent)]             <-  "0"
exponent     <-  as.numeric(exponent)
}

Storm_data$PROPDMGEXP <- get_exponent(Storm_data$PROPDMGEXP)
Storm_data$CROPDMGEXP <- get_exponent(Storm_data$CROPDMGEXP)

Storm_data <- Storm_data %>%
  mutate(Property_damage = PROPDMG * 10^PROPDMGEXP,
         Crop_damage = CROPDMG * 10^CROPDMGEXP)

Top_Crop_Event <- Storm_data %>%
  arrange(desc(Crop_damage)) 

Top_Crop_Event[1:20, c("Crop_damage", "EVTYPE")]

Top_Property_Event <- Storm_data %>%
  arrange(desc(Property_damage))

Top_Property_Event[1:20, c("Property_damage", "EVTYPE")]

Top_crop <- Storm_data %>%
  group_by(EVTYPE) %>%
  summarize(Total_crop = sum(Crop_damage)) %>%
  arrange(desc(Total_crop)) %>%
  head(10)

ggplot(Top_crop, aes(x = EVTYPE, y = Total_crop)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90))


head(Average_crop, 10)

Top_property <- Storm_data %>%
  group_by(EVTYPE) %>%
  summarize(Total_property = sum(Property_damage)) %>%
  arrange(desc(Total_property)) %>%
  head(10)

ggplot(Top_property, aes(x = EVTYPE, y = Total_property)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90))
