#862300

#Pirma uzduotis

setwd("C:/Users/Rokas/Desktop/KTU-duomenu-vizualizacija")
data = read.csv("lab_sodra.csv")
library(dplyr)
data = data %>%
  filter(ecoActCode == 862300)
unique(data$ecoActCode)
options(scipen=999)
hist(data$avgWage, main = "Histogram of Average Wage", xlab = "Average Wage")

#Antra uzduotis

data$month = as.Date(paste0(as.character(data$month), '01'), format = '%Y%m%d')
top = data %>%
  group_by(name) %>%
  select(name, avgWage, month) %>%
  mutate(perMetus = tapply(avgWage, name, sum))
top = top %>%
  select(name, perMetus) %>%
  arrange(desc(perMetus)) %>%
  unique %>%
  head(5)
data2 = data %>%
  filter(name %in% top$name)
library(ggplot2)
ggplot(data2, aes(x = month, y = avgWage, group = name)) +
  geom_line(aes(col = name), size = 1) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  labs(x = "Month", y = "Average Wage", color = "Company") +
  theme_classic() +
  theme(legend.position = "none")

#Trecia uzduotis

apdrausti = data2 %>%
  group_by(name) %>%
  mutate(did = tapply(numInsured, name, max)) %>%
  select(name, did) %>%
  unique
apdrausti[1:5, 1] = c("Denticija", "Prodenta", "Vilniaus implantologijos centro klinika", "Dr. G. Kobs odontologijos klinika",
                      "Implantologijos centras Siauliuose")
names(apdrausti)[1] = "Company"
ggplot(apdrausti, aes(x = reorder(Company, -did), y = did)) +
  geom_col(aes(col = Company, fill = Company)) +
  labs(x = "Company", y = "Insured") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())