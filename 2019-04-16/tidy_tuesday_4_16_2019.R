# Kathleen Cachel

library(tidyverse)
library(wesanderson)
women_research_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/Economist_women-research.csv")

research_titles <- c("country",
  "Health sciences",
  "Physical sciences",
  "Engineering",
  "Computer science, maths",
  "Women inventors")

# remove rows with NA values
# update Column Names
women_research_clean <- women_research_raw %>% 
  na.omit() %>% 
  set_names(nm = research_titles) %>% 
  filter(country != "Country" & country != "Brazil" & country != "Denmark" & country !="Britain" & country != "France") %>% 
  gather(field, percent, `Health sciences`:`Women inventors`)

#make men version
men_research_clean <- women_research_clean %>%
  mutate(percent = 1 - as.numeric(percent))

#create new gender column
women_research_clean$gender <- "female"
men_research_clean$gender <- "amale"

#update type in women data frame
women_research_clean$percent <- as.numeric(women_research_clean$percent)

#union rows to make one big tidy data set
research_clean <- union(women_research_clean, men_research_clean)

  


#plotting
d <- with(research_clean, research_clean[order(country, gender, field),])
#d <- subset.data.frame(d, country == "Australia")
united_d <-  unite(d, field_gender, field, gender, sep = ".", remove = FALSE)

ggplot(data=united_d, aes(x=percent, y=field, fill=field_gender)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#efedf5", "#756bb1","#e5f5e0" , "#31a354","#deebf7", "#3182bd",
                               "#fde0dd", "#c51b8a","#fff7bc", "#d95f0e"))+
facet_grid(country ~ .)+

+
  coord_flip()

ggplot(data=united_d, aes(x=field, y=percent, fill=field_gender)) + 
  geom_bar(stat="identity") + 
  facet_grid(~country)+
   scale_fill_manual( values = c("#efedf5", "#756bb1","#e5f5e0" , "#31a354","#deebf7", "#3182bd",
                                 "#fde0dd", "#c51b8a","#fff7bc", "#d95f0e"))


breaks = c("Health sciences.female", "Health sciences.male", "Physical sciences.female"),



