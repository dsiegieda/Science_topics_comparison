library(tidyverse)
library(rentrez)
library(glue)
library(wesanderson)


#First define the years
year <- 2000:2021

#Then define keywords that we are interested in
myco_search <- glue("mycobiome AND plant AND {year}[PDAT]")
myco_hum_search <- glue("mycobiome AND human AND {year}[PDAT]")
micro_search <- glue("microbiome AND plant AND {year}[PDAT]")
micro_hum_search <- glue("microbiome AND human AND {year}[PDAT]")
all_search <- glue("{year}[PDAT]")
cancer_search <- glue("cancer AND {year}[PDAT]")

#Now download the data and create a data frame with all of the data with 
#number of papers published in a particular year
search_counts <- tibble(year = year,
                        myco_search = myco_search,
                        micro_search=micro_search,
                        micro_hum_search=micro_hum_search,
                        myco_hum_search=myco_hum_search,
                        cancer_search = cancer_search,
                        all_search = all_search) %>%
  mutate(myco = map_dbl(myco_search, ~entrez_search(db="pubmed", term=.x)$count),
         myco_hum = map_dbl(myco_hum_search, ~entrez_search(db="pubmed", term=.x)$count),
         micro = map_dbl(micro_search, ~entrez_search(db="pubmed", term=.x)$count),
         micro_hum = map_dbl(micro_hum_search, ~entrez_search(db="pubmed", term=.x)$count),
         cancer = map_dbl(cancer_search, ~entrez_search(db="pubmed", term=.x)$count),
         all = map_dbl(all_search, ~entrez_search(db="pubmed", term=.x)$count)
  )


#Prepare a data frame for ggplot (realtive number and pivot longer)
search_counts <- search_counts %>%
  select(year, myco, myco_hum, micro, micro_hum, cancer, all) %>%
  filter(year != 2022) %>%
  mutate(rel_myco = 100 * myco / all,
         rel_myco_hum = 100 * myco_hum / all,
         rel_micro = 100 * micro / all,
         rel_micro_hum = 100 * micro_hum / all,
         rel_cancer = 100 * cancer / all) %>%
  select(year, starts_with("rel")) %>%
  pivot_longer(-year)

#Finally, create the plot
search_counts %>% 
  ggplot(aes(x=year, y=value, group=name, color=name)) +
  geom_line(size=1) +
  scale_y_log10(limits=c(NA, 100),
                breaks=c(0.0001, 0.001,0.01, 0.1, 1, 10, 100),
                labels=c('0.0001%','0.001%',"0.01%", "0.1%", "1%", "10%", "100%")) +
  labs(x="Year", y="Percentage of papers in PubMed") +
  scale_color_manual(name=NULL,
                     label=c('Cancer','Human microbiome', "Plant microbiome",'Human mycobiome',"Plant mycobiome"),
                     values=c("#a6a6a6", '#b3e6b3', "#339933",'#b3d1ff','#0066ff'),
                     #values=wes_palette(n=5, name='Darjeeling1'),
                     breaks=c('rel_cancer','rel_micro_hum',"rel_micro",'rel_myco_hum',"rel_myco")) +
  theme_classic() +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=10))+
  theme(legend.position=c(0.15, 0.65))

