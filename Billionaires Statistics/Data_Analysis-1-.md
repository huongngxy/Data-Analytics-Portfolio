---
title: "EDA of World's Billionaires (2023)"
author: "Huong Nguyen"
date: "2023-12-28"
output:
  bookdown::html_document2: 
    css: style.css
    theme: flatly
    highlight: tango
    toc: yes
    toc_float: yes
    df_print: paged
    code_folding: hide 
    fig_caption: yes
    keep_md: true
    keep_tex: true
---



# 1. Introduction

The dataset contains statistics on the world's billionaires in 2023 and is obtained from [Kaggle](https://www.kaggle.com/datasets/nelgiriyewithana/billionaires-statistics-dataset).\ 
Provided are the billionaires' demographic information, industries and businesses.The dataset also includes several selected macroeconomic indices such as the CPI, GDP, gross teritary education enrollment, gross primary education enrollment, life expectancy, tax revenue, tax rate and population. All these indices refer to the country, in which the billionaire is living in.\



# 2. Distribution 

## 2.1 By Industry 


```r
factor_category <- factor(df_Billionaires$category)
category1 <- as.data.frame(table(factor_category)) 

pl_cat <- ggplot(category1, aes(x = reorder(factor_category, -Freq),
                                y = Freq)) + 
  geom_bar(stat = 'identity', aes(fill = Freq)) +
  scale_fill_viridis() +     #from package "viridis"
  geom_text(aes(label= Freq), vjust=-0.3, size=2.5) +
  theme_bw() +
  labs(color = NULL,
       y = "Frequency", 
       x = "Industries",
       title = "Figure 1: Distribution of world's billionaires by industry") + 
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 70,
                                   hjust = 1)) 
pl_cat
```

<img src="Data_Analysis-1-_files/figure-html/Figure_1-1.png" width="100%" height="100%" style="display: block; margin: auto;" />

The Finance & Investements industry has the highest number of billionaires (372). Other most populous sectors among the billionaires list are Manufacturing (372), Technology (314), Fashion & Retail (266) and Food & Beverage (212).\

## 2.2 By Gender


```r
factor_ges <- factor(df_Billionaires$gender)
ges <- as.data.frame(table(factor_ges)) %>%
  rename(Anzahl = Freq) %>%
  mutate(Anteil = Anzahl/2640) %>%
  mutate(Anteilp = scales::percent(Anteil, accuracy = 0.01)) 

plot_ges <- ggplot(ges, aes(x = "", y = Anteilp, fill = factor_ges)) + 
  geom_bar(stat = 'identity', width = 0.6) + 
  geom_text(aes(label = Anteilp), 
            position = position_stack(vjust = 0.5), size= 4) +
  theme_void() + 
  guides(fill = guide_legend(title = "Gender"))  + 
  coord_polar("y", start=0) + 
  labs(color = NULL,
       title = "Figure 2: Distribution of world's billionaires by gender") 
plot_ges 
```

<img src="Data_Analysis-1-_files/figure-html/Figure-2 -1.png" width="100%" height="100%" style="display: block; margin: auto;" />

The majority of the billionaires (87.23%) are male, whereas only 12.77% are female.\ 

## 2.3 By Country


```r
factor_country <- factor(df_Billionaires$country)
country <- as.data.frame(table(factor_country)) 
country_sorted <- country[order(country$Freq),] %>%
  tail( n= 10L)

cols1 <- c( "pink",
            "#BF87B3","#BF87B3","#BF87B3","#BF87B3","#BF87B3",
            "#8255a1",
            "#8255a1",
            rgb(72/255, 32/255, 143/255),
            rgb(72/255, 32/255, 143/255))

plot_country <- ggplot(country_sorted, aes(x = reorder(factor_country, -Freq, 
                                                       decreasing = FALSE),
                                           y = Freq)) + 
  geom_bar(stat = 'identity', fill = cols1) + 
  geom_text(aes(label= Freq), vjust=-0.3, size=3) +
  theme_bw() + 
  labs(color = NULL,
       y = "Number of Billionaires", 
       x = "Country",
       title = "Figure 3: Distribution of world's billionaires by country") + 
 theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, size = 9),
       axis.text.y = element_text(size = 9)) 
plot_country
```

<img src="Data_Analysis-1-_files/figure-html/Figure 3-1.png" width="100%" height="100%" style="display: block; margin: auto;" />

The United States have the most billionaires, followed by China, India and Germany.\ 

## 2.4 By Status 


```r
factor_status <- factor(df_Billionaires$selfMade)
status <- as.data.frame(table(factor_status)) %>%
  mutate(Anteil = Freq/2640) %>%
  mutate(Anteilp = scales::percent(Anteil, accuracy = 0.01))

plot_status <- ggplot(status, aes(x = "", y = Anteilp, fill = factor_status)) + 
  geom_bar(stat = 'identity') +
  theme_void() + 
  geom_text(aes(label = Anteilp),
            position = position_stack(vjust = 0.5), size= 4.5) +
  guides(fill = guide_legend(title = "Status")) +
  scale_fill_manual(labels = c("inherited", "self-made"), 
                    values=c("beige", "coral1")) + 
  coord_polar("y", start=0) + 
  labs(color = NULL,
       title = "Figure 4: Distribution of word's billionaires by status")
plot_status
```

<img src="Data_Analysis-1-_files/figure-html/figure 4-1.png" width="100%" height="100%" style="display: block; margin: auto;" />

The largest part (68.64%) of the billionaires are self-made, whereas the rest 31.36% are inherited.\

## 2.5 By Age 

<img src="../../../../WS 23,24/Datenanalyse/Age distribution.png" width="100%" />

Based on the normal curve, the population's age is approx. normally distributed, since the most common age falls between 55 and 60, but not at the peak of the normal curve (between 60-65 or 70-75).\

## 2.6 Top 10 richest


```r
top10 <- df_Billionaires %>% 
  head(n = 10L)

plot_top10 <- ggplot(top10, aes(x = reorder(personName, finalWorth_bln), 
                                y = finalWorth_bln, fill = category)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = finalWorth_bln),hjust = -0.5, size = 1.8)  + 
  guides(fill = guide_legend(title = "Category")) + 
  theme_bw() +
  coord_flip() + 
  labs(color = NULL, 
       y = "Net wealth in billion USD", 
       x = NULL,
       title = "Figure 6: Top 10") + 
  theme(strip.background = element_blank(),
        axis.text.y = element_text(size = 7.5),
        legend.key.size = unit(0.3, 'cm'),
        legend.text = element_text(size = 7))
plot_top10
```

<img src="Data_Analysis-1-_files/figure-html/figure 6-1.png" width="100%" height="100%" style="display: block; margin: auto;" />

As of 04.04.2023, Bernard Arnault & family from the fashion & retail industry rank first among the world's 10 wealthiest people with the highest net worth of $211 billion.\
Elon Musk is the world's second wealthiest person whose net worth stems from the automotive industry.\
Technology is most populous sector among the world's top 10 since it has 4 billionaires named in the list, including Jeff Bezos ($114 billion), Larry Ellison ($107 billion), Bill Gates ($104 billion) and Steve Ballmer ($80.7 billion).\

# 3. Crosstabulations

## 3.1 Net Worth and Industries


```r
netw_industry <- df_Billionaires %>%
  group_by(category) %>%
  summarise(summe = sum(finalWorth_bln)) %>%
  mutate(Pct = summe/sum(summe)) %>%
  mutate(Per = scales::percent(Pct, accuracy = 0.01))

netw_industry_sorted <- netw_industry[order(-netw_industry$summe),]

plot_netw_industry <- ggplot(netw_industry_sorted, aes(x = reorder(category, -summe, decreasing = FALSE), y = summe)) +
  geom_bar(stat = "identity", fill = "#F5CD7A") +
  geom_text(aes(label = summe), vjust = -0.5, size = 2.8) +
  theme_bw() + 
  labs(y = "Aggregated net wealth in $bln", 
       x = "Industry",
       title = "Figure 7: Aggregated Net Worth by Industry") + 
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))
plot_netw_industry
```

<img src="Data_Analysis-1-_files/figure-html/figure 7-1.png" width="100%" height="100%" style="display: block; margin: auto;" />
Technology, Fashion & Retail, Finance & Investments, Manufacturing and Food & Beverage are the top five industries that have the highest aggregated net worth. This can be explained by the fact that these five industries have the highest number of billionaires (as Figure \ref{fig:Figure_1} suggests). 


```r
df_anova <- df_Billionaires %>%
  group_by(industries) %>%
  summarize(min = min(finalWorth_bln),
            max = max(finalWorth_bln),
            median = median(finalWorth_bln),
            mean = mean(finalWorth_bln))
df_anova
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["industries"],"name":[1],"type":["chr"],"align":["left"]},{"label":["min"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["max"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["median"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["mean"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"Automotive","2":"1.0","3":"180.0","4":"2.60","5":"7.195890"},{"1":"Construction & Engineering","2":"1.0","3":"13.7","4":"2.10","5":"2.633333"},{"1":"Diversified","2":"1.0","3":"83.4","4":"2.30","5":"4.840642"},{"1":"Energy","2":"1.0","3":"25.5","4":"2.45","5":"4.535000"},{"1":"Fashion & Retail","2":"1.0","3":"211.0","4":"2.50","5":"6.386466"},{"1":"Finance & Investments","2":"1.0","3":"106.0","4":"2.60","5":"4.314785"},{"1":"Food & Beverage","2":"1.0","3":"68.0","4":"2.50","5":"4.515094"},{"1":"Gambling & Casinos","2":"1.0","3":"35.0","4":"2.80","5":"4.820000"},{"1":"Healthcare","2":"1.0","3":"22.6","4":"2.10","5":"3.200000"},{"1":"Logistics","2":"1.0","3":"39.1","4":"3.00","5":"5.987500"},{"1":"Manufacturing","2":"1.0","3":"29.7","4":"2.00","5":"3.145062"},{"1":"Media & Entertainment","2":"1.0","3":"94.5","4":"2.50","5":"4.697802"},{"1":"Metals & Mining","2":"1.1","3":"27.0","4":"2.60","5":"6.037838"},{"1":"Real Estate","2":"1.0","3":"29.5","4":"2.30","5":"3.406218"},{"1":"Service","2":"1.0","3":"21.2","4":"2.60","5":"3.271698"},{"1":"Sports","2":"1.0","3":"13.3","4":"2.10","5":"3.448718"},{"1":"Technology","2":"1.0","3":"114.0","4":"2.20","5":"5.980573"},{"1":"Telecom","2":"1.0","3":"93.0","4":"2.80","5":"6.564516"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
plot_anova <- ggplot(df_anova, aes(x = reorder(industries,-mean,
                                               decreasing = FALSE), y = mean)) +
  geom_bar(stat = "identity",aes(fill = mean)) + 
  scale_fill_viridis(option = "magma", name = paste0("average", "\n","net worth"))+ 
  theme_bw() + 
  geom_text(aes(label = scales::number(mean, accuracy = 0.01)),
            vjust = -0.3, size = 3) +
  labs(y = "average net worth $bn",
       x = "industry",
       title = "Figure 8: Average net worth by industry") +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))
 
plot_anova
```

<img src="Data_Analysis-1-_files/figure-html/figure 8-1.png" width="100%" height="100%" style="display: block; margin: auto;" />
The automotive sector has the highest average net worth ($7.20 billion) among other sectors.\
That the results differ from Figures 1 and 10 can be explained by the proportion between the aggregated net worth and the number of billionaires within the corresponding industry. 


## 3.2 Industries and Age groups 


```r
age_groups <- ifelse(df_Billionaires$age < 35, "under 35",
                    ifelse(df_Billionaires$age < 55, "35 to under 55",
                           ifelse(df_Billionaires$age < 75, "55 to under 75", 
                                  "over 75")))
df_Billionaires$agegroups <- age_groups
df_agegroups_industries <- as.data.frame(xtabs(~df_Billionaires$industries + 
                                                 df_Billionaires$agegroups))


v1 <- df_agegroups_industries$Freq[1:18] / sum(df_agegroups_industries$Freq[1:18]) 
v2 <- df_agegroups_industries$Freq[19:36] / sum(df_agegroups_industries$Freq[19:36])
v3 <- df_agegroups_industries$Freq[37:54] / sum(df_agegroups_industries$Freq[37:54])
v4 <- df_agegroups_industries$Freq[55:72] / sum(df_agegroups_industries$Freq[55:72])                                  

v5 <- c(v1,v2,v3,v4)
df_agegroups_industries$v5 <- v5            

df_ageindus_final <- df_agegroups_industries %>%
  mutate(v5= scales::percent(v5, accuracy = 0.01))

plot_treemap <- df_ageindus_final %>%
  ggplot(aes(area = Freq, fill = df_Billionaires.industries, 
             group = df_Billionaires.agegroups, subgroup = df_Billionaires.industries,
             label = paste0(df_Billionaires.industries, "\n",v5))) + 
  geom_treemap() + 
  facet_wrap(~factor(df_Billionaires.agegroups, c("under 35", "35 to under 55", 
                                                  "55 to under 75", "over 75")))  + 
  theme(strip.text = element_text(face = "bold", size = 10, color = "black"),
        legend.key.size = unit(0.5, 'cm'),
        legend.text = element_text(size = 7))+ 
  geom_treemap_text(grow = F, reflow = T, size = 7, colour = "black") +
  geom_treemap_subgroup_border(colour = "white") + 
  scale_fill_tableau(palette = "Hue Circle") + 
  guides(fill = guide_legend(title = "Industry")) +
  labs(title = "Percentage of different industries in different age groups") 
plot_treemap 
```

<img src="Data_Analysis-1-_files/figure-html/figure 9-1.png" width="100%" height="100%" style="display: block; margin: auto;" />

It is observed that the percentage of billionaires involved in the technology sector declines across the age groups (from 28%, 25.10%, 10.06% to 4.89%). Younger billionaires tend to be more involved in the tech industry than older ones.\
The opposite applies to Investment and Finance, where the percentage rises as age increases (from 0%, 13.62%, 14.33% to 14.53%). Older billionaires seem to be more active in the investment and finance industry than younger ones.\
These observations suggest that there could be a correlation between age and industry. 
By computing the chi-square-test from inferential statistics, the observations can be verified.\


<img src="../../../../WS 23,24/Datenanalyse/All plots/Chi Square Tests.png" width="80%" height="80%" style="display: block; margin: auto;" />
The p-value of 0.000 - displayed in row "Pearson Chi-Square" under "asymptotic significance (2-sided)" - is below the significance level under the assumption that the significance level is set at 0.05. The null hypothesis is therefore rejected and the observations are true. There is a statistically significant association between age groups and industries.\ 

