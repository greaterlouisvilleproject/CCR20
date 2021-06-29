---
title: "Internet Access in Louisville"
author: "Nate Kratzer"
date: '2020-09-09'
output:
  html_document:
    self_contained: no
    toc: yes
    toc_float: yes
    code_folding: hide
  pdf_document:
    toc: yes
  word_document:
    toc: yes
---

# Digital Divide 







```r
#Filtering to just GLP peer MSAs
library(tidyverse)

df <- read_csv(
  "ipums_internet_access/glp_int.csv",
  col_types = cols(
    YEAR = col_double(),
    SAMPLE = col_double(),
    SERIAL = col_double(),
    CBSERIAL = col_double(),
    HHWT = col_double(),
    CLUSTER = col_double(),
    STATEFIP = col_double(),
    METRO = col_double(),
    MET2013 = col_double(),
    PUMA = col_double(),
    STRATA = col_double(),
    GQ = col_double(),
    CINETHH = col_double(),
    CILAPTOP = col_double(),
    CISMRTPHN = col_double(),
    CITABLET = col_double(),
    CIHISPEED = col_double(),
    CIDIAL = col_double(),
    PERNUM = col_double(),
    PERWT = col_double(),
    SEX = col_double(),
    AGE = col_double(),
    RACE = col_double(),
    RACED = col_double(),
    HISPAN = col_double(),
    HISPAND = col_double(),
    EDUC = col_double(),
    EDUCD = col_double(),
    POVERTY = col_double(),
    city = col_character()
  )
)

df_city <- df %>%
  filter(MET2013 %in% c(24340, 41180, 36420, 46140, 24860, 28940, 13820, 31140, 26900, 
                        28140, 36540, 24660, 16740, 18140, 17140, 34980, 32820)) %>%
  mutate(city = case_when(
    MET2013 == 24340 ~ "Grand Rapids",
    MET2013 == 41180 ~ "St. Louis",
    MET2013 == 36420 ~ "Oklahoma City",
    MET2013 == 46140 ~ "Tulsa",
    MET2013 == 24860 ~ "Greenville",
    MET2013 == 28940 ~ "Knoxville",
    MET2013 == 13820 ~ "Birmingham",
    MET2013 == 31140 ~ "Louisville",
    MET2013 == 26900 ~ "Indianapolis",
    MET2013 == 28140 ~ "Kansas City",
    MET2013 == 36540 ~ "Omaha",
    MET2013 == 24660 ~ "Greensboro",
    MET2013 == 16740 ~ "Charlotte",
    MET2013 == 18140 ~ "Columbus",
    MET2013 == 17140 ~ "Cincinnati",
    MET2013 == 34980 ~ "Nashville",
    MET2013 == 32820 ~ "Memphis",
    TRUE ~ NA_character_
  )) %>%
  write_csv("ipums_internet_access/glp_int_MSAs.csv")
```


## Overall Access

This report uses census Microdata from IPUMS to look at internet access in the Louisville MSA. Data is available from 2013 to 2018. About 1 in 10 households in Louisville have no internet access, and 3 in 10 do not have the high speed access necessary for working and learning at home. Access to high speed internet access has not improved in the past 6 years. Households also lack access to devices, with 15% of houses not having a computer or a tablet. Houses that do have computers or tablets are also unlikely to have enough devices for multiple people working from home (unfortunately the Census data only asks about having 1 or more device). 


```r
# Libraries
library(tidyverse)
library(survey)
library(glptools)
library(classInt)
library(ggthemes)
library(ggplot2)
library(rgdal)
library(sf)
library(magrittr)

library(showtext)
library(kableExtra)

showtext_auto()
font_add("Montserrat", "Black_wealth/Montserrat/Montserrat-Regular.ttf")
font_add("Montserrat Bold", "Black_wealth/Montserrat/Montserrat-SemiBold.ttf")

df <- read_csv(
  "ipums_internet_access/glp_int_MSAs.csv",
  col_types = cols(
    YEAR = col_double(),
    SAMPLE = col_double(),
    SERIAL = col_double(),
    CBSERIAL = col_double(),
    HHWT = col_double(),
    CLUSTER = col_double(),
    STATEFIP = col_double(),
    METRO = col_double(),
    MET2013 = col_double(),
    PUMA = col_double(),
    STRATA = col_double(),
    GQ = col_double(),
    CINETHH = col_double(),
    CILAPTOP = col_double(),
    CISMRTPHN = col_double(),
    CITABLET = col_double(),
    CIHISPEED = col_double(),
    CIDIAL = col_double(),
    PERNUM = col_double(),
    PERWT = col_double(),
    SEX = col_double(),
    AGE = col_double(),
    RACE = col_double(),
    RACED = col_double(),
    HISPAN = col_double(),
    HISPAND = col_double(),
    EDUC = col_double(),
    EDUCD = col_double(),
    POVERTY = col_double(),
    city = col_character()
  )
)

df <- df %>%
  mutate(
    int_acc = case_when(
      CINETHH == 0 ~ NA_real_,
      CINETHH == 1 ~ 1,
      CINETHH == 2 ~ 1,
      CINETHH == 3 ~ 0),
    hspd_int = case_when(
      CINETHH == 0 ~ NA_real_,
      CINETHH == 3 & CIHISPEED == 0 ~ 0, #seems like it's marked NA if the household doesn't have internet access. We want it to be no High Speed access
      CIHISPEED > 9 & CIHISPEED < 20 ~ 1,
      CIHISPEED == 20 ~ 0
    ),
    computer = case_when(
      CILAPTOP == 0 ~ NA_real_,
      CILAPTOP == 1 ~ 1,
      CILAPTOP == 2 ~ 0
    ),
    tablet = case_when(
      CITABLET == 0 ~ NA_real_,
      CITABLET == 1 ~ 1,
      CITABLET == 2 ~ 0
    ),
    comp_tab = case_when(
      computer == 1 | tablet == 1 ~ 1,
      computer == 0 & tablet == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    hspd_dev = case_when(
      comp_tab == 1 & hspd_int == 1 ~ 1,
      comp_tab == 0 | hspd_int == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )


# These questions are usually only NA for group quarters
# All the NA rows are the same for all categories in the Louisville sample
df_na <- df %>%
  filter(!is.na(int_acc))

svy_df <- svydesign(ids = ~ 1, weights = ~PERWT, data = df_na)

int_tbl <- svyby(~int_acc, ~YEAR+city, design = svy_df, svymean)

#Even accounting for not asking people without internet if they had hi speed internet 
df_na <- df %>%
  filter(!is.na(hspd_int))

svy_df <- svydesign(ids = ~ 1, weights = ~PERWT, data = df_na)

hspd_tbl <- svyby(~hspd_int, ~YEAR+city, design = svy_df, svymean)

df_na <- df %>%
  filter(!is.na(comp_tab) & !is.na(hspd_dev))

svy_df <- svydesign(ids = ~ 1, weights = ~PERWT, data = df_na)

hspd_dev_tbl <- svyby(~hspd_dev, ~YEAR+city, design = svy_df, svymean)
comp_tab_tbl <- svyby(~comp_tab, ~YEAR+city, design = svy_df, svymean)

ranking <- function(df, var, plot_title = "",
                    year = NULL, sex = "total", race = "total", peers = "Current",
                    order = "Descending",
                    y_title = "Percent", caption_text = "", subtitle_text = "",
                    bar_label = TRUE, sigfig = 3, accuracy = 0.1,
                    label_function, alternate_text = NULL,
                    ranking_colors = TRUE, text_size){

  # Copy variable var to a new column for use with the '$' operator
  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))
  df$var <- df[[var]]

  # Filter to sex, race, and year
  if ("sex" %in% names(df)) df <- df[df$sex == sex,]
  if ("race" %in% names(df)) df <- df[df$race == race,]
  if("year" %in% names(df)) {
    if (is.null(year)) year <- max(years_in_df(df, var))
    df <- df[df$year %in% year,]

    if (length(year) > 1) {
      df %<>%
        group_by_at(df %cols_in% c("MSA", "FIPS")) %>%
        summarise(var = mean(var, na.rm = TRUE)) %>%
        ungroup()
    }
  }

  # Add peer data if not already present
  if ("current" %not_in% names(df)) df %<>% pull_peers(add_info = T)

  # Filter to peer parameter
  if (peers %in% c("current", "Current"))   df %<>% filter(current == 1)
  if (peers %in% c("baseline", "Baseline")) df %<>% filter(baseline == 1)

  # Sort according to order parameter
  if (order %in% c("descending", "Descending")) df %<>% arrange(desc(var))
  if (order %in% c("ascending", "Ascending"))   df %<>% arrange(var)

  df %<>% filter(!is.na(var))

  # Create numbered city labels for left side of graph
  df %<>%
    mutate(
      rank = row_number(),
      names = paste0(rank, ". ", city))

  # Set bar colors
  if (ranking_colors) {

    # color_values <- c("#96ca4f", "#ffd600", "#db2834")
    # color_names <- c("green", "yellow", "red")
    # if (order %in% c("descending", "Descending")) {color_names  = rev(color_names)}
    # 
    # breaks <- classInt::classIntervals(na.omit(df$var), 3, style = "jenks")
    # df$color <- NA
    # df$color[df$var <= breaks$brks[2]] <- color_names[1]
    # df$color[df$var > breaks$brks[2] & df$var <= breaks$brks[3]] <- color_names[2]
    # df$color[df$var > breaks$brks[3]] <- color_names[3]
    
    color_values <- c("#d63631", "#323844")
    color_names <- c("gray", "red")
    
    df$color <- "gray"
    df$color[df$city == "Louisville"] <- "red"

  } else {
    df$color <- "blue"
    color_values <- "#f58021"
    color_names <- "blue"
  }

  if (order %in% c("descending", "Descending")) color_values = rev(color_values)

  # Create numeric labels
  if (!missing(label_function)) {
    label_text <- df$var %>% signif(sigfig) %>% label_function()
  } else if (y_title == "Dollars") {
    if (mean(df$var, na.rm = TRUE) > 10000) {
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = accuracy, scale = .001, suffix = "k")
    } else if (mean(df$var, na.rm = TRUE) > 100){
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = 1)
    } else {
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = .01)
    }
  } else if (stringr::str_detect(y_title, "Percent")) {
    label_text <- df$var %>% signif(sigfig) %>% scales::percent(accuracy = accuracy, scale = 1, suffix = "%")
  } else {
    label_text <- df$var %>% signif(sigfig) %>% scales::comma(accuracy = accuracy)
  }

  # Set text format, highlight and italicise Louisville text, highlight Louisville bar
  df$textcolor <- "#000000"
  df$textcolor[df$city == "Louisville"] <- "#000000"
  
  df$textfont <- "Montserrat"
  df$textfont[df$city == "Louisville"] <- "Montserrat Bold"
  
  label_color_names <- c("white", "black")
  label_color_values <- c("#000000", "#ffffff")
  
  df$label_color <- "white"
  df$label_color[df$city == "Louisville"] <- "black"

  #df$linecolor <- "#ffffff"
  #df$linecolor[df$city == "Louisville"] <- "#00a9b7"

  df$lou <- if_else(df$city == "Louisville", 1, 0)

  df$text_alignment <- 1.1
  if (!is.null(alternate_text)) df$text_alignment[df$rank %in% alternate_text] <- -0.1

  ### PLOT GRAPH
  
  # Initial plot
  p <- ggplot(data = df,
              aes(x = factor(names, levels = rev(names)),
                  y = var,
                  fill  = factor(color, levels = color_names, ordered = TRUE)))

  p <- p + guides(fill = FALSE, color = FALSE)

  # Add bars
  p <- p +
    geom_bar(stat = "identity",
             size = text_size) +
    coord_flip() +
    ggthemes::theme_tufte()

  p <- p + scale_fill_manual(values = color_values)
  #p <- p + scale_color_manual(values = c("#ffffff", "#00a9b7"))

  # Add features
  title_scale <- min(1, 48 / nchar(plot_title))

  p <- p + theme(text = element_text(family = "Montserrat"),
                 plot.title = element_text(size = 74 * title_scale * text_size, hjust = 0.5, margin = margin(b = 10, unit = "pt")),
                 axis.text.y = element_text(hjust = 0,
                                            size = 40 * text_size, 
                                            color = rev(df$textcolor),
                                            family = rev(df$textfont)),
                 axis.title.x = element_text(size = 60 * text_size),
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(size = 36 * text_size, lineheight = 0.5))

  if(subtitle_text != ""){
    p <- p + theme(plot.subtitle = element_text(hjust = 0.5, size = 60 * text_size)) +
      labs(subtitle = subtitle_text)
  }

  # Add numeric labels to bars based on bar_label parameter
  if (y_title != "" & bar_label) {
    p <- p + geom_text(aes(label = label_text, 
                           hjust = text_alignment, 
                           color = factor(label_color),
                           family = textfont),
                       size = 14 * text_size) +
       scale_colour_manual(values=c("#000000", "#ffffff"))
    }

  # Add vertical line to the left side of the bars based on the h_line parameter
  if (min(df$var, na.rm = TRUE) < 0) p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 2)

  # Add remaining text
  p <- p + labs(title = plot_title, y = y_title,
                x = "", caption = caption_text)
  
  p
}

int_2018 <- int_tbl %>%
  filter(YEAR == 2018) %>%
  mutate(current = 1,
         int_acc = int_acc * 100)

hspd_2018 <- hspd_tbl %>%
  filter(YEAR == 2018) %>%
  mutate(current = 1,
         hspd_int = hspd_int * 100)

comp_tab_2018 <- comp_tab_tbl %>%
  filter(YEAR == 2018) %>%
  mutate(current = 1,
         comp_tab = comp_tab * 100)

hspd_dev_2018 <- hspd_dev_tbl %>%
  filter(YEAR == 2018) %>%
  mutate(current = 1,
         hspd_dev = hspd_dev * 100)
  

plt1 <- ranking(int_2018, int_acc,
                plot_title = "Internet Access",
                subtitle_text = "2018", text_size = .60) 

plt2 <- ranking(hspd_2018, hspd_int,
                plot_title = "High Speed Internet Access",
                subtitle_text = "2018", text_size = .60)

plt3 <- ranking(comp_tab_2018, comp_tab,
                plot_title = "Computer or Tablet in Household",
                subtitle_text = "2018", text_size = .60)

plt4 <- ranking(hspd_dev_2018, hspd_dev,
                plot_title = "High Speed Internet and Device",
                subtitle_text = "2018", text_size = .60)
```

### High Speed Internet

Our focus on internet access is a focus on high speed access. While internet access over a smartphone or other non high speed source can be useful for staying connected, in the COVID era of working and learning from home high speed internet is a necessary utility that over a quarter of our households do not have. While 91% of houses have some form of internet access, only 73% have high speed internet access. 


```r
plt2
```

<img src="internet_report_files/figure-html/unnamed-chunk-3-1.png" width="672" />

### Digital Devices

A full 15% of our households do not have either a computer or tablet at home, and this understates the extent of the internet device shortage, as a single device is insufficient for a household with multiple people working and learning. 


```r
plt3
```

<img src="internet_report_files/figure-html/unnamed-chunk-4-1.png" width="672" />

### Internet and Devices

Finally, we can also look at the overlap of having both high speed internet and a device to work on. At 71% this is fairly similar to the numbers with high speed internet, suggesting that most people with high speed internet do have at least one device capable of accessing it. 


```r
plt4
```

<img src="internet_report_files/figure-html/unnamed-chunk-5-1.png" width="672" />

## Across Louisville

We can also take a deeper look into internet access within Louisville. We'll focus on high speed internet. While devices are also important, the data doesn't let us know how many devices are in each household, so it's guidance on the magnitude of the device shortage is quite limited. 

### Internet Speed

While our focus is on high speed access, we do show the history of both all internet access and high speed access in the graph below. Although overall internet access has increased a little since 2013, high speed internet access remains flat or possibly slightly below 2013 levels. 
In 2016, the Census Bureau began asking about smart phones for the first time. Although the internet access question was not changed, the addition of the question about smartphones may still explain the jump in consumers reporting that they had internet acess at home. There is no similar uptick in high speed access.  

```r
#Cut the data down to just Jefferson County
# df_msa <- df %>%
#   filter(MET2013 == 31140)
# 
# df_jeff <- df_msa %>%
#   filter(PUMA %in% c("1701", "1702", "1703", "1704", "1705", "1706")) %>%
#   write_csv("jeff_co_int.csv")


df <- read_csv("ipums_internet_access/jeff_co_int.csv")


# See if there's data on high speed internet access at the tract level 

df <- df %>%
  mutate(poverty = if_else(POVERTY < 100 & POVERTY != 000, 1, 0),
         under65 = if_else(AGE < 65, 1, 0))

df_na <- df %>%
  filter(!is.na(hspd_int))

svy_df <- svydesign(ids = ~ 1, weights = ~PERWT, data = df_na)

hint_tbl <- svyby(~hspd_int, ~YEAR, design = svy_df, svymean)
hint_tbl_count <- svyby(~hspd_int, ~YEAR, design = svy_df, svytotal)
int_tbl <- svyby(~int_acc, ~YEAR, design = svy_df, svymean)
int_pov_tbl <- svyby(~hspd_int, ~YEAR+poverty, design = svy_df, svymean)
int_age_tbl <-svyby(~hspd_int, ~YEAR+under65, design = svy_df, svymean)
int_race_tbl <- svyby(~hspd_int, ~YEAR+RACE, design = svy_df, svymean)


hint_tbl <- hint_tbl %>%
  mutate(int = hspd_int * 100,
         se100 = se * 100,
         `High Speed` = "High Speed")

int_tbl <- int_tbl %>%
  mutate(int = int_acc * 100,
         se100 = se * 100,
         `High Speed` = "All Internet") %>%
  bind_rows(hint_tbl)

plt_by <- function(df, group_var, title_text = "Internet Access") {
  group_var <- enquo(group_var)

  text_scale <- 1
  
  plt <- ggplot(data = df, aes(x = YEAR, y = int, group = !!group_var, colour = !!group_var)) +
    geom_errorbar(aes(ymin = int - (1.96 * se100), ymax = int + 1.96 * se100), width = .1) +
    geom_point(size = 2) +
    geom_line(size = .65) +
    theme_bw() +
    labs(title = title_text, x = "Year", y = "Percent") +
    theme(legend.position = "bottom")
  
  plt <- plt  + 
    scale_colour_manual(values = c("#323844", "#d63631")) +
    theme(text = element_text(family = "Montserrat"),
          
          legend.title     = element_text(size = 30 * text_scale),
          legend.text      = element_text(size = 24 * text_scale,
                                          margin = margin(b = 0.2 * text_scale, t = 0.2 * text_scale, unit = "cm")),

          axis.text    = element_text(size = 24 * text_scale),
          axis.title   = element_text(size = 30 * text_scale),
          axis.title.x = element_text(margin = margin(t = 0.3 * text_scale, unit = "cm")),
          axis.title.y = element_text(margin = margin(r = 0.3 * text_scale, unit = "cm")),
      
          plot.title = element_text(size = 42 * text_scale,
                                    hjust = .5,
                                    margin = margin(b = 0.4 * text_scale, unit = "cm")))

  plt <- plt +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
      legend.key = element_rect(fill = "transparent",colour = NA)
  )
  
  plt
}

plt_int <- plt_by(int_tbl, `High Speed`)

plt_int
```

<img src="internet_report_files/figure-html/jeff co-1.png" width="672" />

```r
# plt_int <- ggplot(int_tbl, aes(x = YEAR, y = int)) +
#   geom_errorbar(aes(ymin = int - (1.96 * se100), ymax = int + 1.96 * se100), width = .1) +
#   geom_line() +
#   geom_point() +
#   theme_bw() +
#   labs(title = "Household Internet Access", x = "Year", y = "Percent") +
#   theme(legend.position = "bottom")
# 
# plt_int
```

### Poverty

HIgh speed internet access for households in poverty has been _decreasing_ since 2013. 


```r
int_pov_tbl <- int_pov_tbl %>%
  mutate(Poverty = if_else(poverty == 0, "Above Poverty Line", "Below Poverty Line"),
         int = hspd_int * 100,
         se100 = se * 100)

plt_pov <- plt_by(int_pov_tbl, Poverty)

plt_pov
```

<img src="internet_report_files/figure-html/poverty_int-1.png" width="672" />

### Race

High speed internet access has either remained steady or slightly declined for Black households in Louisville. 


```r
int_race_tbl <- int_race_tbl %>%
  filter(RACE < 3) %>% #Louisville's racial groups other than White and Black have small populations leading to low sample sizes
  mutate(Race = if_else(RACE == 1, "White", "Black"),
         int = hspd_int * 100,
         se100 = se * 100)

plt_race <- plt_by(int_race_tbl, Race, title_text = "High Speed Internet Access")

plt_race
```

<img src="internet_report_files/figure-html/race-1.png" width="672" />

### Age

Households with adults age 65 and older is one of the only household groupings where high speed internet access has actually increased over time


```r
int_age_tbl <- int_age_tbl %>%
  mutate(int = hspd_int * 100,
         se100 = se * 100,
         Age = if_else(under65 == 1, "Under 65", "65 and Older"))

plt_age <- plt_by(int_age_tbl, Age, title_text = "High Speed Internet Access")

plt_age
```

<img src="internet_report_files/figure-html/unnamed-chunk-6-1.png" width="672" />

## Child Internet Access


### School Age Children

In this section, we limit the scope of our analysis to look at children ages 5-18. Nontraditional instruction (NTI) functions much better with access to high speed internet. About 25,600 school age children in Jefferson County do not have access to high speed internet. The map below is broken into 6 areas based on public use microdata geographies. Unfortunately, this is the lowest geographic level of detail we can get with Census microdata for school-age childen.


```r
##Section on Children

#Recode values so that 1 indicates not having interent access
#This makes it easier to get a raw count on number of kids without high speed access
df_ch <- df_na %>%
  filter(AGE > 4 & AGE < 19 & YEAR == 2018) %>%
  mutate(hint = case_when(
    hspd_int == 1 ~ 0,
    hspd_int == 0 ~ 1,
    TRUE ~ NA_real_
  ))

svy_df <- svydesign(ids = ~ 1, weights = ~PERWT, data = df_ch)

##Making a map
int_tbl <- svyby(~hint, ~YEAR + PUMA, design = svy_df, svymean)

int_tbl_count <- svyby(~hint, ~YEAR + PUMA, design = svy_df, svytotal)

ky_shp <- readOGR("ipums_internet_access/cb_2016_21_puma10_500k", layer = "cb_2016_21_puma10_500k", GDAL1_integer64_policy = TRUE, verbose = FALSE)
ky_shp@data$PUMA <- as.numeric(as.character(ky_shp@data$PUMACE10))
ky_sf <- st_as_sf(ky_shp)

jeff_co_sf <- ky_sf %>% 
  filter(ky_sf$PUMA %in% c("1701", "1702", "1703", "1704", "1705", "1706"))

int_puma <- tibble(
  PUMA = int_tbl$PUMA,
  int_per = int_tbl$hint * 100,
  int_num = formattable::comma(round(int_tbl_count$hint, -2), digits = 0)
)

jeff_co_sf <- full_join(jeff_co_sf, int_puma, by = "PUMA")

text_scale <- 1

ggplot(jeff_co_sf) + 
  geom_sf(aes(fill=int_per), color = "#ffffff") +
  geom_sf_label(aes(label = int_num), family = "Montserrat", fontface = "bold", size = 8) +
  #scale_fill_gradient(low = "#0e4a99", high = "#f58021", name = "Percent") +
  scale_fill_gradient(low = "#323844", high = "#d63631", name = "Percent") +
  theme_bw() +
  theme(text = element_text(family = "Montserrat"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        
        plot.caption     = element_text(size = 18 * text_scale, lineheight = .5),
        legend.title     = element_text(size = 24 * text_scale),
        legend.text      = element_text(size = 20 * text_scale,
                                        margin = margin(b = 0.2 * text_scale, t = 0.2 * text_scale, unit = "cm")),
        plot.title = element_text(size = 32 * text_scale, lineheight = .5,
                                  margin = margin(b = 0.4 * text_scale, unit = "cm"))) +
  labs(title = "Children ages 5-18 in households without
                High Speed Internet Access",
       caption = "Map is shaded by the percent of children without access in each 
                  Public Use Microdata Area. The number of children without access 
                  is given by the label rounded to the nearest 100. \n
                  Greater Louisville Project Analysis of Census Microdata from IPUMs") +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA),
    legend.box.background = element_rect(fill = "transparent",colour = NA),
    legend.key = element_rect(fill = "transparent",colour = NA))
```

<img src="internet_report_files/figure-html/unnamed-chunk-7-1.png" width="672" />


### Metro Council Districts

If we expand our focus to all children ages 0-17, we can get data at finer levels of geography. The map below shows the percent of children ages 0-17 who lack either high-speed internet or a computer at home. Most of these children are school age, though the data also includes some infants and toddlers who are not in school.

<!-- ::: {.tab} -->
<!-- <button class="tablinks" onclick="unrolltab(event, 'no-district-labels')">No District Labels</button> -->
<!-- <button class="tablinks" onclick="unrolltab(event, 'district-labels')">District Labels</button> -->

::::: {.panelset}

#### No District labels {.panel .unnumbered}


```r
map_district <- st_read("ipums_internet_access/Council_Districts", quiet =T)
child_internet <- read_csv("ipums_internet_access/child_internet.csv", col_types = "nnnn")

load("ipums_internet_access/district_label_points.RData")

map_district %<>% select(district = coundist) %>%
  left_join(child_internet, by = "district") %>%
  mutate(no_int = 100 - percent_internet)

# Replicate binary decision tree to determine most-interior point of polygons
# buffers <- c()
# 
# for(d in 1:26) {
#   buff_max = -0.06
#   buff_min = 0
#   this_buffer = buff_min
#   this_step = buff_max
#   current_resolution = 11
#   
#   while(current_resolution > 10){
#   
#     #browser()
#     # Buffer inside the polygon using this_buffer
#     temp_sf <- st_buffer(map_district[d,], dist = this_buffer, singleSide = T) 
#   
#     # Calculate area of polygon
#     temp_area = st_area(temp_sf) %>% as.numeric()
#     
#     # If remaining area > 0, enlarge buffer by going away from 0. 
#     #   Also record most recent correct buffer and area produced by buffer.
#     # If remaining area is 0, reduce buffer by going toward 0.
#     if (temp_area > 0) {
#       current_result = this_buffer
#       current_resolution = temp_area
#       
#       this_buffer = this_buffer + this_step
#     } else {
#       this_buffer = this_buffer - this_step
#     }
#     
#     # Cut search step in half
#     this_step = this_step / 2
#   }
#   
#   buffers <- c(buffers, current_result)
#   
# }

text_scale <- 1

ggplot(map_district) + 
  geom_sf(aes(fill=no_int), color = "#ffffff") +
  #geom_sf_text(aes(label = district), family = "Montserrat Bold", fontface = "bold", size = 6, color = "#ffffff",
               #fun.geometry = function(x) st_buffer(x, dist = buffers, singleSide = T) %>% st_point_on_surface()) +
  #scale_fill_gradient(low = "#0e4a99", high = "#f58021", name = "Percent") +
  scale_fill_gradient(low = "#323844", high = "#d63631", name = "Percent") +
  theme_bw() +
  theme(text = element_text(family = "Montserrat"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        
        plot.caption     = element_text(size = 18 * text_scale, lineheight = .5),
        legend.title     = element_text(size = 24 * text_scale),
        legend.text      = element_text(size = 20 * text_scale,
                                        margin = margin(b = 0.2 * text_scale, t = 0.2 * text_scale, unit = "cm")),
        plot.title = element_text(size = 32 * text_scale, lineheight = .5, hjust = 0.5,
                                  margin = margin(b = 0.4 * text_scale, unit = "cm"))) +
  labs(title = "Children ages 0-17 in households without
                High Speed Internet Access and a computer",
       caption = "Map is shaded by the percent of children without access in each 
                  Metro Council District. The number of children without internet 
                  or without a computer is given by the label rounded to the nearest 100. \n
                  Greater Louisville Project Analysis of Census ACS data.") +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA),
    legend.box.background = element_rect(fill = "transparent",colour = NA),
    legend.key = element_rect(fill = "transparent",colour = NA))
```

<img src="internet_report_files/figure-html/unnamed-chunk-8-1.png" width="672" />

```r
output_table <- map_district %>% 
  transmute(
    District = district, 
         "Percent without access" = scales::percent(100 - percent_internet, scale = 1, accuracy = 0.1),
         "Children with access" = scales::comma(child_internet, accuracy = 1),
         "Children without access" = scales::comma(total_child - child_internet, accuracy = 1)) %>%
  sf::st_drop_geometry()

kbl(output_table, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  scroll_box(height = "300px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; "><table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> District </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Percent without access </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Children with access </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Children without access </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 18.2% </td>
   <td style="text-align:center;"> 5,423 </td>
   <td style="text-align:center;"> 1,205 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> 11.8% </td>
   <td style="text-align:center;"> 6,438 </td>
   <td style="text-align:center;"> 865 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> 22.4% </td>
   <td style="text-align:center;"> 4,669 </td>
   <td style="text-align:center;"> 1,351 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> 32.5% </td>
   <td style="text-align:center;"> 3,755 </td>
   <td style="text-align:center;"> 1,805 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 5 </td>
   <td style="text-align:center;"> 33.6% </td>
   <td style="text-align:center;"> 5,036 </td>
   <td style="text-align:center;"> 2,554 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> 33.2% </td>
   <td style="text-align:center;"> 4,259 </td>
   <td style="text-align:center;"> 2,111 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 7 </td>
   <td style="text-align:center;"> 3.3% </td>
   <td style="text-align:center;"> 5,608 </td>
   <td style="text-align:center;"> 187 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 8 </td>
   <td style="text-align:center;"> 1.9% </td>
   <td style="text-align:center;"> 5,067 </td>
   <td style="text-align:center;"> 100 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 9 </td>
   <td style="text-align:center;"> 4.0% </td>
   <td style="text-align:center;"> 4,867 </td>
   <td style="text-align:center;"> 205 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> 7.6% </td>
   <td style="text-align:center;"> 5,114 </td>
   <td style="text-align:center;"> 421 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> 5.6% </td>
   <td style="text-align:center;"> 5,819 </td>
   <td style="text-align:center;"> 342 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 12 </td>
   <td style="text-align:center;"> 7.8% </td>
   <td style="text-align:center;"> 6,200 </td>
   <td style="text-align:center;"> 523 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 13 </td>
   <td style="text-align:center;"> 12.0% </td>
   <td style="text-align:center;"> 6,399 </td>
   <td style="text-align:center;"> 874 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 14 </td>
   <td style="text-align:center;"> 9.0% </td>
   <td style="text-align:center;"> 6,229 </td>
   <td style="text-align:center;"> 618 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 15 </td>
   <td style="text-align:center;"> 27.1% </td>
   <td style="text-align:center;"> 4,620 </td>
   <td style="text-align:center;"> 1,714 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 16 </td>
   <td style="text-align:center;"> 2.3% </td>
   <td style="text-align:center;"> 7,218 </td>
   <td style="text-align:center;"> 171 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 17 </td>
   <td style="text-align:center;"> 3.2% </td>
   <td style="text-align:center;"> 7,401 </td>
   <td style="text-align:center;"> 247 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 18 </td>
   <td style="text-align:center;"> 2.7% </td>
   <td style="text-align:center;"> 5,804 </td>
   <td style="text-align:center;"> 164 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 19 </td>
   <td style="text-align:center;"> 2.5% </td>
   <td style="text-align:center;"> 7,788 </td>
   <td style="text-align:center;"> 199 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 20 </td>
   <td style="text-align:center;"> 4.2% </td>
   <td style="text-align:center;"> 8,095 </td>
   <td style="text-align:center;"> 353 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 21 </td>
   <td style="text-align:center;"> 19.9% </td>
   <td style="text-align:center;"> 4,681 </td>
   <td style="text-align:center;"> 1,161 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 22 </td>
   <td style="text-align:center;"> 4.1% </td>
   <td style="text-align:center;"> 7,379 </td>
   <td style="text-align:center;"> 315 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 23 </td>
   <td style="text-align:center;"> 9.9% </td>
   <td style="text-align:center;"> 5,911 </td>
   <td style="text-align:center;"> 650 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 24 </td>
   <td style="text-align:center;"> 8.4% </td>
   <td style="text-align:center;"> 6,850 </td>
   <td style="text-align:center;"> 632 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 25 </td>
   <td style="text-align:center;"> 3.8% </td>
   <td style="text-align:center;"> 6,040 </td>
   <td style="text-align:center;"> 241 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 26 </td>
   <td style="text-align:center;"> 10.3% </td>
   <td style="text-align:center;"> 5,291 </td>
   <td style="text-align:center;"> 590 </td>
  </tr>
</tbody>
</table></div>

#### District Labels {.panel .unnumbered}


```r
ggplot(map_district) + 
  geom_sf(aes(fill=no_int), color = "#ffffff") +
  geom_sf_text(data = district_label_points, 
               aes(label = district), family = "Montserrat Bold", fontface = "bold", size = 6, color = "#ffffff") +
  #scale_fill_gradient(low = "#0e4a99", high = "#f58021", name = "Percent") +
  scale_fill_gradient(low = "#323844", high = "#d63631", name = "Percent") +
  theme_bw() +
  theme(text = element_text(family = "Montserrat"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        
        plot.caption     = element_text(size = 18 * text_scale, lineheight = .5),
        legend.title     = element_text(size = 24 * text_scale),
        legend.text      = element_text(size = 20 * text_scale,
                                        margin = margin(b = 0.2 * text_scale, t = 0.2 * text_scale, unit = "cm")),
        plot.title = element_text(size = 32 * text_scale, lineheight = .5, hjust = 0.5,
                                  margin = margin(b = 0.4 * text_scale, unit = "cm"))) +
  labs(title = "Children ages 0-17 in households without
                High Speed Internet Access and a computer",
       caption = "Map is shaded by the percent of children without access in each 
                  Metro Council District. The number of children without internet 
                  or without a computer is given by the label rounded to the nearest 100. \n
                  Greater Louisville Project Analysis of Census ACS data.") +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA),
    legend.box.background = element_rect(fill = "transparent",colour = NA),
    legend.key = element_rect(fill = "transparent",colour = NA))
```

<img src="internet_report_files/figure-html/unnamed-chunk-9-1.png" width="672" />

```r
kbl(output_table, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  scroll_box(height = "300px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; "><table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> District </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Percent without access </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Children with access </th>
   <th style="text-align:center;position: sticky; top:0; background-color: #FFFFFF;"> Children without access </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 18.2% </td>
   <td style="text-align:center;"> 5,423 </td>
   <td style="text-align:center;"> 1,205 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> 11.8% </td>
   <td style="text-align:center;"> 6,438 </td>
   <td style="text-align:center;"> 865 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> 22.4% </td>
   <td style="text-align:center;"> 4,669 </td>
   <td style="text-align:center;"> 1,351 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> 32.5% </td>
   <td style="text-align:center;"> 3,755 </td>
   <td style="text-align:center;"> 1,805 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 5 </td>
   <td style="text-align:center;"> 33.6% </td>
   <td style="text-align:center;"> 5,036 </td>
   <td style="text-align:center;"> 2,554 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> 33.2% </td>
   <td style="text-align:center;"> 4,259 </td>
   <td style="text-align:center;"> 2,111 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 7 </td>
   <td style="text-align:center;"> 3.3% </td>
   <td style="text-align:center;"> 5,608 </td>
   <td style="text-align:center;"> 187 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 8 </td>
   <td style="text-align:center;"> 1.9% </td>
   <td style="text-align:center;"> 5,067 </td>
   <td style="text-align:center;"> 100 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 9 </td>
   <td style="text-align:center;"> 4.0% </td>
   <td style="text-align:center;"> 4,867 </td>
   <td style="text-align:center;"> 205 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 10 </td>
   <td style="text-align:center;"> 7.6% </td>
   <td style="text-align:center;"> 5,114 </td>
   <td style="text-align:center;"> 421 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> 5.6% </td>
   <td style="text-align:center;"> 5,819 </td>
   <td style="text-align:center;"> 342 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 12 </td>
   <td style="text-align:center;"> 7.8% </td>
   <td style="text-align:center;"> 6,200 </td>
   <td style="text-align:center;"> 523 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 13 </td>
   <td style="text-align:center;"> 12.0% </td>
   <td style="text-align:center;"> 6,399 </td>
   <td style="text-align:center;"> 874 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 14 </td>
   <td style="text-align:center;"> 9.0% </td>
   <td style="text-align:center;"> 6,229 </td>
   <td style="text-align:center;"> 618 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 15 </td>
   <td style="text-align:center;"> 27.1% </td>
   <td style="text-align:center;"> 4,620 </td>
   <td style="text-align:center;"> 1,714 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 16 </td>
   <td style="text-align:center;"> 2.3% </td>
   <td style="text-align:center;"> 7,218 </td>
   <td style="text-align:center;"> 171 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 17 </td>
   <td style="text-align:center;"> 3.2% </td>
   <td style="text-align:center;"> 7,401 </td>
   <td style="text-align:center;"> 247 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 18 </td>
   <td style="text-align:center;"> 2.7% </td>
   <td style="text-align:center;"> 5,804 </td>
   <td style="text-align:center;"> 164 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 19 </td>
   <td style="text-align:center;"> 2.5% </td>
   <td style="text-align:center;"> 7,788 </td>
   <td style="text-align:center;"> 199 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 20 </td>
   <td style="text-align:center;"> 4.2% </td>
   <td style="text-align:center;"> 8,095 </td>
   <td style="text-align:center;"> 353 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 21 </td>
   <td style="text-align:center;"> 19.9% </td>
   <td style="text-align:center;"> 4,681 </td>
   <td style="text-align:center;"> 1,161 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 22 </td>
   <td style="text-align:center;"> 4.1% </td>
   <td style="text-align:center;"> 7,379 </td>
   <td style="text-align:center;"> 315 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 23 </td>
   <td style="text-align:center;"> 9.9% </td>
   <td style="text-align:center;"> 5,911 </td>
   <td style="text-align:center;"> 650 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 24 </td>
   <td style="text-align:center;"> 8.4% </td>
   <td style="text-align:center;"> 6,850 </td>
   <td style="text-align:center;"> 632 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 25 </td>
   <td style="text-align:center;"> 3.8% </td>
   <td style="text-align:center;"> 6,040 </td>
   <td style="text-align:center;"> 241 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 26 </td>
   <td style="text-align:center;"> 10.3% </td>
   <td style="text-align:center;"> 5,291 </td>
   <td style="text-align:center;"> 590 </td>
  </tr>
</tbody>
</table></div>

```r
# map_district %<>%
#   mutate(hover = paste("District", district))

# library(plotly)
# 
# plotly::plot_ly(stroke = I("white")) %>%
#   add_sf(data = map_district, 
#          color = ~percent_internet, 
#          split = ~district,
#          colors = c("#d63631", "#323844"),
#          alpha = 1)
```

:::::

## Internet as a Public Utility

The lack of high speed internet access in Louisville is part of a broader trend across the United States of continuing to treat home internet as something that is 'nice to have' instead of something that is necessary to fully take part in modern society. In general, the U.S. leaves broadband infrastructure to private companies who will not build out infrastructure to areas that aren't profitable. An exception to this trend in Chatanooga, TN, where [the electric utility is also an Internet Service Provider](https://tech.co/news/chattanooga-fastest-internet-usa-2018-08). The lack of internet acess in Louisville (and throughout the U.S) has [severe consequences](https://www.theatlantic.com/technology/archive/2020/08/virtual-learning-when-you-dont-have-internet/615322/) including exacerbating inequality (both racial and economic) and slower overall economic growth. 

### Local Efforts

- Jefferson County Public Schools have distributed hotspots and chromebooks to try to complete NTI. 
- Kentucky is investing $8 million to [subsidize internet access for low-income students](https://wfpl.org/kentucky-to-invest-in-broadband-access-for-low-income-students/)
 

```r
# png("rankings_graph.png", 1500, 1200, res = 200)
# plt1
# dev.off
# 
# png("int_access_graph.png", 1600, 1200, res = 200)
# plt_int
# dev.off
# 
# png("int_poverty.png", 1600, 1200, res = 200)
# plt_pov
# dev.off
# 
# png("int_race.png", 1600, 1200, res = 200)
# plt_race
# dev.off
# 
# png("int_age.png", 1600, 1200, res = 200)
# plt_age
# dev.off
```

For more local data and analysis please visit the [Greater Louisville Project](https://greaterlouisvilleproject.org/)
