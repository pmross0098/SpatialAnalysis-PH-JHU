
library(utils)
library(stringr)
library(tidyverse)
library(kableExtra)
library(formattable)
library(magick)
library(webshot)
webshot::install_phantomjs()

# Identify AQ yearly data sets
AQ_dir <- 'AQ_Data'
AQ_zps <- dir(AQ_dir, pattern = "daily", full.names = FALSE)

lng_df = data.frame(matrix(nrow = 0, ncol = 34)) #empty dataframe
#cycle through them
for (yr_dat in AQ_zps) {
  AQ_dat <- read.csv(unzip(file.path(AQ_dir,yr_dat),
                           paste0(str_replace(yr_dat, '.zip',''), '.csv')))
  # Data between 6/1 and 10/31
  # Find #AQI>100 days per site
  AQ_WA <- AQ_dat %>%
    dplyr::filter(!is.na(AQI)) %>% #ignore data with missing AQI
    dplyr::filter(State.Name == 'Washington') %>% #just WA
    dplyr::mutate(Date = as.Date(Date.Local)) %>%
    dplyr::mutate(Yr = as.integer(str_sub(Date.Local,1,4))) %>% #grab year, month, day
    dplyr::mutate(Mo = as.integer(str_sub(Date.Local,6,7))) %>%
    dplyr::mutate(Day = as.integer(str_sub(Date.Local,9,10))) %>%
    dplyr::filter(Mo > 5 & Mo < 11) %>% # only interested in June-Oct
    dplyr::mutate(Site = str_to_title(Local.Site.Name)) #make the column headers less crappy

  lng_df <- rbind(lng_df, AQ_WA) # add filtered cleaned data to full dataframe
  file.remove(file.path(paste0(str_replace(yr_dat, '.zip',''), '.csv'))) # delete unzipped file
}

# Prep Table
# create timespan column for 5 year `eras`
lng_df$Span <- '2008-2012'
lng_df$Span[lng_df$Yr > 2012 & lng_df$Yr < 2018] <- '2013-2017'
lng_df$Span[lng_df$Yr > 2017 & lng_df$Yr < 2023] <- '2018-2022'

# conversion of monitor name to just city for 6 monitors of interest I'll highlight in a table
# I chose these because the data was near complete for the 3 "eras" I'm looking at
# I'm also ordering these according to how I want' them arranged in the final table
locs <- c('Seattle','Tacoma','Yakima','Spokane','Marysville','Omak')
names(locs) <- c('Seattle - Beacon Hill','Tacoma - L Street','Yakima - 4th Ave','Spokane - Augusta Ave','Marysville - 7th Ave (Marysville Junior High)','Omak-Colville Tribe')

# big dataframe to capture # smoke days (AQI > 100)
smkdy_df <- lng_df %>%
  filter(Yr > 2007) %>% #only 2008 and later
  group_by(Site, Span) %>% # two criteria
  summarize(n = n(), Smoke.Days = sum(AQI>100), MaxAQI = max(AQI)) %>% # by group, adding summary of
                                                                       # total days, smoke days, max AQI in 5 year period
  add_tally(name = "sets") %>%      # this is to point out which sets are complete when I was exploring data
  filter(Site %in% names(locs)) %>% # just the 6 monitors of interest, each had a tally = 3 eras
  mutate(Prob = round(Smoke.Days/n ,3),          # simple probability, Smoke Days / total days with observations
         Location = locs[Site],                  # Simplified name
         srt = which(Site == names(locs))) %>%   # This gives me an extra column to sort on based on my `locs`
  ungroup() %>%
  arrange(srt, Span) %>%                     # sort first by sort column, then by era
  select(Span, n, Smoke.Days, Prob, MaxAQI)  # Select columns of interest in order I want them displayed
                                             # Note I drop the Site, opting to manually add below as section headers
                                             # Aldo drop the `srt` sort variable

smkdy_df$Prob = formattable::percent(smkdy_df$Prob, digits = 1) # format probs as %

# rename column headers for display
colnames(smkdy_df) <- c('Time Span','Total Days','# Smoke Days (AQI>100)',
                        'Probability of Smoke Day','Maximum AQI Reading')

# Final table
# Again, I hate the *attempt* to control column. I had to mess repeatedly with "full_width" argument,
# width, and vwidth to try to find something that accidentally had decent spacing.
# Whatever. I'll go with flextable next time...
smk_ktab <- knitr::kable(smkdy_df, format = 'html', align = 'c', latex_options="scale_down") %>% #
  kable_classic(html_font = "Cambria", full_width = FALSE,
                lightable_options = "striped") %>% # lightly-striped rows, Cambria font
  add_header_above(header = c('Jun 1 - Oct 31' = 5),
                   font_size = 18, bold = T) %>% # Add subtitle first
  add_header_above(header = c('Increasing Number of Smoke Days In Washington' = 5),
                   font_size = 24, bold = T) %>% # The main title above the subtitle
  row_spec(row = 0, bold = T, color = "white", background = "#888", font_size = 15) %>% # format column headers
  # break up groups of 3 eras per city. I could do this programmatically, but this is still pretty clear
  pack_rows('Seattle', 1, 3, label_row_css = "background-color: #ccc; color: #444; border-bottom: 1px solid; font-size: 15px;") %>%
  pack_rows('Tacoma', 4, 6, label_row_css = "background-color: #ccc; color: #444; border-bottom: 1px solid; font-size: 15px;") %>%
  pack_rows('Yakima', 7, 9, label_row_css = "background-color: #ccc; color: #444; border-bottom: 1px solid; font-size: 15px;") %>%
  pack_rows('Spokane', 10, 12, label_row_css = "background-color: #ccc; color: #444; border-bottom: 1px solid; font-size: 15px;") %>%
  pack_rows('Marysville', 13, 15, label_row_css = "background-color: #ccc; color: #444; border-bottom: 1px solid; font-size: 15px;") %>%
  pack_rows('Omak', 16, 18, label_row_css = "background-color: #ccc; color: #444; border-bottom: 1px solid; font-size: 15px;") %>%
  # The column width arguments don't really work, or at
  column_spec(column = 1, bold = T, width = '4cm') %>% # Era column make bold
  # This does the `viridis` conditional highlighting of values in the probability column
  column_spec(column = 4, color = "darkgray35", bold = T, #width = '1.5cm',
            background = kableExtra::spec_color(smkdy_df$`Probability of Smoke Day`[1:18], alpha = 0.6,
                                    option = "B", begin = 0.2, end = 0.8, direction = -1)) %>%
  # column_spec(column = c(1:5), width_min = c('12em', '4em', '4em', '4em', '4em')) %>%
  # column_spec(column = 5, width_min = '1cm') %>%
  save_kable(file = "table_1.png", vwidth = 650) # Actually outputs as an image file the table
                                                 # This might be why the column spacing doesn't work well...

smk_ktab

