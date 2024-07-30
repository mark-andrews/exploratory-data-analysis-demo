library(tidyverse)
library(psyntur)

# Process the NPI data ----------------------------------------------------

read_csv("raw-data/npi-data.csv") %>%
  select(score, gender, age) %>%
  mutate(gender = case_when(
    gender == 1 ~ "male",
    gender == 2 ~ "female",
    gender == 3 ~ "other",
    TRUE ~ NA
  )) %>% 
  # only less than 50 in over 11,000 cases are not male or female
  filter(gender %in% c('male', 'female')) %>%
  # in the codebook, it says that age below 14 were removed
  # but they were not so I will remove them now
  filter(age >= 15) %>% 
  # the number of people 60 and older is very small, so we will
  # remove these cases so that we have age-decade, i.e. 10-19, 20-29, etc.,
  # that have sufficient numbers of cases
  filter(age < 60) %>% 
  mutate(age_group = floor(age/10),
         age_group = case_when(
           age_group == 1 ~ '15-19',
           age_group == 2 ~ '20-29',
           age_group == 3 ~ '30-39',
           age_group == 4 ~ '40-49',
           age_group == 5 ~ '50-59'),
         age_group = factor(age_group, ordered = TRUE)
         ) %>% 
  select(narcissism = score, gender, age = age_group) ->
  npi_df

# Process the Big 5 data --------------------------------------------------

read_tsv('raw-data/big5-data.csv') %>% 
  # one row has all zeros for item values, remove it
  # in fact, remove any rows with zeros for item values,
  # even if it is just one row in fact
  filter(if_all(E1:O10, ~ . != 0))  %>% 
  # replace the numerical values for gender with proper labels
  # see codebook
  mutate(gender = case_when(
    gender == 1 ~ "male",
    gender == 2 ~ "female",
    gender == 3 ~ "other",
    TRUE ~ NA
  )) %>% 
  # only less than 150 in over 18,000 cases are not male or female
  # exclude cases that are neither male or female
  filter(gender %in% c('male', 'female')) %>%
  # in the codebook, it says that age below 13 were removed
  # there is a noticeable drop off in cases where age < 16
  # so I will use 16 and above
  filter(age >= 16) %>% 
  # the number of people 60 and older is very small, so we will
  # remove these cases so that we have age-decade, i.e. 10-19, 20-29, etc.,
  # that have sufficient numbers of cases
  # also we have "age" values of well over 100, and things like 1961, 1964 etc
  # and even 999999999 etc.
  # these are errors or someome deliberately entering in nonsense
  # so only value for age less than 60 will be kept
  filter(age < 60) %>% 
  mutate(age_group = floor(age/10),
         age_group = case_when(
           age_group == 1 ~ '15-19',
           age_group == 2 ~ '20-29',
           age_group == 3 ~ '30-39',
           age_group == 4 ~ '40-49',
           age_group == 5 ~ '50-59'),
         age_group = factor(age_group, ordered = TRUE)
  ) %>% 
  select(gender, age=age_group, matches('^O|C|E|A|N', ignore.case = FALSE)) ->
  big5_df

# Comparing big5-codebook and the keying on IPIP.org, e.g., 
# https://ipip.ori.org/newBigFive5broadKey.htm#Conscientiousness,
# which items are positively and negatively keyed appears as follows:
E_pos <- str_c('E', seq(1, 10, by = 2))
E_neg <- str_c('E', seq(2, 10, by = 2))

C_pos <- str_c('E', c(seq(1, 10, by = 2), 10))
C_neg <- str_c('E', seq(2, 9, by = 2))

# Note: "O" is not really "openness" I think
# I am too lazy to track down exactly where the 10 "O" items came from.
# It does not really matter for present purposes because this is just a 
# demo of exploratory data analysis. But it is probably important to note that
# the "O" items are not, for example, the items we see here: 
# https://ipip.ori.org/newNEOKey.htm#Openness-to-Experience
# even if there is relationship betweem them.
# Some of the "O" items are here: https://ipip.ori.org/newNEOKey.htm#Intellect
# which is classed as "intellect"

# I have had to decide these myself, but I think it is obvious in each case:
O_pos <- str_c("O", c(1, 3, 5, 7, 8, 9, 10))
O_neg <- str_c("O", c(2, 4, 6))

# I had to guess these ones too, but again, pretty obvious:
N_pos <- str_c('N', c(1, 3, 5, 6, 7, 8, 9, 10))
N_neg <- str_c('N', c(2, 4))

# Again guess; again obvious
A_pos <- str_c('A', c(2, 4, 6, 8, 9, 10))
A_neg <- str_c('A', c(1, 3, 5, 7))

# convenience util to reverse code values on a 1:5 scale
reverse_code <- function(x){
  re_code(x, from = 1:5, to = 5:1)
}

big5_df %>% 
  # I think this should do it: reverse code all the items that need to be reversed
  mutate(across(any_of(c(O_neg, C_neg, E_neg, A_neg, N_neg)), reverse_code)) %>%
  # total scores
  total_scores(openness = starts_with('O', ignore.case = FALSE),
               conscientiousness = starts_with('C', ignore.case = FALSE ),
               extraversion = starts_with('E', ignore.case = FALSE),
               agreeableness = starts_with('A', ignore.case = FALSE),
               neuroticism = starts_with('E', ignore.case = FALSE),
               .method = 'sum',
               .append = TRUE) %>% 
  select(gender, age, openness:neuroticism) ->
  big5_df_totals

# The second, already preprocessed, big 5 data ----------------------------

read_csv('raw-data/big5-b-data.csv') %>% 
  select(-id) %>% 
  select(age, gender, handedness = hand, country,
         # to keep variable names consistent with other big5 data set
         openness = open,
         conscientiousness = concs,
         extraversion = extra,
         agreeableness = agree,
         neuroticism = neuro) %>% 
  # as above, clip ages above
  filter(age >= 16, age < 60) %>% 
  mutate(age_group = floor(age/10),
         age_group = case_when(
           age_group == 1 ~ '15-19',
           age_group == 2 ~ '20-29',
           age_group == 3 ~ '30-39',
           age_group == 4 ~ '40-49',
           age_group == 5 ~ '50-59'),
         age_group = factor(age_group, ordered = TRUE)
  ) %>%
  select(gender, age = age_group, handedness, country, openness:neuroticism) ->
  big5b_df


# Save processed data as RDS  ---------------------------------------------

saveRDS(npi_df, file = 'data/npi.Rds')
saveRDS(big5_df_totals, file = 'data/big5a.Rds')
saveRDS(big5b_df, file = 'data/big5b.Rds')
