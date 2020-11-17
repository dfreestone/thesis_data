suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyverse))
library(quanteda)

data <- read_csv("https://raw.githubusercontent.com/freestone-lab/thesis_data/main/savva_data.csv")

data %<>%
  slice(3:nrow(data))

data %<>%
  select(ResponseId, Finished, Progress, `Duration (in seconds)`,
         `1`, `2`, `3`, `5`, `6`, `7`, `8`, `9`, `10`, `13`, `15`,
         `17`, `19`, `21`, `23`, Q36, Q37, Q38, Q39, Q42_1, Q42_2) %>%
  rename(gender = `1`, age = `2`, major = `3`, sentence_dogballrun = `5`,
         sentence_studentschooltest = `6`, sentence_clothesboydirtybaseball = `7`,
         sentence_carsslowfriendstunnelparty = `8`, sentence_beachfamilyschoolsummericecream = `9`,
         sentence_promschoolgirlruinedshopping = `10`, wmnumbers_542 = `13`, wmnumbers_836 = `15`,
         wmnumbers_698 = `17`, wmnumbers_102612 = `19`, wmnumbers_133520 = `21`, wmnumbers_935742 = `23`,
         picture_yellowshirt = Q36, wmwords_toweldog = Q37, picture_bluesweater = Q38, wmwords_batpurse = Q39,
         howoften_read = Q42_1, howoften_write = Q42_2)

print(glue::glue("Number of participants total: {nrow(data)}"))
data %<>%
  filter(Finished == "TRUE")
print(glue::glue("Number of participants after exclusion: {nrow(data)}"))

data %>%
  select(ResponseId, sentence_dogballrun, sentence_studentschooltest, sentence_clothesboydirtybaseball,
         sentence_carsslowfriendstunnelparty, sentence_beachfamilyschoolsummericecream, sentence_promschoolgirlruinedshopping,
         picture_yellowshirt, picture_bluesweater) %>%
  write_csv("sentence_complexity.csv")

wmWORD_score <- data %>%
  mutate(wmwords_toweldog = str_to_lower(wmwords_toweldog),
         wmwords_batpurse = str_to_lower(wmwords_batpurse),
         wmwords_toweldog_correct = str_detect(wmwords_toweldog, "towel") + str_detect(wmwords_toweldog, "dog"),
         wmwords_batpurse_correct = str_detect(wmwords_batpurse, "bat") + str_detect(wmwords_batpurse, "purse")) %>%
  rowwise() %>%
  mutate(m_wmWORD_score = mean(c_across(ends_with("correct")))) %>%
  select(ResponseId, m_wmWORD_score)


wmNUM_score <- data %>%
  separate(wmnumbers_542, into = c("wm542_1", "wm542_2", "wm542_3")) %>%
  separate(wmnumbers_836, into = c("wm836_1", "wm836_2", "wm836_3")) %>%
  separate(wmnumbers_698, into = c("wm698_1", "wm698_2", "wm698_3")) %>%
  separate(wmnumbers_102612, into = c("wm102612_1", "wm102612_2", "wm102612_3")) %>%
  separate(wmnumbers_133520, into = c("wm133520_1", "wm133520_2", "wm133520_3")) %>%
  separate(wmnumbers_935742, into = c("wm935742_1", "wm935742_2", "wm935742_3")) %>%
  mutate(wm542_score = (wm542_1==3) + (wm542_2==2) + (wm542_3==0),
         wm836_score = (wm836_1==6) + (wm836_2==1) + (wm836_3==4),
         wm698_score = (wm698_1==4) + (wm698_2==7) + (wm698_3==6),
         wm102612_score = (wm102612_1==8) + (wm102612_2==24) + (wm102612_3==10),
         wm133520_score = (wm133520_1==11) + (wm133520_2==33) + (wm133520_3==18),
         wm935742_score = (wm935742_1==91) + (wm935742_2==55) + (wm935742_3==40)) %>%
  rowwise() %>%
  mutate(m_wmNUMBER_score = mean(c_across(starts_with("wm") & ends_with("score")))) %>%
  select(ResponseId, m_wmNUMBER_score)

how_often <- data %>%
  mutate(howoften_read = case_when(howoften_read == "Never" ~ 0,
                                   startsWith(howoften_read, "1-2") ~ 1,
                                   startsWith(howoften_read, "3-4") ~ 2,
                                   howoften_read == "Everyday" ~ 3),
         howoften_write = case_when(howoften_write == "Never" ~ 0,
                                    startsWith(howoften_write, "1-2") ~ 1,
                                    startsWith(howoften_write, "3-4") ~ 2,
                                    howoften_write == "Everyday" ~ 3)) %>%
  select(ResponseId, howoften_read, howoften_write)


# ------------------------------------------------------------------------------
# WORD COMMPLEXITY
# Creating a corpusA
measures <- c("meanSentenceLength","meanWordSyllables",
              "Flesch.Kincaid", "Flesch")

sentences <- data %>%
  select(ResponseId, starts_with("sentence"), starts_with("picture")) %>%
  mutate(across(-ResponseId, ~corpus(.x, text_field = "text")),
         across(-ResponseId, ~textstat_readability(.x, measures,
                                                   remove_hyphens = TRUE,
                                                   min_sentence_length = 1, max_sentence_length = 10000,
                                                   intermediate = FALSE)))

nwords <- sentences %>%
  mutate(across(-ResponseId, ~.x$meanSentenceLength)) %>%
  rowwise() %>%
  mutate(sentence_nwords = mean(c_across(starts_with("sentence"))),
         picture_nwords = mean(c_across(starts_with("picture")))) %>%
  select(ResponseId, sentence_nwords, picture_nwords)


syllables <- sentences %>%
  mutate(across(-ResponseId, ~.x$meanWordSyllables)) %>%
  rowwise() %>%
  mutate(sentence_syllables = mean(c_across(starts_with("sentence"))),
         picture_syllables= mean(c_across(starts_with("picture")))) %>%
  select(ResponseId, sentence_syllables, picture_syllables)

Flesch.Kincaid <- sentences %>%
  mutate(across(-ResponseId, ~.x$Flesch.Kincaid)) %>%
  rowwise() %>%
  mutate(sentence_Flesch.Kincaid = mean(c_across(starts_with("sentence"))),
         picture_Flesch.Kincaid = mean(c_across(starts_with("picture")))) %>%
  select(ResponseId, sentence_Flesch.Kincaid, picture_Flesch.Kincaid)


d <- data %>%
  select(ResponseId, gender, age, major) %>%
  left_join(wmWORD_score, by = "ResponseId") %>%
  left_join(wmNUM_score, by = "ResponseId") %>%
  left_join(how_often, by = "ResponseId") %>%
  left_join(nwords, by = "ResponseId") %>%
  left_join(syllables, by = "ResponseId") %>%
  left_join(Flesch.Kincaid, by = "ResponseId") %>%
  write_csv("savva_data_cleaned.csv")


