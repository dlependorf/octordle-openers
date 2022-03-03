########################################################################################################################
# Best Starting Words for Octordle?                                                                                    #
#                                                                                                                      #
# Author: Dan Lependorf                                                                                                #
# Date: 2022-02-28                                                                                                     #
#                                                                                                                      #
# I know I know, a million people have already "solved" the best starting word for Wordle. But for that multiplying    #
# series of Dordle/Quordle/Octordle/Secordle/whatever, it seems to me that as the number of puzzles grows, the more    #
# important it is to cover the field as broadly as possible, that is, start with a two or even a three word opener     #
# that covers the 10/15 most common letters, with no duplicates. What is the best opening combo?                       #
########################################################################################################################

library(tidyverse)
library(qdapDictionaries)

# The qdapDictionaries package comes with a couple of dictionaries, but I'm going to stick with the simplest one, since
# they don't allow weird unusual words as solutions, and while they do allow them as guesses, it feels more fun to use
# simpler ones and not super obtuse dictionary-only ones.
data("DICTIONARY")

########################################################################################################################
# First, what are the most used 15 letters in this dictionary?                                                         #
########################################################################################################################

five_letter_words <- DICTIONARY %>%
    as_tibble() %>%
    select(word) %>%
    filter(nchar(word)==5) %>%
    mutate(split_letters=map(word, ~str_split(.x, "")[[1]])) %>%
    filter(map_lgl(split_letters, ~all(.x %in% letters)))

best_letters <- five_letter_words %>%
    select(split_letters) %>%
    unnest(split_letters) %>%
    count(split_letters) %>%
    arrange(desc(n)) %>%
    filter(row_number() <= 15) %>%
    pull(split_letters)

# Now, what are the words in the dictionary that contain those letters and no duplicates?
best_words <- five_letter_words %>%
    filter(map_lgl(split_letters, ~all(.x %in% best_letters)),
           map_int(split_letters, ~max(table(.x)))==1)

########################################################################################################################
# What I need to do now is take the triple cross product of this list of words and check to see if each set of three   #
# covers the full list of 15 best letters. This loop is, uh, more efficient.                                           #
########################################################################################################################

unique_starters <- tibble(word_1=list(),
                          word_2=list(),
                          word_3=list())

# The basic idea here: for each word, remove those letters from the list of best letters, then see which words can be
# constructed out of the remaining letters still available. Then repeat again for the second word.
for (i in seq_len(nrow(best_words))) {
    word_1 <- best_words$split_letters[[i]]
    
    remaining_letters <- best_letters[!best_letters %in% word_1]
    remaining_words <- best_words %>%
        # This filter call both removes all words that can't be spelled with the remaining letters, and also removes all
        # words that come alphabetically before word_1. This is so we don't have duplicates (since a starter of A/B/C
        # and a different starter of A/C/B are the same thing).
        filter(map_lgl(split_letters, ~all(.x %in% remaining_letters)),
               word > best_words$word[[i]])
    
    if (nrow(remaining_words)==0) {
        next
    }
    
    for (j in seq_len(nrow(remaining_words))) {
        word_2 <- remaining_words$split_letters[[j]]
        
        still_remaining_letters <- remaining_letters[!remaining_letters %in% word_2]
        still_remaining_words <- remaining_words %>%
            filter(map_lgl(split_letters, ~all(.x %in% still_remaining_letters)),
                   word > remaining_words$word[[j]])
        
        if (nrow(still_remaining_words)==0) {
            next
        }
        
        for (k in seq_len(nrow(still_remaining_words))) {
            word_3 <- still_remaining_words$split_letters[[k]]
            
            unique_starters <- tibble(word_1=list(word_1),
                                      word_2=list(word_2),
                                      word_3=list(word_3)) %>%
                bind_rows(unique_starters, .)
        }
    }
}

########################################################################################################################
# Okay, turns out there are a lot! But not all of these are equal. Letter position also matters, so if I have all of   #
# these options that are equally good at giving me the most frequently used 15 letters, which ones give me the most    #
# letters in the most common positions?                                                                                #
########################################################################################################################

# I'm going to need to clean this up and tidy it a bit.
cleaned_starters <- unique_starters %>%
    mutate(starter_id=row_number(), .before="word_1") %>%
    pivot_longer(cols=starts_with("word"),
                 names_to="word_num",
                 values_to="split_letters") %>%
    unnest(split_letters) %>%
    group_by(starter_id, word_num) %>%
    mutate(letter_pos=row_number()) %>%
    ungroup()

# Also, for this, I'll need not only the best letters, but also how often each letter shows up in each position.
best_letters_positions <- five_letter_words %>%
    unnest(split_letters) %>%
    group_by(word) %>%
    mutate(letter_pos=row_number()) %>%
    ungroup() %>%
    group_by(split_letters, letter_pos) %>%
    summarize(position_count=n(), .groups="drop") %>%
    filter(split_letters %in% best_letters)

cleaned_starters %>%
    left_join(best_letters_positions, by=c("split_letters", "letter_pos")) %>%
    mutate(position_count=if_else(is.na(position_count), 0L, position_count)) %>%
    group_by(starter_id) %>%
    summarize(position_sum=sum(position_count)) %>%
    arrange(desc(position_sum)) %>%
    inner_join(cleaned_starters, by=c("starter_id")) %>%
    group_by(starter_id, position_sum, word_num) %>%
    summarize(words=toupper(paste(split_letters, collapse="")), .groups="drop") %>%
    group_by(starter_id, position_sum) %>%
    summarize(starter=paste(words, collapse="/"), .groups="drop") %>%
    arrange(desc(position_sum))
