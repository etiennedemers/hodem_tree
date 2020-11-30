library(dplyr)
library(combinat)

# split data into small blind and big blind hands, recombine and join to grid of all possible terminal nodes

df_sb <- df_bucket %>%
  filter(sb_cards != "") %>%
  mutate(card_seq = paste0(sb_pf_card_bucket, sb_f_card_bucket, sb_t_card_bucket, sb_r_card_bucket),
         action_seq = paste0(pf_bucket, f_bucket, t_bucket, r_bucket),
         position = "sb") %>%
  select(position, card_seq, action_seq)

df_bb <- df_bucket %>%
  filter(bb_cards != "") %>%
  mutate(card_seq = paste0(bb_pf_card_bucket, bb_f_card_bucket, bb_t_card_bucket, bb_r_card_bucket),
         action_seq = paste0(pf_bucket, f_bucket, t_bucket, r_bucket),
         position = "bb") %>%
  select(position, card_seq, action_seq)

df_all <- rbind(df_sb, df_bb) %>%
  mutate(agg = paste0(position, card_seq, action_seq),
         count = 1)

combs <- c()

for (len in 1:4){
  
  for (g in 0:len){
    b = len - g
    
    ls <- permn(c(rep("G", g), rep("B", b)))
    combs <- c(combs, unlist(unique(lapply(ls, paste, collapse=""))))
  }
}

combs2 <- c("1", "2", "31", "32", "331", "341", "332", "342")

for (i in c(3,4)){
  for (j in c(3,4)){
    for (k in c(1:4)){
      combs2 <- c(combs2, paste(c("3",i,j,k), collapse=""))
    }
  }
}

grid <- expand.grid(position = c("sb", "bb"),
                    card_seq = combs, 
                    action_seq = combs2, 
                    stringsAsFactors = FALSE) %>%
  filter(nchar(card_seq) == nchar(action_seq)) %>%
  left_join(df_all, by = c("position","card_seq","action_seq")) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  group_by(position, card_seq, action_seq) %>%
  summarise(count = sum(count))