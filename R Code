library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)



##https://raw.githubusercontent.com/CanNuhlar/Turkce-Kelime-Listesi/master/turkce_kelime_listesi.txt

dictionary<-read_tsv("turkish_words.txt")

dictionary<- dictionary%>%
   mutate(a=str_trim(a))%>%
  filter(str_length(a)==5)%>%
  filter(a!="a / e") %>%
  mutate(a=str_to_lower(a))%>%
  filter(!str_detect(a," "))%>%
  filter(!str_detect(a,"û|î|â"))


find_occurences<-function(dictionary)  {
dictionary%>%
  mutate(letter1=str_sub(a,1,1),
         letter2=str_sub(a,2,2),
         letter3=str_sub(a,3,3),
         letter4=str_sub(a,4,4),
         letter5=str_sub(a,5,5))%>%
  gather(key="letter_position",value = "letter",2:6)%>%
  arrange(a)%>%
  group_by(letter)%>%
  summarise(occurences=n())%>%
  ungroup()%>%
  mutate(score=occurences/sum(occurences))%>%
    arrange(score)
}


letter_occurences<-find_occurences(dictionary)




word_to_score<-function(word_m,letter_occurences){
  list<-as_vector(str_split(word_m,""))
  score<-sum(letter_occurences[which(letter_occurences$letter %in%list),3])
  return(score+length(unique(list)))
}




check_matches<-function(solution,word_m){
  ltrs<-as_vector(str_split(word_m,""))
  indices<-which(ltrs!=".")
  let_value<-ltrs[which(ltrs!=".")]
  all(sapply(indices, function(x){return(str_sub(solution,x,x))})==let_value)
}


check_dismatches<-function(solution,word_m){
  
  
  ltrs<-as_vector(str_split(word_m,""))
  indices<-which(ltrs!=".")
  let_value<-ltrs[which(ltrs!=".")]
  
  any(sapply(indices, function(x){return(str_sub(solution,x,x))})==let_value)
}




###############
reset<-function(){
dictionary_solver<<-dictionary
letter_occurences_solver<<-find_occurences(dictionary)
}
  
solver<-function(guess="erika",placed_letters=".....",no_place_letters="....."){
  
  letters_input<-unique(as_vector(str_split(guess,"")))
  
  letters_inclued<-str_c(placed_letters,no_place_letters)
  letters_inclued<-as_vector(str_split(letters_inclued,""))
  letters_inclued<-letters_inclued[which(letters_inclued!=".")]
  letters_exclued<-setdiff(letters_input,letters_inclued)
  
  dictionary_solver<<-
  dictionary_solver%>%
  rowwise()%>%
  filter(sum(str_detect(a,letters_exclued))==0)%>%
  filter(sum(str_detect(a,letters_inclued))==length(letters_inclued))%>%  
  filter(check_matches(a,placed_letters))  %>%
  filter(!check_dismatches(a,no_place_letters))  %>%
  mutate(scr=word_to_score(a,letter_occurences_solver))  %>%
  arrange(desc(scr))
  
  #letter_occurences_solver<<-find_occurences(dictionary_solver)
  
  first_lookup<-
    dictionary_solver%>%
    mutate(letter1=str_sub(a,1,1))%>%
    group_by(letter1)%>%
    summarise(n=n())%>%
    arrange(desc(n))
  
  dictionary_solver<<-
    dictionary_solver%>%
    mutate(letter1=str_sub(a,1,1))%>%
    left_join(first_lookup)%>%
    arrange(desc(n,scr))
  
  return(dictionary_solver[1,1])
}


reset()


solver(guess="kaime",placed_letters=".a...",no_place_letters="..i..")

solver(guess="haris",placed_letters=".ari.",no_place_letters=".....")

solver(guess="tariz",placed_letters=".ari.",no_place_letters=".....")

solver(guess="faril",placed_letters=".aril",no_place_letters=".....")


