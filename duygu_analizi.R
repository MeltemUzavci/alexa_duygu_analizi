library(dplyr)

#verinin genel görünümünü incelemek için kullanýlýr. 
glimpse(alexa)


#verinin satýr sayýsý kadar satýr deðeri atadýk. 
amazon_alexa=data.frame(line=1:3150,amazon_alexa)


#veride sütun isimlerini türkçeleþtirdik. 

alexa= amazon_alexa %>%
  select(yorum_numarasi=line,
         degerlendirme_puani=rating,
         tarih=date,
         urun_model=variation,
         yorumlar=verified_reviews,
         geri_donus=feedback)


#tüm yorumlarý küçük harfe çevirdik. 
alexa$yorumlar=sapply(alexa$yorumlar,tolower)

#Birden fazla boþluk var ise o boþluklarýn silinmesi
library(tm)
alexa$yorumlar=sapply(alexa$yorumlar,stripWhitespace)

#noktalama iþaretlerinin metinden kaldýrýlmasý
alexa$yorumlar=sapply(alexa$yorumlar,removePunctuation)  


#istenmeyen kelimelerin listelenmesi
library(tidytext)
stopwords()

#bizim eklemek isteyeceðimiz istemediðimiz kelimeler olabilir onlarý eklemek için;

kaldirilacak_kelimeler<- c("echo","amazon","alexa","device",
                           "sound","time","play","home","bedroom",
                           "price","purchase")


alexa_temiz= alexa %>% 
  unnest_tokens(word, yorumlar) %>%                 #verinin kelimelere ayrýlmasý
  anti_join(stop_words) %>%                         #stop words kelimelerin kaldýrýlmasý
  filter(!word %in% kaldirilacak_kelimeler) %>%     #kendi belirlediðimiz istemediðimiz kelimeler
  filter(nchar(word)>3)                             #üç harften küçük kelimelerin kaldýrýlmasý

#en çok gecen kelimeleri sayan fonksiyon

alexa_kelime_sayisi=alexa_temiz %>%
  count(word,sort=TRUE)


#geliþmiþ kelime bulutu

library(wordcloud2)
wordcloud2(alexa_kelime_sayisi[1:200, ],size=1 )


#DUYGU ANALÝZÝ ADIMLARI
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("nrc")

#bing sözlüðünün veriye dahil edilmesi
alexa_duygu_bing <- alexa_temiz %>%
  inner_join(get_sentiments("bing"))

#nrc sözlüðünün veriye dahil edilmesi
alexa_duygu_nrc <- alexa_temiz %>%
  inner_join(get_sentiments("nrc"))

#afinn sözlüðünün veriye dahil edilmesi
alexa_duygu_afinn <- alexa_temiz %>%
  inner_join(get_sentiments("afinn"))

library(dplyr)
library(ggplot2)

options(repr.plot.width=5, repr.plot.height=4)
tilt_theme <- theme(axis.text.x=element_text(angle=45, hjust=1))
alexa_duygu_bing %>%
  group_by(yorum_numarasi) %>%
  top_n(1) %>%
  ggplot(aes(x=sentiment,y=yorum_numarasi,fill=urun_model)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=urun_model), vjust=1.6, color="black", size=3)+
  ggtitle("Bing sözlüðüne göre yorumlarýn duygu daðýlýmlarý") +
  tilt_theme






