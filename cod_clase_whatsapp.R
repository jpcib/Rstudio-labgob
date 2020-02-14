#DF-IDF
#DFM: Documento Feature Matrix. tokenización, conteo de las veces que aparecen en las tablas
install.packages(tidyte)
install.packages("tidyverse")
install.packages("lubridate")
install.packages("quanteda")
install.packages("stopwords")
#Instalar un paquete directo desde github
install.packages("devtools")
install_github("JBGruber/rwhatsapp")
install.packages("rwhatsapp")
library(tidytext)
library(tidyverse)
library(lubridate)
library(base)
library(quanteda)
library(utils)
library(stopwords)
library(devtools)
library(remotes)
library(rwhatsapp)

chat <- rwa_read("C:/Users/Juan Cib/Desktop/Lab/Clase Whastapp/la_logia.txt")
#esta funcion parsea y ordena el chat

#agregamos una nueva columna para identificar dia y hora de cada comentario filtrando NA
chat <- chat %>%
  mutate(day=date(time))%>% #Lubridate
  filter(author!="NA") %>%
  #se debe filtrar el nombre del grupo porque lo toma como usuario
  filter(author!="La Logia, wacho")%>%
  mutate(hour=hour(time))

#Graficar la frecuencia diaria de chats

chat_por_dia<-chat %>%
  count(day)%>%
  ggplot(aes(x=day,y=n))+
  geom_bar(stat = "identity")+
  scale_y_continuous()
chat_por_dia
#garfico chat por hora
chat_por_hora<- chat%>%
  count(hour) %>%
  ggplot(aes(x=hour, y=n))+
  geom_bar(stat="identity")
chat_por_hora

chat_por_usuario <- chat %>%
  count(author)
ggplot(chat_por_usuario,aes(x=author, y = n))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Que se dice por ahi")
#separamos los emojis de cada usuario
chat_emojis <- chat%>%
  unnest(emoji)%>%
  count(author,emoji)%>%
  group_by(author)%>%
  top_n(n=5)
chat_emojis
#graficamos los emojis mas usasdos por cada uno
ggplot(chat_emojis, aes(x=reorder(emoji,n), y=n))+
  geom_bar(stat="identity")+
  ylab("cantidad")+
  xlab("emoji")+
  coord_flip()+
  facet_wrap(~author, ncol = 3,scales = "free_y")+
  ggtitle("emojis de la gente")+
  theme_light()
chat_emojis
#sacamos las palabras mas usadas
chat_palabras <- chat %>%
  unnest_tokens(input = "text", output = "word") %>%
  select(word, author)%>%
  filter(!word %in% c(stopwords(language="es"),"jaja","jajaj", "jajajaja", "jajaja", "youtu.be", "si", "https", "q", "multimedia","omitido"))%>%
  count(author,word,sort=TRUE)%>%
  bind_tf_idf(term=word,document = author, n=n)%>%
  filter(n>2)%>%
  group_by(author)%>%
  top_n(n=8)
view(chat_palabras)

#graficamos
ggplot(chat_palabras,aes(x=reorder(word,n),y=n,fill=author))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  facet_wrap(~author,ncol=2,scales = "free_y")+
  coord_flip()
