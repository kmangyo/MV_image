library(rvest)
library(stringr)
library(stringi)
library(rjson)
library(httr)
library(dplyr)
library(ggplot2)
library(reshape2)

library(imager)
#library(EBImage)

theme_set(theme_gray(base_family='NanumGothic'))
#install.packages('imguR')
library(imguR)
# login imgur

image_url<-read.csv(file.choose())
imgur_url<-list()

tkn<-imgur_login()

for (i in 1:nrow(image_url)){
  temp<-get_image(image_url[i,1], token=tkn)
  imgur_url[i]<-temp$link
}

imgur_url <- unlist(imgur_url)

image_tt<-list()

for (i in 1:length(imgur_url)){
  temp <- tempfile()
  download.file(imgur_url[i],temp,mode="wb")
  image_tt[[i]]<- load.image(temp)
}

key <-'xxxx'
visionURL = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true&returnFaceAttributes=age,gender,smile,emotion"

con <- list()

for (i in 1:length(imgur_url)) {
  mybody = list(url = imgur_url[i])
  visionResponse = POST(
    url = visionURL, 
    content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
    body = mybody,
    encode = 'json')
  con[[i]] <- content(visionResponse)
  # because of limit
  if(!is.null(con[[i]]$error$code)){
    Sys.sleep(60)
    mybody = list(url = imgur_url[i])
    visionResponse = POST(
      url = visionURL, 
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
      body = mybody,
      encode = 'json')
    con[[i]] <- content(visionResponse)
  }
}

face <- rep(list(list()), length(imgur_url)) 

for (i in 1:length(imgur_url)) {
  if (is.null(unlist(con[[i]]))){
    face[[i]][1]<-'na'
  } else {
    for(j in 1:length(con[[i]])){
    face[[i]][j]<-unlist(con[[i]][[j]]$faceId)
    }
  }
}  

face<-melt(face)
face<-subset(face,value!=c('na'))

face_top<- rep(list(list()), length(imgur_url)) 

for (i in 1:length(imgur_url)) {
  if (is.null(unlist(con[[i]]))){
    face_top[[i]][1]<-'na'
  } else {
    for(j in 1:length(con[[i]])){
      face_top[[i]][j]<-unlist(con[[i]][[j]]$faceRectangle$top)
    }
  }
}  

face_top<-melt(face_top)
face_top<-subset(face_top,value!=c('na'))

face_left<- rep(list(list()), length(imgur_url)) 

for (i in 1:length(imgur_url)) {
  if (is.null(unlist(con[[i]]))){
    face_left[[i]][1]<-'na'
  } else {
    for(j in 1:length(con[[i]])){
      face_left[[i]][j]<-unlist(con[[i]][[j]]$faceRectangle$left)
    }
  }
}  

face_left<-melt(face_left)
face_left<-subset(face_left,value!=c('na'))

face_length<- rep(list(list()), length(imgur_url)) 

for (i in 1:length(imgur_url)) {
  if (is.null(unlist(con[[i]]))){
    face_length[[i]][1]<-'na'
  } else {
    for(j in 1:length(con[[i]])){
      face_length[[i]][j]<-unlist(con[[i]][[j]]$faceRectangle$width)
    }
  }
}  

face_length<-melt(face_length)
face_length<-subset(face_length,value!=c('na'))

face_pst <- cbind(face_top, face_left, face_length)
face_pst <- face_pst[c(-2,-3,-5,-6)]
names(face_pst)[1:3]<-c('top','left','length')
face_w_pst <- merge(face, face_pst, c('L1','L2'),all.x=T)

# example
# faceRect
# x > left & x < left + width, y > top & y < top + height
# face1<- imsub(pic, x > 391 & x < 391+159, y > 214 & y < 214+159) 
# face2<- imsub(pic, x > 620 & x < 620+221, y > 230 & y < 230+221) 

# image_tt[[1]] %>% plot()
# imsub(image_tt[[face_w_pst[1,1]]], x > face_w_pst[1,5] & x < face_w_pst[1,5]+face_w_pst[1,6], 
#      y > face_w_pst[1,4] & y < face_w_pst[1,4]+face_w_pst[1,6]) %>% plot()

image_face<-list()
for (i in 1:nrow(face_w_pst)){
  image_face[[i]]<- imsub(image_tt[[face_w_pst[i,1]]], x > face_w_pst[i,5] & x < face_w_pst[i,5]+face_w_pst[i,6], 
      y > face_w_pst[i,4] & y < face_w_pst[i,4]+face_w_pst[i,6])
}

facegroupURL = "https://westus.api.cognitive.microsoft.com/face/v1.0/group"
faceIds = list(faceIds = as.character(face$value))

visionResponse = POST(
  url = facegroupURL, 
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
  body = faceIds,
  encode = 'json')
group <- content(visionResponse)
face_group <- melt(group)

#head(face_group)
#head(face_w_pst)
names(face_group)[2:4]<- c('seq_group','group','name')
names(face_w_pst)[1:2]<- c('image.no','seq_pst')
face_w_pst$seq<-1
face_w_pst$seq<-cumsum(face_w_pst$seq)

face_group_pst<-merge(face_group, face_w_pst, c('value'),all.x=T)

face_group_pst_group2<- subset(face_group_pst, group==2 & name==c('groups'))

# test example
image_face[[33]] %>% plot()
par(mfrow = c(1, 1))
image_tt[[37]] %>% plot()

face_group_pst_group <- subset(face_group_pst, name==c('groups'))
face_group_pst_messy <- subset(face_group_pst, name!=c('groups'))
face_group_pst_group <- split(face_group_pst_group, face_group_pst_group$group)

# group1 / 6,13,18 are others
par(mar = rep(1, 4))
par(mfrow = c(5, 9))
for (i in 1:nrow(face_group_pst_group[[1]])){
  temp<-face_group_pst_group[[1]][i,10]
  image_face[[temp]] %>% plot()
}

#messy
par(mar = rep(1, 4))
par(mfrow = c(5, 10))
for (i in 1:nrow(face_group_pst_messy)){
  temp<-face_group_pst_messy[i,10]
  image_face[[temp]] %>% plot()
}

# 6,13,18 are others

# 2 is ziyu
# 3,17 are dahyun
# 4 is chayoung
# 5 is momo
# 7,9 are mina
# 8,10,12 are zihyo
# 14 nayun
par(mar = rep(1, 4))
par(mfrow = c(3, 3))
for (i in 1:nrow(face_group_pst_group[[3]])){
  temp<-face_group_pst_group[[3]][i,10]
  image_face[[temp]] %>% plot()
}


face_age<- rep(list(list()), length(imgur_url)) 

for (i in 1:length(imgur_url)) {
  if (is.null(unlist(con[[i]]))){
    face_age[[i]][1]<-'na'
  } else {
    for(j in 1:length(con[[i]])){
      face_age[[i]][j]<-unlist(con[[i]][[j]]$faceAttributes$age)
    }
  }
}  

face_age<-melt(face_age)
face_age<-subset(face_age,value!=c('na'))
par(mar = rep(3, 4))
par(mfrow = c(1, 1))
hist(face_age$value)

face_emotion<- rep(list(list()), length(imgur_url)) 

for (i in 1:length(imgur_url)) {
  if (is.null(unlist(con[[i]]))){
    face_emotion[[i]][1]<-'na'
  } else {
    for(j in 1:length(con[[i]])){
      face_emotion[[i]][[j]]<-unlist(con[[i]][[j]]$faceAttributes$emotion)
    }
  }
} 

face_emotion<-melt(face_emotion)
face_emotion$seq<-1
face_emotion$seq<-cumsum(face_emotion$seq)
emt<-face_emotion %>% group_by(L1, L2) %>% summarise(cnt=n())
emt$tf<-with(emt, ifelse(cnt==8,1,0))
face_emotion<-merge(face_emotion, emt, c('L1','L2'),all.x=T)
face_emotion<-subset(face_emotion, tf>0)

face_age_emt<-merge(face_age, face_emotion, c('L1','L2'),all.x=T)
names(face_age_emt)[3:4]<-c('age','emotion')
face_age_emt<-subset(face_age_emt, age>1)

face_age_emt$emotion.name<-rep(c('anger','contempt','disgust','fear','happiness','neutral','sadness','surprise'),154)

names(face_age_emt)[1:2] <-c('image.no','seq_pst')

face_group_pst_emt<-merge(face_group_pst, face_age_emt, c('image.no','seq_pst'),all.x=T)

# 2 is ziyu
# 3,17 are dahyun
# 4 is chayoung
# 5 is momo
ggplot(face_group_pst_emt, aes(factor(emotion.name), emotion)) + geom_boxplot()
ggplot(subset(face_group_pst_emt, name==c('groups')), aes(factor(group), age)) + geom_boxplot() 

face_group_pst_emt_group2<- subset(face_group_pst_emt, group==2 & name==c('groups'))
ggplot(face_group_pst_emt_group2, aes(factor(emotion.name), emotion)) + geom_boxplot() 

face_group_pst_emt_part<-subset(face_group_pst_emt, group==2|group==3|group==4|group==5)
ggplot(subset(face_group_pst_emt_part, name==c('groups')), aes(factor(emotion.name), emotion)) + geom_boxplot() + facet_grid(group ~ .)

write.csv(face_group_pst_emt,"face_group_pst_emt.csv")
