#Data analysis about the course of dept. of electrical engineering(NCKU)

library(rvest)
#url need to be update
#url<-"https://course.ncku.edu.tw/index.php?c=qry11215&i=BWhRO1RlB2sNflJ7DmlUZlppU3AGbQF0V2tWPFE4AjUDPgVqWG8CIFA7BmAAaVMkBW1RdFNgB2EIMwY0UT1Yeg1oDGYEMQNuWDFSMghkUmtZKwByVW0DY1ZrVncANwdkVSkDIAlaBDtWaFEkBm4FIwY3BTcFNwciUG8AJlRDVzcFLlEjVGoHLA1sUjIOYlRmWmNTaQZlAWxXYlZvUXkCdwM1BWFYbgJxUHIGKAAgUyQFNVElU2EHNQgzBiVRclhvDWUMOQRzA3dYa1J7CG9SZllqACNVNAM7Vj1WOQA2B2ZVPAN2CT4EJlY9UTcGbwVyBlsFIAU2B31QOAA7VDNXPwU8UTpUMAdrDTRSMg4oVCRaaVNlBj4BdFc0VjBRcgJwA1AFPFg7AnFQOgYiAGlTNAU0UXRTHgcyCCsGPFF6WHwNcgxmBDADb1hyUnoId1JqWTEAO1VjA25WKlY8AGkHMVViAz0JPwRlVjxRbwZvBWEGNgVrBTcHMVBhADFUOVdnBTNRM1RqBzgNP1I5DmJUZ1poUzsGPgFlV2tWPFE4AjUDPgVnWG8CLlByBmsAYlM8BSxRMVN5BzsIawY9UTdYOg18"
#raw_html<-read_html(url)
#write_html(raw_html,"source.html")
raw_html<-read_html("./source.html")

source<-raw_html %>%
  html_nodes(xpath="//body//div[@class='container theme-showcase']//div[@id='main_content']//div[@class='hidden-xs hidden-sm']//div[@id='result']//table//tbody//tr") %>%
  html_text()

tag_head<-"//body//div[@class='container theme-showcase']//div[@id='main_content']//div[@class='hidden-xs hidden-sm']//div[@id='result']//table//tbody//tr["

#Xpath to target data
dept<-"//td[1]"
course_code<-"//td[2]"
grade<-""
course_class<-"//td[3]"
course_name<-"//td[5]//span[@class='course_name']"
credit<-"//td[6]"
attr<-"//td[6]/br"#select/required course
prof<-"//td[7]"
balance<-"//td[8]"
week<-"//td[9]"

dept_v<-c()
course_code_v<-c()
grade_v<-c()
course_class_v<-c()
course_name_v<-c()
credit_v<-c()
attr_v<-c()
prof_v<-c()
balance_v<-c()
week_v<-c()
time_v<-c()

all_data<-list(dept=dept_v,code=course_code_v,grade=grade_v,class=course_class_v,name=course_name_v,
                    credit=credit_v,attr=attr_v,prof=prof_v,balance=balance_v,week=week_v,time=time_v)
data_path<-c(dept,course_code,grade,course_class,course_name,
             credit,attr,prof,balance,week)

for(i in c(1:length(source))){#length(source)
  tag<-paste(tag_head,as.character(i),']',sep="")
  for(j in c(1:2,4:6,8:10)){
    t<-raw_html %>%
      html_nodes(xpath=paste(tag,data_path[j],sep="")) %>%
      html_text()
    #special case:
    if(j==6){
      #ex: 1  必修 (credit,attr)
      t<-strsplit(t,'  ')
      all_data[[6]]<-append(all_data[[6]],t[[1]][1])
      all_data[[7]]<-append(all_data[[7]],t[[1]][2])
    }else if(j==2){
      #ex: E2-112 E231120-2  [EE3440]
      pattern <- "\\[(.*)\\]"
      t<-regmatches(t, regexec(pattern, t))[[1]][2]
      all_data[[2]]<-append(all_data[[2]],t)
    }else if(j==4){
      #ex: 3  乙  A  (grade,class)
      t<-strsplit(t,' ')
      all_data[[3]]<-append(all_data[[3]],t[[1]][1])
      all_data[[4]]<-append(all_data[[4]],t[[1]][3])
    }else if(j==10){
      #ex: "[1]2~3 電機系館 92720[3]2 電機系館 92720"
      t<-strsplit(t,' ')[[1]][1]
      all_data[[10]]<-append(all_data[[10]],substr(t,2,2))
      all_data[[11]]<-append(all_data[[11]],substr(t,4,6))
    }else{
      all_data[[j]]<-append(all_data[[j]],t)
    }
  }
}

all_data<-as.data.frame(all_data)
#all_data2<-all_data
all_data$week[which(all_data$week=="定")]<-NA # NA replace "未定"
#write.csv(all_data,file="./source5.csv",row.names = FALSE)

#delete the duplicated  course
cut_data<-all_data[!duplicated(all_data$name),]

##plot
windows()

#histogram of required/select course
ggplot(cut_data, aes(grade, fill=attr)) +
  geom_bar(position="stack")+
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5,size = 25, face = "bold"),axis.title=element_text(size=14,face="bold"))+
  ggtitle("Course distribution")

#pie chart of group
library(RColorBrewer)
p_data<-read.csv("./new_prof.csv") #getwd() setwd("D://Junior Semester I//R//HW")
counter<-replicate(length(p_data), 0)
counter2<-replicate(length(p_data), 0)
p_counter<-replicate(length(p_data), 0)

for(i in c(1:length(p_data))){
  p_v<-as.character(p_data[[i]])
  for(j in c(1:length(p_v))){
    if(p_v[j]==""){
      p_counter[i]<-j-1
      break
    }else{
      if(j==length(p_v)){
        p_counter[i]<-length(p_v)
      }
      counter[i]<-counter[i]+length(which(all_data$prof==p_v[j]))
      counter2[i]<-counter2[i]+length(which(all_data$prof==p_v[j]&all_data$attr=="選修"))
    }

  }

}



#choose pie chart color
myPalette <- brewer.pal(length(counter), "Set3")
#pie1
pct1<-round(p_counter/sum(p_counter)*100)
lbls1<-paste(names(p_data),pct1,"%")

#pie2
pct2<-round(counter/sum(counter)*100)
lbls2<-paste(names(p_data),pct2,"%")

#pie3
pct3<-round(counter2/sum(counter2)*100)
lbls3<-paste(names(p_data),pct3,"%")

windows(width=200,height=150)
#layout(matrix(c(1,2), ncol=1))
layout(matrix(c(1,1,2,3), ncol=2))
pie(p_counter,labels = lbls1,edges=1000, border="white", col=myPalette)
title("電機系各組教授比例",line = -4,cex.main=2)
pie(counter , labels = lbls2,edges=1000, border="white", col=myPalette,main="電機系所有課程比例",cex.main=2  )
pie(counter2 , labels = lbls3,edges=1000, border="white", col=myPalette,main="電機系選修課程比例",cex.main=2  )


#creat heat map data
heat_map<-matrix(0, 10, 5)

head<-substr(all_data$time,1,1)
head[which(head=='N')]<- '-1'
head<-as.numeric(head)
head[which(head>=5)]<-head[which(head>=5)]+1
head[which(head==-1)]<-5

end<-substr(all_data$time, 3, 3)
end[which(end=="")]<-NA
end<-as.numeric(end)
end[which(end>=5)]<-end[which(end>=5)]+1
end[which(!is.na(head)&is.na(end))]<-head[which(!is.na(head)&is.na(end))]

for( i in c(1:length(source))){
  if(is.na(head[i]))
    next
  heat_map[head[i]:end[i],all_data$week[i]]<-heat_map[head[i]:end[i],all_data$week[i]]+1
}

# Heatmap of course density
library(ggplot2)
library(reshape2)

windows()
x <- c(1:5)
y <- c('A',8:5,'N',4:1)
data <- expand.grid(week=x, section=y)
data$count <- as.vector(t(heat_map))
data$section<-rev(data$section)

color <- colorRampPalette(colors = c("navy","white"))
ggplot(data, aes(week, section, fill= factor(count))) +
  geom_tile()+
  ggtitle("Course density")+
  theme(plot.title = element_text(hjust = 0.5,size = 25, face = "bold"),axis.title=element_text(size=14,face="bold"))+
  scale_fill_manual(values= color(max(heat_map)),guide = guide_legend(title="counts",reverse = TRUE))
