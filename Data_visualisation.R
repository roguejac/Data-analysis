library(ggplot2)


xx <- read.csv("mydata.csv")


index <- matrix(0,NROW(xx),1)
for (i in 1:NROW(xx)) {
  if (xx$rate_female[i]>0 & xx$rate_male[i]>0 & xx$rate_female[i] > xx$rate_male[i]){
    index[i] = 1
  }else if (xx$rate_female[i]<0 & xx$rate_male[i]<0 & abs(xx$rate_female[i]) < abs(xx$rate_male[i])){
    index[i] = 1
  }else if (xx$rate_female[i]>0 & xx$rate_male[i]<0 ){
    index[i] = 1
  }
}



library(ggrepel)
xx <- cbind(xx,index)
head(xx)


ggplot(xx, aes(x= rate_male, y= rate_female, colour=category))+
  geom_point() +geom_hline(yintercept=0,color = "black")+geom_vline(xintercept = 0, color = "black")+
  #geom_text(aes(label=ifelse(index == 1,as.character(Country),'')),hjust=0,vjust=1)+
  scale_color_manual(values = c("Female_decrease male_decrease"="blue","Female_increase male_increase"="brown","Female_increase male_decrease"="black","Female_decrease male_increase"="orange"))+
  xlab("Change in male rate")+ylab("Change in female rate")+ggtitle("Daily smoking prevalence, 1990-2015, percentage point change")+
  annotate(geom="text", x=-3, y=4, label="Female increase, Male decrease", color="black",size = 5)+geom_text_repel(aes(label = ifelse(index == 1,as.character(Country),'')), size = 3, max.overlaps=10)+
  annotate(geom="text", x=-3, y=-5.5, label="Female decrease, Male decrease", color="blue", size = 5)+
  annotate(geom="text", x=2, y=-5.5, label="Female decrease, Male increase", color="orange",size = 5)+
  annotate(geom="text", x=2, y=4, label="Female increase, Male increase", color="brown",size = 5)
                
