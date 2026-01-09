#Violin plot

library(ggplot2)
library(reshape2)

Melt_virus6 <- melt(virus6, id.vars=c("Sample", "Site"))

Melt_virus6[460,]$value=0

Melt_virus6$value <- as.numeric(Melt_virus6$value)

ggplot(Melt_virus6, aes(x = variable, y = as.numeric(value)))+ geom_bar(stat = 'identity')

ggplot(Melt_virus6, aes(x= "variable", y= as.numeric(value))) + 
  geom_violin()+facet_wrap(~variable, scales="free")

## Makes a box plot 

ggplot(`Eat2`, aes(x=Day, y="a","b" )) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

## Makes a bar plot for qPCR fold change data w/ standard error bars

ggplot(data=Dwv3, aes(x=Treatment, y=Mean))+
geom_bar(stat="Identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.2,
                position=position_dodge(.9)) +
  ggtitle("DWV")
