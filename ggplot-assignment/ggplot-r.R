library(ggplot2)
library(quantreg)
library(hexbin)
library(mapproj)

dataset<-read.csv("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\DT-Credit.csv", header=TRUE, sep=";") 

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\01.png")
ggplot(data = dataset, aes(x=AMOUNT))+geom_area(stat="bin")
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\02.png")
ggplot(data = dataset, aes(x=AMOUNT))+geom_area(stat="bin")+stat_bin(bins=50)
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\03.png")
ggplot(data = dataset ,aes(x=AMOUNT))+ geom_density(kernel = "gaussian")
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\04.png")
ggplot(data = dataset ,aes(x=AMOUNT))+ geom_dotplot()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\05.png")
ggplot(data=dataset ,aes(x=AMOUNT))+geom_freqpoly()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\06.png")
ggplot(data=dataset ,aes(x=AGE))+geom_histogram(binwidth = 5)
dev.off()

# Discrete
png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\07.png")
ggplot(data=dataset ,aes(x=AGE))+geom_bar()
dev.off()

Latlong<-read.csv("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\MplsStops.csv", header=TRUE, sep = ';')

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\08.png")
ggplot(data=Latlong , aes('long','lat')) + geom_polygon(aes(group= 'date'))
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\09.png")
ggplot(data=dataset ,aes(AGE,AMOUNT))+geom_path(lineend="butt",linejoin="round",linemitre=1)
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\10.png")
ggplot(data=dataset ,aes(AGE,AMOUNT))+geom_ribbon(aes(ymin=AMOUNT-5000,ymax=AMOUNT+5000))
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\11.png")
ggplot(data= Latlong ,aes(x='long',y='lat'))+geom_segment(aes(xend='long+delta_long',yend='lat+delta_lat'))
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\12.png")
ggplot(data= Latlong ,aes(x='long',y='lat'))+geom_rect(aes(xmin='long',ymin='lat',xmax='long+delta_long',ymax='lat+delta_lat'))
dev.off()

# Two Variables
# Continuous X, Continuous Y

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\13.png")
ggplot(data=dataset ,aes(AGE,AMOUNT))+geom_blank()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\14.png")
ggplot(data=dataset ,aes(AGE,AMOUNT))+geom_jitter()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\15.png")
ggplot(data=dataset ,aes(AGE,AMOUNT))+geom_point()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\16.png")
ggplot(data=dataset ,aes(AGE,AMOUNT))+geom_quantile()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\17.png")
ggplot(data=dataset ,aes(AGE,AMOUNT))+geom_rug(sides="bl")
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\18.png")
ggplot(data=dataset ,aes(AGE,AMOUNT))+geom_smooth(model="lm")
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\19.png")
ggplot(data=dataset ,aes(AGE,AMOUNT))+geom_text(aes(label = AGE))
dev.off()

# Discrete X, Continuous Y

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\20.png")
ggplot(data=dataset ,aes(EMPLOYMENT,AMOUNT))+geom_bar(stat="identity")
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\21.png")
ggplot(data=dataset ,aes(EMPLOYMENT,AMOUNT))+geom_boxplot()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\22.png")
ggplot(data=dataset ,aes(EMPLOYMENT,AMOUNT))+geom_dotplot(binaxis="y",stackdir="center")
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\23.png")
ggplot(data=dataset ,aes(EMPLOYMENT,AMOUNT))+geom_violin(scale="area")
dev.off()

# Discrete X, Discrete Y

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\24.png")
ggplot(data=dataset ,aes(USED_CAR,NEW_CAR))+geom_jitter()
dev.off()

# Continuous Bivariate Distribution

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\25.png")
ggplot(data=dataset ,aes(DURATION,AMOUNT))+geom_bin2d(binwidth=c(25,15))
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\26.png")
ggplot(data=dataset,aes(DURATION,AMOUNT))+geom_density2d()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\27.png")
ggplot(data=dataset ,aes(DURATION,AMOUNT))+geom_hex()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\28.png")
ggplot(data=dataset,aes(AMOUNT,AGE))+geom_area()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\29.png")
ggplot(data=dataset ,aes(AGE,DURATION))+geom_line()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\30.png")
ggplot(data=dataset ,aes(AMOUNT,AGE))+geom_step(direction="hv")
dev.off()

# Visualizing errors

df <- data.frame(grp = c("A", "B"), fit = 4:5, se = 1:2)

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\31.png")
ggplot(df, aes(grp, fit, ymin = fit-se, ymax = fit+se))+geom_crossbar(fatten=2)
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\32.png")
ggplot(df, aes(grp, fit, ymin = fit-se, ymax = fit+se))+geom_errorbar()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\33.png")
ggplot(df, aes(grp, fit, ymin = fit-se, ymax = fit+se))+geom_linerange()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\34.png")
ggplot(df, aes(grp, fit, ymin = fit-se, ymax = fit+se))+geom_pointrange()
dev.off()

# COORDINATE SYSTEMS

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\35.png")
ggplot(data=dataset ,aes(x=EMPLOYMENT))+geom_bar() + coord_cartesian(xlim=c(0,5))
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\36.png")
ggplot(data=dataset ,aes(x=EMPLOYMENT))+geom_bar() + coord_fixed(ratio = 1/2)
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\37.png")
ggplot(data=dataset ,aes(x=EMPLOYMENT))+geom_bar() + coord_flip()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\38.png")
ggplot(data=dataset ,aes(x=EMPLOYMENT))+geom_bar() + coord_polar(theta = "x", direction=1 )
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\39.png")
ggplot(data=dataset ,aes(x=EMPLOYMENT))+geom_bar() + coord_trans(y = "sqrt")
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\40.png")
ggplot(data=dataset ,aes(x=EMPLOYMENT))+geom_bar() + coord_map(projection = "ortho",orientation=c(41, -74, 0))
dev.off()

# POSITION ADJUSTMENTS

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\41.png")
ggplot(dataset, aes(AGE, fill = AMOUNT)) + geom_bar(position = "dodge", colour="black")
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\42.png")
ggplot(dataset, aes(AGE, fill = AMOUNT)) + geom_bar(position = "fill", colour="black")
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\43.png")
ggplot(data=dataset ,aes(AGE,AMOUNT)) + geom_point(position = "jitter")
dev.off()

# THEMES

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\44.png")
ggplot(data=dataset ,aes(x=AGE))+geom_bar() + theme_bw()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\45.png")
ggplot(data=dataset ,aes(x=AGE))+geom_bar() + theme_grey()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\46.png")
ggplot(data=dataset ,aes(x=AGE))+geom_bar() + theme_classic()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\47.png")
ggplot(data=dataset ,aes(x=AGE))+geom_bar() + theme_minimal()
dev.off()

png("D:\\code\\data-analytics\\r-project\\ggplot-assignment\\plots\\48.png")
ggplot(data=dataset, aes(AGE, AMOUNT)) + geom_point() + ggtitle("New Plot Title ")
dev.off()
