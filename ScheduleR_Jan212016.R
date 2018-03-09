# Murat Aydogdu
# A plot of course starting times for an academic department with two sets of courses
# Last update: Sep 5, 2015

library("ggplot2")
library("sqldf")
library("plyr")
library("splitstackshape")

setwd('/Users/murataydogdu/Desktop/DataScience/R_DeptSchedule')
getwd()
# Get the output from the RICONNECT report directly

Courses <- read.csv("Spring2016_ZRSR_DEPT_SEM_CLASSES_DAY_ALL.csv")
Courses <- cSplit(Courses, "Name", ",")
Courses$Course <- paste(Courses$Subject, Courses$Catalog, 
                        Courses$Section, Courses$Name_1 , sep="-")

Courses <- cSplit(Courses, "Mtg.Start", ":")
Courses$Mtg.Start_3 <- sub("00.000000","", Courses$Mtg.Start_3)
Courses$Mtg.S <- ifelse(Courses$Mtg.Start_3 == "PM" & Courses$Mtg.Start_1 < 11, Courses$Mtg.Start_1 + 12, Courses$Mtg.Start_1)
Courses$StartTime <- paste(Courses$Mtg.S, formatC(Courses$Mtg.Start_2, width=2, flag="0"), sep=":")

Courses <- subset(Courses, select = c(Subject, Course, StartTime, Mon, Tues, Wed, Thurs, Fri))
# Subject 	Course          	StartTime	Mo	Tu	We	Th	Fr
# ECON    	ECON-200-1-Carr 	18:00   	N	Y	N	N	N
# ECON    	ECON-200-2-Marsis	8:00    	Y	N	Y	N	N

Courses

Sch <- read.csv("STimes.csv") # Start times for all courses along with a counter: 8:00, 1 ... 18:00, 21
#     STime	SCount	
# 1 	 8:00	 1   
# 2 	 8:30	 2   
# 21	18:00	21  

Sch$RevCnt <- 22 - Sch$SCount # This will be used in the graph
Sch

# Days on which a course takes place
t1 <- sqldf("select Subject,Course, 'Mo' as Day, StartTime from Courses where Mon = 'Y'")
t2 <- sqldf("select Subject,Course, 'Tu' as Day, StartTime from Courses where Tues = 'Y'")
t3 <- sqldf("select Subject,Course, 'We' as Day, StartTime from Courses where Wed = 'Y'")
t4 <- sqldf("select Subject,Course, 'Th' as Day, StartTime from Courses where Thurs = 'Y'")
t5 <- sqldf("select Subject,Course, 'Fr' as Day, StartTime from Courses where Fri = 'Y'")
t <- rbind (t1,t2,t3,t4,t5)

# Join the courses with the course times
t_all <- sqldf("select a.*,b.* from t a left join Sch b
               on a.StartTime = b.STime
               order by Course, Day")
t_all
table(t_all$Day)

# Factorize course days and times 
t_all$D <- factor(t_all$Day, levels = c("Mo", "Tu", "We", "Th", "Fr"))
t_all$D <- mapvalues(t_all$D, from = c("Mo", "Tu", "We", "Th", "Fr"), to = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
table(t_all$D)

t_all$Times <- factor(t_all$STime,levels = 
                        c("18:00","17:30",
                          "17:00","16:30","16:00","15:30",
                          "15:00","14:30","14:00","13:30","13:00",
                          "12:30","12:00","11:30","11:00","10:30",
                          "10:00" ,"9:30","9:00","8:30","8:00"))
table(t_all$Times)

p <- ggplot(t_all, aes(x=D, y=factor(RevCnt), label=Course, colour=Subject))+
  theme(panel.grid.major.x  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        panel.grid.major.y  = element_line(colour = "grey95", size=0.25, linetype = "solid"), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=8), 
        panel.background = element_blank(),
        panel.border = element_blank())+
  labs( title = "Economics and Finance Department Spring 2016 Schedule", x="Days", y="Start time") +
  scale_y_discrete(breaks = c(21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
                   labels = c("8:00","8:30","9:00","9:30",
                              "10:00","10:30","11:00","11:30","12:00",
                              "12:30","13:00","13:30","14:00","14:30",
                              "15:00","15:30","16:00","16:30","17:00",
                              "17:30","18:00")) 

p + geom_text(fontface=3, size=3, #show_guide = FALSE,
              position = position_jitter(w=0.1, height=0.2))+
  scale_color_manual(values = c("green2", "blue2")) +
  guides(color = FALSE)







