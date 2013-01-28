survey1 = data.frame(StudentID=c(1,2,6), Question1=c('A','B','B'))
survey2 = data.frame(Question1=c('A','B','C'), ID=c(2,3,4), Question2=c('No','No','Yes'))
students = data.frame(StdID=c(1,2,3,4,5), Gender=c('Male','Male','Female','Female','Male'))

survey1
survey2
students

names(survey1)
names(survey2)
names(students)

rbind(survey1, survey2) #This won't work since the column names do not match

survey1$Question2 = NA
survey1

names(survey2)
names(survey2)[2] = "StudentID"

survey2 = survey2[,c('StudentID', 'Question1', 'Question2')]

combinedSurvey = rbind(survey1, survey2)
combinedSurvey

survey1$Administration = 1
survey2$Administration = 2
combinedSurvey = rbind(survey1, survey2)
combinedSurvey

duplicated(combinedSurvey$StudentID)
combinedSurvey[ !duplicated(combinedSurvey$StudentID), ]

merge(combinedSurvey, students, by.x='StudentID', by.y='StdID')
merge(combinedSurvey, students, by.x='StudentID', by.y='StdID', all.x=TRUE)
merge(combinedSurvey, students, by.x='StudentID', by.y='StdID', all.x=TRUE, all.y=TRUE)

