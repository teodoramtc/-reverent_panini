# ucitavanje
Period <-read.table ("C:\\Users\\Slaven\\Desktop\\pslovedata\\pslovedata\\Period.csv", sep=",", header=T)
Symptom <-read.table ("C:\\Users\\Slaven\\Desktop\\pslovedata\\pslovedata\\Symptom.csv", sep=",", header=T)
User <-read.table ("C:\\Users\\Slaven\\Desktop\\pslovedata\\pslovedata\\User.csv", sep=",", header=T)

# rename

Period$user_id <- Period$User_id
User$user_id <- User$id

# spajanje User i Symptom
Sy_Us <- merge(Symptom,User,by="user_id", all=TRUE)

# podvlacenje Period
library(reshape)
Period$User_id <- Period$id <-  NULL
Period_re <-reshape::melt (Period, id = "user_id")
Period_re$date <- Period_re$value
Period_re$period <- Period_re$variable
Period_re$value <- Period_re$variable <-  NULL
head(Period_re)

# sredjivanje datuma
Period_re$date <- as.Date(Period_re$date, format = "%d/%m/%y")
Period_re_clean <- subset(Period_re, date < "2019-01-04" & date > "2014-01-01")

dim(Period_re)
dim(Period_re_clean)
str(Sy_Us)
Sy_Us$date <- as.Date(Sy_Us$date, format = "%d/%m/%y")
Sy_Us_clean <- subset(Sy_Us, date < "2019-01-04")

head(Period_re_clean)
head(Sy_Us_clean)


# spajanje Symptom&User sa Periodom

total <- merge(Period_re_clean, Sy_Us_clean, by=c("user_id","date"), all=TRUE)
total2 <- merge(Period_re_clean, Sy_Us_clean, by=c("date","user_id"), all=TRUE)
write.csv(total, "C:\\Users\\Slaven\\Desktop\\pslovedata\\pslovedata\\total_clean.csv")
write.csv(total2, "C:\\Users\\Slaven\\Desktop\\pslovedata\\pslovedata\\total_clean2.csv")


head (total)
str(total)
dim(total)

# uzimamo samo start date
start <- total[ which(total$period=='start_date'), ]
dim(start)
str(start)

#sort
start <- start[order(start$user_id, start$date),]
head (start, 100)

lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}


start$lagpad_date <-lagpad(start$date, -1)

write.csv(start, "C:\\Users\\Slaven\\Desktop\\pslovedata\\pslovedata\\start.csv")


start$lagpad_user_id <-lagpad(start$user_id, -1)

start$id_raz <- start$lagpad_user_id - start$user_id

start$ciklus <-start$lagpad_date - start$date

start$ciklus[start$id_raz > 0] <- NA

write.csv(start, "C:\\Users\\Slaven\\Desktop\\pslovedata\\pslovedata\\start.csv")

cycle_length <- aggregate(cycle_length_initial~user_id, start, max)
head (cycle_length, 100)

test <- merge(start,cycle_length,by="user_id", all=TRUE)

head(start, 100)

write.csv(test, "C:\\Users\\Slaven\\Desktop\\pslovedata\\pslovedata\\test.csv")
