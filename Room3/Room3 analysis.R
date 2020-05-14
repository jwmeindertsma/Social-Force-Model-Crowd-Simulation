# In this file I will discuss the first third room (with triangles on the sides of the door) and
# compare it to the standard room (room 1). I kept the width of the door fixed. As could be seen in the file
# "Finding optimal triangle position" the triangle has a distance to the sides of the door (when focussing on the horizontal
# side of the triangle) of 15 pixels ( it is at y-positions 667 and 433). Now we will compare the mean exit time for room 1
# and room 2 for the case with one door and do robustness-checks using 2 doors, random sizes/weight and visibility.
# Also I will plot a graph of the average exit times per person against the number of players left. This gives an 
# indication in how fluently the system is evacuated. 
library(dplyr)
# First I load the data
room1st = as.data.frame(read.table("room1_st"))
room1r = as.data.frame(read.table("room1_r"))
room1twod = as.data.frame(read.table("room1_twodoors"))
room1vis = as.data.frame(read.table("room1_vis"))
room3st = as.data.frame(read.table("room3_st367"))
room3r = as.data.frame(read.table("room3_r367"))
room3twod = as.data.frame(read.table("room3_twodoors367"))
room3vis = as.data.frame(read.table("room3_vis367"))

# Now for room1 and room3 with 1 door, with persons with weight 80 and radius of 12 pixels
# Changing column names
names(room1st) = c("exit_pp", "exit_group", "rank", "nr_collisions")
names(room3st) = c("exit_pp", "exit_group", "rank", "nr_collisions")
# The mean exit times are
mean(unique(room1st[,2])) # room 1
mean(unique(room3st[,2])) # room 3

# Testing for the fact the exit time in room3 is smaller gives
t.test(unique(room1st[,2]), unique(room3st[,2]), alternative = "greater")
# So there is strong-evidence at 1% significance level, that the triangles decrease exit times

# A plot of the average departure times against the number of people left is:
# Green line for room 1
# Red line for room 3

# Finding average exit time for person i, i = 0,..,72. Room 1
# Deleting observations that are 0 for last 2 persons
y1 = room1st[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarise_at(group_by(y1, rank), vars(exit_pp), mean)
y1 = y1$exit_pp
x1 = 0:72

# Finding average exit time for person i, i = 0,..,72. Room3
# Deleting observations that are 0 for last 2 persons
y2 = room3st[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y2 = y2[y2[,1]>0, ]
y2 = summarise_at(group_by(y2, rank), vars(exit_pp), mean)
y2 = y2$exit_pp
x2 = 0:72
# Plotting both curves in one figure
plot(x1, y1, col = "Green", type = "l", ylim = c(0,100), xlab =  "Rank", ylab = "Exit time for person", 
     main = "Room3: Outflow Comparison Standard Case")
lines(x2,y2, col = "Red")

# Now for room1 and room3 with 1 door, with persons with weights between 60 and 100 (uniform)
# and radius of 12/80*weight pixels
# Changing column names
names(room1r) = c("exit_pp", "exit_group", "rank", "nr_collisions")
names(room3r) = c("exit_pp", "exit_group", "rank", "nr_collisions")
# The mean exit times are
mean(unique(room1r[,2])) # room 1
mean(unique(room3r[,2])) # room 3

# Testing for the fact the exit time in room3 is smaller gives
t.test(unique(room1r[,2]), unique(room3r[,2]), alternative = "greater")
# So there is strong-evidence at 1% significance level, that the triangles decrease exit times

# A plot of the average departure times against the number of people left is:
# Green line for room 1
# Red line for room 3

# Finding average exit time for person i, i = 0,..,72. Room 1
# Deleting observations that are 0 for last 2 persons
y1 = room1r[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarise_at(group_by(y1, rank), vars(exit_pp), mean)
y1 = y1$exit_pp
x1 = 0:72

# Finding average exit time for person i, i = 0,..,72. Room3
# Deleting observations that are 0 for last 2 persons
y2 = room3r[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y2 = y2[y2[,1]>0, ]
y2 = summarise_at(group_by(y2, rank), vars(exit_pp), mean)
y2 = y2$exit_pp
x2 = 0:72
# Plotting both curves in one figure
plot(x1, y1, col = "Green", type = "l", ylim = c(0,100), xlab =  "Rank", ylab = "Exit time for person", 
     main = "Room3: Outflow Comparison Randomized Case")
lines(x2,y2, col = "Red")

# Now for room1 and room3 with 2 doors, with persons with weight 80 and radius of 12 pixels
# Changing column names
names(room1twod) = c("exit_pp", "exit_group", "rank", "nr_collisions")
names(room3twod) = c("exit_pp", "exit_group", "rank", "nr_collisions")
# The mean exit times are
mean(unique(room1twod[,2])) # room 1
mean(unique(room3twod[,2])) # room 3

# Testing for the fact the exit time in room3 is smaller gives
t.test(unique(room1twod[,2]), unique(room3twod[,2]), alternative = "greater")
# So there is strong-evidence at 1% significance level, that the triangles decrease exit times

# Finding average exit time for person i, i = 0,..,70. Room 1
# Deleting observations that are 0 for last 4 persons
y1 = room1twod[-c(seq(72, 746+75, by = 75), seq(73, 747+75, by = 75), seq(74, 748+75, by = 75), 
                  seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarise_at(group_by(y1, rank), vars(exit_pp), mean)
y1 = y1$exit_pp
x1 = 0:70

# Finding average exit time for person i, i = 0,..,70. Room2
# Deleting observations that are 0 or for last 4 persons
y2 = room3twod[-c(seq(72, 746+75, by = 75), seq(73, 747+75, by = 75),
                  seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y2 = y2[y2[,1]>0, ]
y2 = summarise_at(group_by(y2, rank), vars(exit_pp), mean)
y2 = y2$exit_pp
x2 = 0:70
# Plotting both curves in one figure
plot(x1, y1, col = "Green", type = "l", ylim = c(0,100), xlab =  "Rank", ylab = "Exit time for person", 
     main = "Room3: Outflow Comparison Two Doors Case")
lines(x2,y2, col = "Red")


# Now for room1 and room3 with 1 door, with persons with weight 80 and radius of 12 pixels and bad visibility
# Changing column names
names(room1vis) = c("exit_pp", "exit_group", "rank", "nr_collisions")
names(room3vis) = c("exit_pp", "exit_group", "rank", "nr_collisions")
# The mean exit times are
mean(unique(room1vis[,2])) # room 1
mean(unique(room3vis[,2])) # room 3

# Testing for the fact the exit time in room3 is smaller gives
t.test(unique(room1vis[,2]), unique(room3vis[,2]), alternative = "greater")
# So there is strong-evidence at 1%-significance level, that the triangles will decrease exit times in case of
# bad visibility.

# Finding average exit time for person i, i = 0,..,72. Room 1
# Deleting observations that are 0 for last 2 persons
y1 = room1vis[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarise_at(group_by(y1, rank), vars(exit_pp), mean)
y1 = y1$exit_pp
x1 = 0:72

# Finding average exit time for person i, i = 0,..,72. Room3
# Deleting observations that are 0 for last 2 persons
y2 = room3vis[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y2 = y2[y2[,1]>0, ]
y2 = summarise_at(group_by(y2, rank), vars(exit_pp), mean)
y2 = y2$exit_pp
x2 = 0:72
# Plotting both curves in one figure
plot(x1, y1, col = "Green", type = "l", ylim = c(0,100), xlab =  "Rank", ylab = "Exit time for person",
     main = "Room3: Outflow Comparison Bad Visibility Case")
lines(x2,y2, col = "Red")
