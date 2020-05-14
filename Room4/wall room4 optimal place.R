# In this document I compare the means of the experiments with differenten places of the wall in room 4
# I try to draw a conclusion in which place is the most optimal place given the data
# I used 10 experiments per case and made the customers start on the same location in experiment i for 
# the different places of the triangles. i = 1,2,3,4,5
# Since my sample is quite small (too many experiments takes to much time) is will use a t-test
# the triangle start at x-position 675, it was 25 pixels from the door.
# I moved it with 5 pixels away from the door in every experiment, making a total move of 20pixels

# First I load the data
d675 = read.table("room4_st675")
d670 = read.table("room4_st670")
d665 = read.table("room4_st665")
d660 = read.table("room4_st660")
d655 = read.table("room4_st655")

# Now I compute the mean exit time in every experiment
mean(d675[,2])
mean(d670[,2])
mean(d665[,2])
mean(d660[,2])
mean(d655[,2])

# Now since I suspect the 3th experiment presents the optimal case I test if it has a smaller mean than the other means
t.test(unique(d665[,2]), unique(d675[,2]), alternative = "less")
t.test(unique(d665[,2]), unique(d670[,2]), alternative = "less")
t.test(unique(d665[,2]), unique(d660[,2]), alternative = "less")
t.test(unique(d665[,2]), unique(d655[,2]), alternative = "less")
# The position does seem to matters (except for the comparison with the 660 case). 
# We will choose to place the wall at the x-position 665
# Conclusion: the end of the wall should be at x-coordinates 665. This room will be used
# in further analysis.