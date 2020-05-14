# In this document I compare the means of the experiments with differenten places of the triangle
# I try to draw a conclusion in which place is the most optimal place given the data
# I used 10 experiments per case and made the customers start on the same location in experiment i for 
# the different places of the triangle. i = 1,2,3,4,5
# Since my sample is quite small (too many experiments takes to much time) is will use a t-test
# the triangle start at position 670, there fitted just 1 person between it and the door at this position
# I moved it with 5 pixels away from the door in every experiment, making a total move of 20pixels

# First I load the data
d670 = read.table("room2_st670")
d665 = read.table("room2_st665")
d660 = read.table("room2_st660")
d655 = read.table("room2_st655")
d650 = read.table("room2_st650")

# Now I compute the mean exit time in every experiment
mean(d670[,2])
mean(d665[,2])
mean(d660[,2])
mean(d655[,2])
mean(d650[,2])

# Now since I suspect the 4th experiment presents the optimal case I test if it has a smaller mean than the other means
t.test(unique(d665[,2]), unique(d670[,2]), alternative = "less")
t.test(unique(d665[,2]), unique(d650[,2]), alternative = "less")
t.test(unique(d665[,2]), unique(d660[,2]), alternative = "less")
t.test(unique(d665[,2]), unique(d655[,2]), alternative = "less")
# Conclusion: the vertical wall of the triangle should be at x-coordinate 665. Regarding the people with different
# sizes and weight it should be at x-coordinate 660, close to the door but still fitting for fatter people. 