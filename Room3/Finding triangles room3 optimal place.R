# In this document I compare the means of the experiments with differenten places of the triangles
# I try to draw a conclusion in which place is the most optimal place given the data
# I used 10 experiments per case and made the customers start on the same location in experiment i for 
# the different places of the triangles. i = 1,2,3,4,5
# Since my sample is quite small (too many experiments takes to much time) is will use a t-test
# the triangle start at y-position 667, it was next to the door post.
# I moved it with 5 pixels away from the door in every experiment, making a total move of 20pixels

# First I load the data
d347 = read.table("room3_st347")
d352 = read.table("room3_st352")
d357 = read.table("room3_st357")
d362 = read.table("room3_st362")
d367 = read.table("room3_st367")

# Now I compute the mean exit time in every experiment
mean(d347[,2])
mean(d352[,2])
mean(d357[,2])
mean(d362[,2])
mean(d367[,2])

# Now since I suspect the 4th experiment presents the optimal case I test if it has a smaller mean than the other means
t.test(unique(d367[,2]), unique(d362[,2]), alternative = "less")
t.test(unique(d367[,2]), unique(d357[,2]), alternative = "less")
t.test(unique(d367[,2]), unique(d352[,2]), alternative = "less")
t.test(unique(d367[,2]), unique(d347[,2]), alternative = "less")
# The position does not seem to matter really (except for the last case in which triangles are to far). 
# We will choose to place it at the positions with lowest mean exit time.
# Conclusion: the vertical wall of the triangles should be at y-coordinates 667 and 433. This room will be used
# in further analysis.