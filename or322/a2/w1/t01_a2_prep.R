#----------------------------------------------------------------------------------------------------
# SECTION 17.2
#----------------------------------------------------------------------------------------------------
#--------------------------------------------------
# PROBLEM 2
#--------------------------------------------------
states = c(0, 1, 2, 3, 4) # the amount of inventory at the start of the day\
P = matrix(c(
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0
), nrow=5, byrow=TRUE)
rownames(P) = colnames(P) = states

P[1,] = c(0, 0, 1/3, 1/3, 1/3) # from 0 to ?: order 4-0=4 units then D=(0:1/3, 1:1/3, 2:1/3)
P[2,] = c(0, 0, 1/3, 1/3, 1/3) # from 1 to ?: ""
P[3,] = c(1/3, 1/3, 1/3, 0, 0) # from 2 to ?: order 0 & D=(0:1/3, 1:1/3, 2:1/3)
P[5,] = c(0, 1/3, 1/3, 1/3, 0) # from 4 to ?: ""
P[4,] = c(0, 0, 1/3, 1/3, 1/3) # from 3 to ?: ""

cat(sprintf(""))
cat(sprintf("17.2: Problem 2: The P matrix is,\u001b[36m\n"))
P
cat(sprintf("\u001b[0m\n"))


#--------------------------------------------------
# PROBLEM 4
#--------------------------------------------------
states = c("SS", "SC", "CC", "CS")
P = matrix(c(
    0.95, 0.05, 0.00, 0.00, # 0.95 SS -> S=SS, 0.05 SS -> C=SC
    0.00, 0.00, 0.60, 0.40,
    0.00, 0.00, 0.80, 0.20,
    0.70, 0.30, 0.00, 0.00
), nrow=4, byrow=TRUE)
rownames(P) = colnames(P) = states
# cat(sprintf("17.2: Problem 4: ?\u001b[36m %f \u001b[0m\n\n", -1))

cat(sprintf("\u001b[35m17.2\u001b[0m: Problem 4: The P matrix is,\u001b[36m\n"))
P
cat(sprintf("\u001b[0m"))
cat(sprintf("The way to tell the number of states is\u001b[36m %s \u001b[0m\n\n", "2^no_days"))


#--------------------------------------------------
# PROBLEM 6 (also 3)
#--------------------------------------------------
cat(sprintf("17.2: Problem 6:\u001b[36m\u001b[0m\u001b[31m %s \u001b[0m\n\n", "Scuffed question"))


#----------------------------------------------------------------------------------------------------
# SECTION 17.3
#----------------------------------------------------------------------------------------------------
#--------------------------------------------------
# PROBLEM 1
#--------------------------------------------------
# Each American family is classified as living in an urban,
# rural, or suburban location. During a given year, 15% of all
# urban families move to a suburban location, and 5% move
# to a rural location; also, 6% of all suburban families move
# to an urban location, and 4% move to a rural location;
# finally, 4% of all rural families move to an urban location,
# and 6% move to a suburban location.
#       (a) If a family now lives in an urban location, what is
#       the probability that it will live in an urban area two years
#       from now? A suburban area? A rural area?
#       (b) Suppose that at present, 40% of all families live in
#       an urban area, 35% live in a suburban area, and 25%
#       live in a rural area. Two years from now, what percent-
#       age of American families will live in an urban area?
#       (c) What problems might occur if this model were used
#       to predict the future population distribution of the United
#       States?
states = c('urban', 'rural', 'suburban')
P = matrix(c(
    0.80, 0.05, 0.15,
    0.04, 0.90, 0.06,
    0.06, 0.04, 0.90
), nrow=3, byrow=TRUE)
rownames(P) = colnames(P) = states

P2 = P%*%P # NOTE: we don't look for P3 as P2 is already a 2 year lookahead.

cat(sprintf("17.3: Problem 1a: An urban family today will live with this distribution in 2 years,\u001b[36m\n"))
P2[1,]
cat(sprintf("\u001b[0m"))

x=P2[1,]
x[1] = 0.40
x[2] = 0.25
x[3] = 0.35
x2 = x%*%P2
cat(sprintf("17.3: Problem 1b: In 2 years the distribution for all families will be,\u001b[36m\n"))
x2
cat(sprintf("\u001b[0m"))

ans="\t- incorrect sensus data\n\t- illy defined categories"
cat(sprintf("17.3: Problem 1c: Problems that might occur include,\n\u001b[36m%s\u001b[0m\n\n", ans))

#--------------------------------------------------
# PROBLEM 3
#--------------------------------------------------
# In Example 2, determine the following n-step transition
# probabilities:
#       (a) After two balls are painted, what is the probability
#       that the state is [0 2 0]?
#       (b) After three balls are painted, what is the probability that
#       the state is [0 1 1]? (Draw a diagram like Figure 4.)
cat(sprintf("17.3: Problem 3: Honestly just too lazy to do this question properly. Too much reading to understand the states in example 2\n\n"))

cat(sprintf("TT01: \u001b[31mI remember this tut. The model is very confusing. Leaving out for now.\u001b[0m\n"))