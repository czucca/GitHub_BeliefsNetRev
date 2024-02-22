# File to recode domains and remove isolates - if needed

# set directory
# ensure that the package arrow is installed.
#
load("network_analysis.RData")

# recoding multiple domains affiliation into the multidisciplinary category

table(nodes$Domain, useNA = "always")

nodes$Domain[nodes$Domain == "Engineering; Social Sciences; Arts and Humanities"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Engineering; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Engineering; Arts and Humanities; Psychology"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Engineering; Arts and Humanities"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Decision Sciences; Psychology"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Arts and Humanities; Psychology"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Arts and Humanities"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Engineering"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Engineering; Arts and Humanities; Psychology"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Engineering; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Engineering; Social Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Mathematics"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Mathematics; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Psychology"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Social Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Arts and Humanities"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Arts and Humanities; Psychology"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Business Management and Accounting"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Business Management and Accounting; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Business Management and Accounting; Engineering"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Business Management and Accounting; Engineering; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Business Management and Accounting; Mathematics; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Business Management and Accounting; Social Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Decision Sciences; Arts and Humanities; Business Management and Accounting; Psychology"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Engineering; Arts and Humanities"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Engineering; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Engineering; Social Sciences; Business Management and Accounting; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Engineering; Social Sciences; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Mathematics; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Mathematics; Social Sciences; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Social Sciences; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Decision Sciences; Arts and Humanities; Psychology"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Decision Sciences; Engineering; Psychology"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Mathematics; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Business Management and Accounting; Social Sciences; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Social Sciences; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Social Sciences; Arts and Humanities"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Engineering; Mathematics; Social Sciences; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Mathematics; Social Sciences; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Engineering; Social Sciences; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Engineering; Mathematics; Social Sciences; Decision Sciences"] <- "multidisciplinary"
nodes$Domain[nodes$Domain == "Computer Science; Business Management and Accounting; Mathematics; Social Sciences"] <- "multidisciplinary"

# computational social science
nodes$Domain[nodes$Domain == "Computer Science; Mathematics; Social Sciences"] <- "CSS"
nodes$Domain[nodes$Domain == "Computer Science; Engineering; Psychology"] <- "CSS"
nodes$Domain[nodes$Domain == "Engineering; Psychology"] <- "CSS"
nodes$Domain[nodes$Domain == "Mathematics; Psychology"] <- "CSS"
nodes$Domain[nodes$Domain == "Mathematics; Social Sciences"] <- "CSS"
nodes$Domain[nodes$Domain == "Engineering; Social Sciences; Psychology"] <- "CSS"
nodes$Domain[nodes$Domain == "Computer Science; Psychology"] <- "CSS"
nodes$Domain[nodes$Domain == "Engineering; Social Sciences"] <- "CSS"
nodes$Domain[nodes$Domain == "Engineering; Mathematics; Social Sciences"] <- "CSS"
nodes$Domain[nodes$Domain == "Computer Science; Social Sciences"] <- "CSS"
nodes$Domain[nodes$Domain == "Computer Science; Engineering; Social Sciences"] <- "CSS"
nodes$Domain[nodes$Domain == "Computer Science; Mathematics; Psychology"] <- "CSS"
nodes$Domain[nodes$Domain == "Computer Science; Engineering; Mathematics; Social Sciences"] <- "CSS"


# science
nodes$Domain[nodes$Domain == "Engineering; Mathematics"] <- "science"
nodes$Domain[nodes$Domain == "Computer Science; Mathematics"] <- "science"
nodes$Domain[nodes$Domain == "Computer Science; Engineering; Mathematics"] <- "science"
nodes$Domain[nodes$Domain == "Computer Science; Engineering"] <- "science"




table(nodes$Usecase, useNA = "always")
table(nodes$Usecase, nodes$Domain, useNA = "always")

# -----
table(table(nodes$Title))

# making the new network - recoded network - recnet
recnet <- igraph::graph_from_data_frame(edges, nodes, directed = FALSE)



Isolated <- which(igraph::degree(paper_network)==0)
G2 <- igraph::delete.vertices(paper_network, Isolated)

# removing isolates - if needed

# turn it into a network object
GN <- intergraph::asNetwork(G2)

summary(GN ~ edges +
          triangles +
          nodematch("Domain") +
          nodematch("Search.Term") +
          isolatededges +
          absdiff("Year"))

table(nodes$Domain, useNA = "always")
table(nodes$`Search Term`, useNA = "always")

# crosstab

table(igraph::degree(recnet), igraph::get.vertex.attribute(recnet, "Domain"))
