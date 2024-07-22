# test party
library(partykit)

# create nodes
nodelist <- list(
  # root node
  list(id = 1L, split = partysplit(varid = 4L, breaks = 1.9),
       kids = 2:3),
  # V4 <= 1.9, terminal node
  list(id = 2L),
  # V4 > 1.9
  list(id = 3L, split = partysplit(varid = 5L, breaks = 1.7),
       kids = c(4L, 7L)),
  # V5 <= 1.7
  list(id = 4L, split = partysplit(varid = 4L, breaks = 4.8),
       kids = 5:6),
  # V4 <= 4.8, terminal node
  list(id = 5L),
  # V4 > 4.8, terminal node
  list(id = 6L),
  # V5 > 1.7, terminal node
  list(id = 7L)
)
## convert to a recursive structure
node <- as.partynode(nodelist)

# check
node

## set up party object
data("iris")
tree <- party(node, data = iris,
              fitted = data.frame("(fitted)" =
                                    fitted_node(node, data = iris),
                                  check.names = FALSE))

str( tree )

names(tree) <- paste("Node", nodeids(tree), sep = " ")
## number of kids in root node
length(tree)
## depth of tree
depth(tree)
## number of terminal nodes
width(tree)
## node number four
tree["Node 4"]
tree[["Node 4"]]



plot(tree, main = NULL,
     terminal_panel = node_terminal, tp_args = list(),
     inner_panel = node_inner, ip_args = list(),
     edge_panel = edge_simple, ep_args = list(),
     drop_terminal = FALSE, tnex = 1,
     newpage = TRUE, pop = TRUE, gp = gpar(),
     margins = NULL)

## S3 method for class 'constparty'
plot(x, main = NULL,
     terminal_panel = NULL, tp_args = list(),
     inner_panel = node_inner, ip_args = list(),
     edge_panel = edge_simple, ep_args = list(),
     type = c("extended", "simple"), drop_terminal = NULL,
     tnex = NULL, newpage = TRUE, pop = TRUE, gp = gpar(),
     ...)
## S3 meth
