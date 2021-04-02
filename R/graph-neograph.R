#' Create an abstract representation of the Bitcoin neo4j graph
#'
#' Signifies that `aPPR` should query the Bitcoin wallet transaction graph via
#' `neo4r`.
#'
#' @param relationship The relationship to query
#'
#' @export

# neo4j graph representation
neo_graph<- function(con=neo4r::neo4j_api$new(url="http://localhost:7474", user="neo4j", password="pass")){
  agraph <- abstract_graph("neo_graph",con=con)
  agraph
}

appr.neo_graph<-function(graph, seeds, ...){
  seeds<-seeds
  seeds

  NextMethod()
}

check.neo_graph<-function(graph,nodes){
  if(length(nodes)<1) return(character(0))
  #seedtolookup<-paste0("'",nodes,"'")
  #query<-paste0('match (w:Wallet{walletID:',seedtolookup,'}) return w.walletID as walletID')
  #node_data<-query %>% call_neo4j(graph$con)
  #if(is.null(node_data$walletID) || nrow(node_data$walletID)<1) return(character(0))
  if(is.null(nodes)) return(character(0))
  #node_data$walletID$value
  nodes<-nodes
  print(nodes)
}

node_degrees.neo_graph <- function(graph, nodes){
  seedtolookup<-paste0("'",nodes,"'")
  inrelation<-'"<SENT_TO"'
  outrelation<-'"SENT_TO>"'
  query<-paste0('match (w:Wallet{walletID:',seedtolookup,'}) return apoc.node.degree(w,',inrelation,') as indegree, apoc.node.degree(w,',outrelation,') as outdegree;')  
  node_datals<-lapply(query,function(q){
  	node_data<-q %>% neo4r::call_neo4j(graph$con)
  	data.frame(indeg=as.numeric(node_data$indegree$value),outdeg=as.numeric(node_data$outdegree$value))
  })
  print(unlist(lapply(node_datals,function(x)x$indeg)))
  list(
     in_degree=unlist(lapply(node_datals,function(x)x$indeg)),
     out_degree=unlist(lapply(node_datals,function(x)x$outdeg))  
  )
  
  #node_data<-query %>% neo4r::call_neo4j(graph$con)
  #list(
  #  in_degree=node_data$indegree$value,
  #  out_degree=node_data$outdegree$value
  #)
}

neighborhood.neo_graph<-function(graph, node){
  seedtolookup<-paste0("'",node,"'")
  query<-paste0('match (w:Wallet{walletID:',seedtolookup,'}) call apoc.neighbors.athop(w,"SENT_TO",1) yield node return node;')
  receivers<-query %>% neo4r::call_neo4j(graph$con)
  if (nrow(receivers$node)<2)character(0) else receivers$node$walletID
}


