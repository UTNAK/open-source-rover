# Reusable wrapper functions for oml-vision
library(tidyverse)
library(jsonlite)

###############################################################################
#### for sparql

generateQueryDecomposition <- function(df){
  
  text_instance <- ""
  text_instance <- paste0(text_instance,
                          "PREFIX base:        <http://imce.jpl.nasa.gov/foundation/base#>","\n",
                          "PREFIX mission:     <http://imce.jpl.nasa.gov/foundation/mission#>","\n",
                          "PREFIX structure:   <http://opencaesar.io/open-source-rover/vocabulary/structure#>","\n",
                          "PREFIX vim4:        <http://bipm.org/jcgm/vim4#>","\n",
                          "PREFIX base:        <http://imce.jpl.nasa.gov/foundation/base#>","\n",
                          "\n"
  )
  text_instance <- paste0(text_instance,
                          "SELECT DISTINCT ",
                          "?", df$parent, " ",
                          "?", df$parent_instancename, " ",
                          "?", df$parent_id, " ",
                          "?", df$parent_name, " ",
                          "?", df$child, " ",
                          "?", df$child_instancename, " ",
                          "?", df$child_id, " ",
                          "?", df$child_name, " ",
                          "\n",
                          "WHERE {","\n",
                          "\n",
                          "  VALUES ?componentType { ", df$target_type, " }", "\n",
                          "\n"
  )
  text_instance <- paste0(text_instance,
                          "  ?", df$parent, " a ?componentType ;","\n",
                          "    base:hasIdentifier ?", df$parent_id, " ;\n",
                          "    base:hasCanonicalName ?", df$parent_name, " ;\n",
                          "\n",
                          "  OPTIONAL{\n",
                          "    ?", df$parent, " ", df$target_relation, " ?", df$child, " ;\n",
                          "    OPTIONAL{\n",
                          "      ?", df$child, " base:hasIdentifier ?", df$child_id, " ;\n",
                          "          base:hasCanonicalName ?", df$child_name, " .\n",
                          "    }\n",
                          "  }\n",
                          "\n\n"
  )
  
  text_instance <- paste0(text_instance,
                          "  BIND(STRAFTER(STR(?", df$parent, "), \"#\") AS ?", df$parent_instancename, ") .\n",
                          "  BIND(STRAFTER(STR(?", df$child, "), \"#\") AS ?", df$child_instancename, ") .\n"
  )
  text_instance <- paste0(text_instance,
                          " }\n",
                          "ORDER BY ?", df$parent_id, "\n"
  )                          
  
  
}


###############################################################################
#### for DiagramLayout.json


generateDiagramLayoutDecomposition <- function(df){
  
  # text_instance <- ""
  # text_instance <- paste0(text_instance,
  #                         "  \"", df$diagram_id, "\": {\n",
  #                         "    \"name\": \"", df$diagram_id, "\",\n",
  #                         "    \"queries\": {\n",
  #                         #                "      \"", df$diagram_id, "\": \"", df$queryfile, "\",\n",
  #                         "      \"node\": \"", df$queryfile, "\",\n",
  #                         "      \"edge\": \"", df$queryfile, "\"\n",
  #                         "    },\n",
  #                         "    \"rowMapping\": {\n",
  #                         "      \"id\": \"node\",\n",
  #                         "      \"name\": \"Parent\",\n",
  #                         "      \"labelFormat\": \"{", df$parent_node_labelFormat, "}\",\n",
  #                         "      \"nodeColor\": \"", df$parent_node_nodeColor, "\",\n",
  #                         "      \"nodeTextColor\": \"", df$parent_node_nodeTextColor, "\",\n",
  #                         "      \"nodeType\": \"{", df$parent_node_nodeType, "}\",\n",
  #                         "      \"edgeMatchKey\": \"", df$parent_edgeMatchKey, "\"\n",
  #                         "    },\n",
  #                         "    \"edges\": [\n",
  #                         "      {\n",
  #                         "        \"id\": \"edge\",\n",
  #                         "        \"name\": \"Edge\",\n",
  #                         "        \"animated\": true,\n",
  #                         "        \"labelFormat\": \"", df$edge_labelFormat, "\",\n",
  #                         "        \"legendItems\": \"{", df$edge_legendItems, "}\",\n",
  #                         "        \"sourceKey\": \"", df$parent_instancename, "\",\n",
  #                         "        \"targetKey\": \"", df$child_instancename, "\"\n",
  #                         "      }\n",
  #                         "    ]\n",
  #                         "  }\n"
  # )

  new_data <- list(
    name = df$diagram_id,
    queries = list(
      node = df$queryfile,
      edge = df$queryfile
    ),
    rowMapping = list(
      id = "node",
      name = "Parent",
      labelFormat = paste0("{", df$parent_node_labelFormat, "}"),
      nodeColor = df$parent_node_nodeColor,
      nodeTextColor = df$parent_node_nodeTextColor,
      nodeType = df$parent_node_nodeType,
      edgeMatchKey = df$parent_edgeMatchKey
    ),
    edges = list(list(
      id = "edge",
      name = "Edge",
      animated = TRUE,
      labelFormat = df$edge_labelFormat,
      legendItems = df$edge_legendItems,
      sourceKey = df$parent_instancename,
      targetKey = df$child_instancename
    ))
  )
  
  json_string <- toJSON(new_data, pretty = TRUE, auto_unbox = TRUE)
  
}



###############################################################################
#### For Page.json
generatePageDecomposition <- function(df){
  
  text_instance <- ""
  text_instance <- paste0(text_instance,
                          "        {\n",
                          "          \"title\": \"", df$diagram_id, "\",\n",
                          "          \"treeIcon\": \"outline-view-icon\",\n",
                          "          \"path\": \"", df$diagram_id, "\",\n",
                          "          \"isDiagram\": true\n",
                          "        },"
  )
}

###############################################################################
#
omlvisionDecomposition <- function(omlrepo,
                                   targetConcept = c("structure:System structure:Subsystem structure:Assembly"),
                                   targetRelation = "base:isContainedIn",
                                   nodeColor = "green",
                                   nodeTextColor = "white"){
  
  df_keys <- data.frame(
    queryfile = c("auto_components.sparql"),
    parent = c("c1"),
    parent_instancename = c("c1_instancename"),
    parent_id = c("c1_1d"),
    parent_name = c("c1_name"),
    child_instancename = c("c2_instancename"),
    child = c("c2"),
    child_id = c("c2_1d"),
    child_name = c("c2_name"),
    target_type = targetConcept,
    target_relation = targetRelation,
    diagram_id =c("decompositions-autogen")
  )
  
  
  df_keys <- df_keys %>%
    mutate(parent_node_labelFormat = parent_instancename) %>%
    mutate(parent_node_nodeColor = nodeColor) %>%
    mutate(parent_node_nodeTextColor = nodeTextColor) %>%
    mutate(parent_node_nodeType = "NA") %>% # Assembly or Subsystem
    mutate(parent_edgeMatchKey = parent_instancename) %>%
    mutate(edge_labelFormat = targetRelation) %>%
    mutate(edge_legendItems = child_instancename)
  
  df_keys$querytext <- generateQueryDecomposition(df_keys)
  df_keys$diagramLayouttext <- generateDiagramLayoutDecomposition(df_keys)
  df_keys$pagetext <- generatePageDecomposition(df_keys)

  
  outputdir <- paste0(omlrepo,"src/vision/sparql/")
  outputfile <- paste0(outputdir, df_keys$queryfile)
  cat(file=outputfile, df_keys$querytext)
  
  ### Update diagramLayout.json
  #If there is already `df_keys$diagram_id` in diagramLayout.json, update codes to diagramLayouttext
  
  outputdir <- paste0(omlrepo,"src/vision/layouts/")
  readfile <- paste0(outputdir, "diagramLayouts.json")
  # writefile <- paste0(outputdir, "diagramLayouts2.json")
  writefile <- readfile
  
  diagramLayoutjson <- read_json(readfile)
  
  parsedjson <- fromJSON(df_keys$diagramLayouttext)
  
  diagramLayoutjson$`decompositions-autogen` <- parsedjson
  
  
  # Generate diagramLayouts.json
  write_json(diagramLayoutjson, writefile, pretty = TRUE, auto_unbox = TRUE)
  
    
  return(df_keys)
}
