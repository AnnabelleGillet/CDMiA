#!/bin/bash
NEO4J_FOLDER="Neo4j/data" # Replace by your own value

# Simple nodes of 5 attributes
number_of_types_of_nodes=(1 100 200 300 400 500 600 700 800 900 1000)
number_of_nodes=(1 10 100 1000 10000 100000)
for nb_types in "${number_of_types_of_nodes[@]}"
do
    for nb_nodes in "${number_of_nodes[@]}"
    do
        file="nodes_${nb_types}_types_${nb_nodes}_nodes.csv"
	    echo "$nb_types types of node with ${nb_nodes} nodes in file $file"
	    echo "id:ID,att1,att2,att3,att4,att5,:LABEL" > ${NEO4J_FOLDER}/$file
        for (( i=1; i<=$nb_types; i++ ))
	    do
            node_type="label_${i}"
	        for (( j=1; j<=$nb_nodes; j++ ))
	        do
                id=$(( j+((i-1)*nb_nodes) ))
		        echo "$id,att1,att2,3,4,5,:${node_type}" >> ${NEO4J_FOLDER}/$file
	        done
        done
    done
done

# For a fixed number of nodes and a fixed number of attributes, vary the number of edges
number_of_types_of_edges=(100 200 300 400 500 600 700 800 900 1000)
nb_attributes=5
nb_types_of_nodes=100
nb_of_nodes=100
nb_of_edges=10

for nb_types_of_edge in "${number_of_types_of_edges[@]}"
do
    nodes_file="nodescreation_edges_${nb_types_of_edge}_types_nodes_${nb_types_of_nodes}_types.csv"
    edges_file="edgescreation_edges_${nb_types_of_edge}_types_nodes_${nb_types_of_nodes}_types.csv"
	echo "${nb_types_of_edges} types of edge in files $nodes_file and $edges_file"
    
    # Nodes header	
    nodes_header="id:ID"
	for (( i=1; i<$nb_attributes; i++))
	do
		nodes_header="$nodes_header,att$i"
	done
    nodes_header="$nodes_header,:LABEL"
    echo $nodes_header > ${NEO4J_FOLDER}/$nodes_file
    
    # Edges header
    edges_header=":START_ID,:END_ID"
	for (( i=1; i<$nb_attributes; i++))
	do
		edges_header="$edges_header,att$i"
	done
    edges_header="$edges_header,:TYPE"
    echo $edges_header > ${NEO4J_FOLDER}/$edges_file
    
    # Add nodes
	for (( i=1; i<=$nb_types_of_nodes; i++ ))
    do
        node_type="label_${i}"
        for (( j=1; j<=$nb_of_nodes; j++ ))
        do
            id=$(( j+((i-1)*nb_of_nodes) ))
            line="$id"
            for (( a=1; a<$nb_attributes; a++))
	        do
		        line="$line,att$a"
	        done
	        echo "$line,:${node_type}" >> ${NEO4J_FOLDER}/$nodes_file
        done
    done
    
	# Add edges
	space=1
	inc=1
	for (( i=1; i<=$nb_types_of_edge; i++ ))
	do
        edge_type="edge_label_$i"
        if (( $inc > nb_of_nodes ))
        then
        	inc=1
        	space=$(( space+1 ))
        fi
        start_node_type=$(( i%nb_types_of_nodes ))
        end_node_type=$(( (i+space)%nb_types_of_nodes ))
    	for (( j=1; j<=$nb_of_edges; j++ ))
        do
            start_node_id=$(( (start_node_type*nb_of_nodes)+(j%nb_of_nodes) ))
            end_node_id=$(( (end_node_type*nb_of_nodes)+(j%nb_of_nodes) ))
            line="${start_node_id},${end_node_id}"
            for (( a=1; a<$nb_attributes; a++))
            do
	            line="$line,att$a"
            done
            echo "$line,:${edge_type}" >> ${NEO4J_FOLDER}/$edges_file
		done
		inc=$(( inc+1 ))
	done
done

