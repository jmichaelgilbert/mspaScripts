**********************************************************************;
*	410-DL | Assignment 08;
*	Last updated: 2015-08-20 by MJG;
**********************************************************************;

*	Connect to data;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access = readonly;

data EE;
	set mydata.european_employment;
run; quit;

proc contents data = EE;
run; quit;

**********************************************************************;
*	Part 1: Initial Correlation Analysis;
**********************************************************************;

*	Scatterplot of raw data;
*	Pick any two variables;
ods graphics on;
proc sgplot data = EE;
	title 'Scatterplot of Raw Data';
	scatter y = Fin x = Ser /
	datalabel = country group = group;
run; quit;
ods graphics off;

**********************************************************************;
*	Part 2: PCA;
**********************************************************************;

ods graphics on;
	title 'Principal Component Analysis using PROC PRINCOMP';
	proc princomp data = EE out = pca_9components 
		outstat = eigenvectors plots = all;
	run; quit;
ods graphics off;

**********************************************************************;
*	Part 3: Cluster Analysis;
**********************************************************************;

*	Begin discussion of cluster analysis by making a pair of
	scatterplots;
*	How many clusters do you see with each?;
ods graphics on;
proc sgplot data = EE;
	title 'Scatterplot of Raw Data: FIN*SER';
	scatter y = fin x = ser / datalabel = country group = group;
run; quit;
ods graphics off;

ods graphics on;
proc sgplot data = EE;
	title ' Scatterplot of Raw Data: MAN*SER';
	scatter y = man x = ser / datalabel = country group = group;
run; quit;	
ods graphics off;

*	Now use PROC CLUSTER to create a set of clusters algorithmically;
*	Note PROC CLUSTER performs hierarchical clustering so we do not
	need to specify the number of clusters in advance;
*	Will use PROC TREE to assign observations to a specified number
	of clusters after we have performed the hierarchical clustering;
	
*	Different projections of the data will produce different clustering
	results - BE COGNIZANT OF THIS FACT;
*	How do we interpret the measures of CCC, Pseudo F, and Pseudo
	T-Squared? How do we interpret the plots for these three measures?;
ods graphics on;
proc cluster data = EE method = average outtree = tree1
	pseudo ccc plots = all;
	var fin ser;
	id country;
run; quit;
ods graphics off;

*	Now use PROC TREE to assign our data to a set number of clusters;
*	Compare the output when observations are assigned to four and then
	three clusters;
ods graphics on;
proc tree data = tree1 ncl = 4 out = _4_clusters;
	copy fin ser;
run; quit;
ods graphics off;

ods graphics on;
proc tree data = tree1 ncl = 3 out = _3_clusters;
	copy fin ser;
run; quit;
ods graphics off;

*	Now use the macro below to make tables displaying the assignment
	of the observations to the determined clusters;
%macro makeTable(treeout, group, outdata);
data tree_data;
	set &treeout.(rename = (_name_ = country));
run;

proc sort data = tree_data; 
	by country; 
run; quit;

data group_affiliation;
	set &group.(keep = group country);
run;

proc sort data = group_affiliation;
	by country;
run; quit;

data &outdata.;
	merge tree_data group_affiliation;
	by country;
run;

proc freq data = &outdata.;
	table group*clusname / nopercent norow nocol;
run;
%mend makeTable;

*	Now call the macro function;

*	Three clusters;
%makeTable(treeout = _3_clusters, group = EE, 
	outdata = _3_clusters_with_labels);

*	Plot the clusters for a visual display;
ods graphics on;
proc sgplot data = _3_clusters_with_labels;
	title 'Scatterplot of Raw Data';
	scatter y = fin x = ser / datalabel = country group = clusnam;
run; quit;
ods graphics off;

*	Four clusters;
%makeTable(treeout = _4_clusters, group = EE, 
	outdata = _4_clusters_with_labels);

*	Plot the clusters for a visual display;
ods graphics on;
proc sgplot data = _4_clusters_with_labels;
	title 'Scatterplot of Raw Data';
	scatter y = fin x = ser / datalabel = country group = clusnam;
run; quit;
ods graphics off;

*	Display the tables and comment on these results;
*	Did the members of each membership group get clustered into
	the same cluster?;
*	Which number of clusters do you prefer?;

**********************************************************************;
*	Part 4: Cluster Analysis using Principal Components;
**********************************************************************;

***********************************;
*	Create the cluster from first two principal components;
***********************************;
ods graphics on;
proc cluster data = pca_9components method = average outtree = tree3
	pseudo ccc plots = all;
	var prin1 prin2;
	id country;
run; quit;
ods graphics off;

*	Now three clusters;
ods graphics on;
proc tree data = tree3 ncl = 3 out = _3_clusters;
	copy prin1 prin2;
run; quit;
ods graphics off;

*	Now four clusters;
ods graphics on;
proc tree data = tree3 ncl = 4 out = _4_clusters;
	copy prin1 prin2;
run; quit;
ods graphics off;

*	Now call the macros;
%makeTable(treeout = _3_clusters, group = EE, 
	outdata = _3_clusters_with_labels);
%makeTable(treeout = _4_clusters, group = EE, 
	outdata = _4_clusters_with_labels);

***********************************;
*	Plot the clusters for a visual display;
***********************************;
*	Three clusters;
ods graphics on;
proc sgplot data = _3_clusters_with_labels;
	title 'Scatterplot of Raw data';
	scatter y = prin2 x = prin1 / datalabel = country group = clusname;
run; quit;
ods graphics off;

*	Four clusters;
proc sgplot data = _4_clusters_with_labels;
	title 'Scatterplot of Raw data';
	scatter y = prin2 x = prin1 / datalabel = country group = clusname;
run; quit;
ods graphics off;

*	Now call the macros;
%makeTable(treeout = _3_clusters, group = EE, 
	outdata = _3_clusters_with_labels);
%makeTable(treeout = _4_clusters, group = EE, 
	outdata = _4_clusters_with_labels);

**********************************************************************;
*	FIN;
**********************************************************************;
