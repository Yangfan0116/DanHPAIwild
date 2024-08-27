This file aims to provide users with instructions on how to use the DanHPAIwild model step by step. For a short explanation and quick execution, core files of the model package are files in the "R" folder, and for the try-on experience of simulating the model, we have uploaded neccessary data in the "Data\Core data" folder. After downloading the github repo, users can therefore run any excutive code that can be found in the "Model_scripts\03Model_runs\001 Model simulations" folder. Please note, due to the large size of the data, we strongly recommend to use a high performance server to simulate the model, however, our excutive code also give options to run on an ordinary computer.         

This is a detailed description on the model structur:

1. Data Preparation
  The "Data" folder contains original bird counts (NOVANA and DOF data), required packages, assistant shapefiles, a "Data for output analysis" folder, and a "Core data" folder. 
  
  The lastest data from DOF-Birdlife Denmark is publicly available and can be downloaded on https://www.gbif.org/dataset/95db4db8-f762-11e1-a439-00145eb45e9a. However, for replicate the model simulations presented in the paper, we provide the organised datasets of DOF-Birdlife. The data from NOVANA is available by request by contacting the corresponding authors (yali@sund.ku.dk). The data on HPAIV detections are obtained through the Danish passive HPAI surveillance and can be found in the supplementary information.
  
  The "Core data" folder provide model parameters and inputs for simulating based on the raw population and the estimated population.

2. Model Simulation
  The "Model_scripts" folder contains three modelling processes: 1) obtaining the bird abundance ( the "01Bird_counts_est" folder), 2) structuring model inputs (the "02DanHPAIwild_build" folder), and 3) simulating/analysing the model (the "03Model_runs" folder).

  2.1. Obtaining the bird abundance
  We operate on the original forms of NOVANA and DOF counts to get the raw wild bird populations after combining the two types of data. Then, we performe the random forest algorithm to obtain the estimated wild bird populations.
  
  2.2. Structuring model inputs
  We prepare additional model-related data and transforme datasets obtained in 2.1 to the desired form (i.e. cell-week-wise changes of the bird population). We also calibrate the model here.
  
  2.3. Simulating/analysing the model
  You can find the model excutive code for the result replication; both baseline scenarios (using the raw and estimated populations) and altered senarios (sensitivity analysis and controls, using the estimated population). After obtaining the results, you can analysing and visualising model outputs using code in the "002 Analyze model simulations" folder. In the "Figures" folder, we provide figures of our study results. Please note, very slight differences might be found between the users simulated outputs and the our study results, because the model's stochasticity. Neverthless, 400 iterations was considered adequent for convergence.   
  
4. R
  This folder provides the core model scripts for building the Yangfan0116/DanHPAIwild package. 
  
  
