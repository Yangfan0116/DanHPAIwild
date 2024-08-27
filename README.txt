This file aims to provide users with instructions on how to use the DanHPAIwild model step by step. For a short explanation and quick execution, core files of the model package are files in the "R" folder, and for the try-on experience of simulating the model, we have uploaded neccessary data in the "Data\Core data" folder. After downloading the github repo, users can therefore run any excutive code that can be found in the "Model_scripts\03Model_runs\001 Model simulations" folder.       

Here is a detailed description on the model structure.
1. Data Preparation
  There are 
  A. The lastest data from DOF-Birdlife Denmark is publicly available and can be downloaded on https://www.gbif.org/dataset/95db4db8-f762-11e1-a439-00145eb45e9a. However, for replicate the model simulations presented in the paper, we provided the organised datasets of DOF-Birdlife. The data from NOVANA is available by request by contacting the corresponding authors (yali@sund.ku.dk). The data on HPAI detections were obtained through the Danish passive HPAI surveillance and can be found in the supplementary information (in the paper or where?????). In addtion, we provide the final estimated bird abundance dataset namely "E-M M595_Totalbird_2ConseYear.rds".

Other data can be found in the Data folder.

2. Model Simulation
In the folder Model_script, users can find three working directories and should follow the indicated order to perform the complete modelling/analyses process.  

  A. 01Bird_counts_est
  This working directory sorts NOVANA and DOF counts to the desired form, and performs the expectation-maximazation algorithm with GLMM to obtain the final bird abundance (the data is stored in the Data folder).
  
  B. 02DanHPAIwild_build
  Provides codes for Input preparation.
  
  C. 03Model_runs
  This folder provides scripts for running the DanHPAIwild model which simulates HPAI transmission in Denmark, including a baseline model and seven alternative models with different parameters (sensitivity analyses). We recommend users to simulate the stochastic model on the cluster, which would increase the running speed. One can visualize and quantify the simulated outputs with scripts provided in the folder "Analyses on model simulations".
  
4. R
This folder provides scripts of the DanHPAIwild model  - What scripts????? I thought this was tkaen care of in the above?.
  
3. Other information
We (will) also provide an easy-to-access way to inspect the final bird abundance and the disease spatial spread in the folder called "Other info".
  
  
