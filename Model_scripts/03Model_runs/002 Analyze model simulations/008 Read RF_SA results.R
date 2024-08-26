setwd("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/003 Simulation outputs")

load("001eta model output.rda")
one <- results
load("002eta model output.rda")
two <- results
load("003epsilon model output.rda")
three <- results
load("004epsilon model output.rda")
four <- results
load("005contact model output.rda")
five <- results
load("006contact model output.rda")
six <- results
load("007contact model output.rda")
seven <- results
load("Estimated_Baseline_outputs.rda")
base <- results
# save.image("H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/003 Simulation outputs/SA 8 all results.rda")

sa_list <- list(one=one, two=two, three=three, four=four, five=five, six=six, seven=seven, base=base)
save(sa_list, file="H:/All/Backup_documents/KU-PhD_030622/Phd Project/PhD plan/Manuscript 2/Submission/DanHPAIwild/DanHPAIwild/Model_scripts/03Model_runs/003 Simulation outputs/Estimated_sa_list.rda")


