# FacebookLDA


This repository is made in the framework of my PhD research and it aims to ensure the possibility of reproducibility if needed. Without including the raw data, this repository contains all the manipulations made to Facebook data extracted from CORTEXT. It includes:


- The creation of randomized representative samples from the initial corpus as well as individual samples from every individual page studied and a test sample (LDA_Creation)
- pre-processing of the corpus and creation of multiple LDA models iterating over several k and alpha values(LDA_Creation)
- Evaluation of coherence and perplexity of all models with a test sample order to identify best performance (LDA_Sample_eval and LDA_pages_eval)
- Visualization of performance evaluation (LDA_Sample_viz and LDA_pages_viz)
- Application of the best performing models to an evaluation sample to identify topic distribution (LDA_SampleTopicDistribution)
- Visualization of topic-distribution (rplot11, rplot13) and identification of emergent clusters via hclust (LDA_SampleTopicDistribution)
- Miscellaneous analysis (mis_analysis)

This is not entirely comprehensive nor finished but provides an overview of my work thus far.


