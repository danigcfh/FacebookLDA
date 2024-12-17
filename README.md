

# **FacebookLDA**

This repository was created as part of my PhD research in political communication. It aims to ensure reproducibility (without including raw data) and provides all scripts and methods used to process Facebook data extracted from **CORTEXT**. The dataset includes posts produced between 2020 and 2021.

### **Contents**

1. **R_LDA_Creation**:
    - Generates randomized representative samples from the initial corpus.
    - Creates individual samples for each page studied and a test sample.
    - Pre-processes the text data.
    - Builds multiple LDA models, iterating over various *k* and *alpha* values from both random representative samples or from specific pages publications. 

2. **R_LDA_Sample_eval** and **LDA_pages_eval**:
    - Evaluates model coherence and perplexity using the test sample.
    - Measures the performance of LDA models.

3. **R_LDA_Sample_viz** and **LDA_pages_viz**:
    - Visualizes the evaluation of model performance per LDA model created from each sample and page respectively. 

4. **R_LDA_Best_Text** and **R_LDA_Best_Test**:
    - Applies the best-performing LDA model to predict topics in the original text processed and in the test dataset respectively.
    - Conducts sentiment and emotion analysis using the **syuzhet** package, with normalization of emotion values.
    - Identifies the most prevalent topics and emotions in hierarchical order for easier interpretation.
    - Creates a consolidated dataset with all analysis outputs.

5. **R_Best_Topic_Viz and R_Emotion_Viz**:
    - Constructs confusion matrices for topic and emotion distribution respectively using hierarchical exact matching.
    - Generates heatmaps to visualize distribution.
    - Identifies emergent clusters using hierarchical clustering (*hclust*).

6. **R_Combined_Viz**:
    - Combines topics and emotions using Manhattan distance metrics.
    - Creates heatmaps for combined topic-emotion metrics.
    - Identifies clusters using *hclust*.

---

### **Usage Notes**

- This repository does not include raw data due to privacy considerations.
- It is a work-in-progress and may not yet cover all aspects of the analysis.
- Scripts are designed to balance methodological soundness and practical usability for political communication research.


### License and Usage Guidelines
This repository is licensed under CC BY-NC-SA 4.0, which ensures that the code is free to use, share, and modify under the following terms:
1. **What You Can Do**
   - Use for Non-Commercial Purposes: You may freely use this code for research, education, or personal projects that do not involve commercial activity.
   - Modify and Adapt: You are encouraged to modify and adapt the code to suit your own purposes.
   - Share Your Modifications: If you modify or build upon this code, you must share your contributions under the same license (CC BY-NC-SA 4.0).
   - Credit the Author: When using or sharing this code, please provide proper attribution, including:
      - My name as the original author.
      - A link to this repository.
      - An  indication of any changes you made.
2. **What You Cannot Do**
   - Commercial Use: You may not use this code, or any derivative works, for commercial purposes without explicit permission.
      - This includes selling, licensing, or otherwise monetizing the code or its derivatives.
      - Re-License Under Incompatible Terms: Derivative works must also remain open and licensed under CC BY-NC-SA 4.0.

### My Philosophy
This code was developed as part of a PhD project funded by the French state. My work reflects the principles of open science and public accessibility. I believe that knowledge and science should remain open and free, avoiding commercialization that could restrict access or benefit only private actors. By sharing this work under these terms, I aim to support collaborative research and equitable access to scientific tools.


### **Contact**

Feel free to reach out with questions or feedback regarding this repository or its methodologies.
email: daniela.gonzalez@sciencespo.fr

This work © 2024 by Daniela González is licensed under CC BY-NC-SA 4.0 
