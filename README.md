

# **FacebookLDA**

This repository was created as part of my PhD research in political communication. It aims to ensure reproducibility (without including raw data) and provides all scripts and methods used to process Facebook data extracted from **CORTEXT**. The dataset includes posts produced between 2020 and 2021.

### **Contents**

1. **R_LDA_Creation**:
    - Generates randomized representative samples from the initial corpus.
    - Creates individual samples for each page studied and a test sample.
    - Pre-processes the text data.
    - Builds multiple LDA models, iterating over various *k* and *alpha* values.

2. **R_LDA_Sample_eval** and **LDA_pages_eval**:
    - Evaluates model coherence and perplexity using the test sample.
    - Measures the performance of LDA models.

3. **R_LDA_Sample_viz** and **LDA_pages_viz**:
    - Visualizes the evaluation of model performance.

4. **R_LDA_Best_Test**:
    - Applies the best-performing LDA model to predict topics in the test dataset.
    - Conducts sentiment and emotion analysis using the **syuzhet** package, with normalization of emotion values.
    - Identifies the most prevalent topics and emotions in hierarchical order for easier interpretation.
    - Creates a consolidated dataset with all analysis outputs.

5. **R_Best_Topic_Viz**:
    - Constructs confusion matrices for topic distribution using hierarchical exact matching.
    - Generates heatmaps to visualize topic distribution.
    - Identifies emergent clusters using hierarchical clustering (*hclust*).

6. **R_Emotion_Viz**:
    - Constructs confusion matrices for emotion distribution using hierarchical exact matching.
    - Creates heatmaps for emotion visualization.
    - Identifies emotion-based clusters via *hclust*.

7. **R_Combined_Viz**:
    - Combines topics and emotions using Manhattan distance metrics.
    - Creates heatmaps for combined topic-emotion metrics.
    - Identifies clusters using *hclust*.

---

### **Usage Notes**

- This repository does not include raw data due to privacy considerations.
- It is a work-in-progress and may not yet cover all aspects of the analysis.
- Scripts are designed to balance methodological soundness and practical usability for political communication research.

### **Contact**

Feel free to reach out with questions or feedback regarding this repository or its methodologies.
