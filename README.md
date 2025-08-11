# A/B Testing Analysis Using Frequentist and Bayesian Inference

This project evaluates the performance of a new landing page design using a simulated A/B test. The analysis employs a dual approach, combining classical Frequentist methods with modern Bayesian inference to provide a robust, evidence-based recommendation for business decision-making.

---

## 🎯 Problem Statement

The objective of this project is to demonstrate a comprehensive A/B testing workflow, from experiment design and data analysis to generating clear, actionable business recommendations. The analysis aims to determine if the observed difference between a control and a treatment group is statistically significant and practically meaningful.

---

## 📈 Methodology

    Experiment Simulation & Data Cleaning: An experiment for a dataset of approximately 294,000 users was simulated to model an A/B test experiment. The data was inspected for integrity and prepared for analysis.

    Frequentist Analysis: Classical hypothesis testing was performed using a p-value and a confidence interval to determine the statistical significance of the results.

    Bayesian Analysis: A Bayesian approach was implemented using a Beta(1,1) prior. The posterior distribution was used to calculate the probability that the new page's conversion rate is higher than the original's and to define a Region of Practical Equivalence (ROPE).

    Expanded Analysis: A Direct Posterior Comparison by country to evaluate the treatment on a segmented analysis.

    Conclusion & Recommendations: The final decision is based on a synthesis of both statistical frameworks to provide a comprehensive, actionable recommendation.

---

## 📊 Datasets Overview

Two datasets were used for this analysis:

    ab_data.csv

        Instances: 294,478

        Features: 5 features (user_id, timestamp, group, landing_page, converted) 

            user_id: A unique identifier for each user.

            timestamp: The time and date the user's action was recorded.

            group: Indicates whether the user was in the control or treatment group.

            landing_page: Indicates the version of the page the user saw (old_page or new_page).

            converted: A binary outcome (1 for conversion, 0 for no conversion).

    countries.csv

        Instances: 290,584

        Features: 2 features (user_id, country)
                    
            user_id: A unique identifier for each user.

            country: The country of origin for each user

---

## 📄 Key Findings

The initial global analysis, using both Frequentist and Bayesian methods, concluded that there was insufficient evidence to justify deploying the new landing page. The observed lift was practically negligible.

However, an expanded segmented analysis by country revealed nuanced regional performance. The Bayesian analysis suggested a higher probability of the new page outperforming the old in certain regions, which shifted the final recommendation from a simple "no-go" to a more strategic, targeted approach.

---

## 📁 Project Structure

    data/ – The datasets used for the A/B test.

    src/ – The core modular R scripts: (main.R, frequentist.R, bayesian.R, etc) containing the complete analysis workflow.

    report/ – The full analytical report in PDF format.

    presentation/ – A slide deck summarizing the project's key findings.

---

## 🚀 How to Run

    Open RStudio in the project root directory.

    Run the main script src/main.R to execute the full analysis.

    Ensure the required packages (tidyverse) are installed.

---

## 📄 License

This repository and its contents are for personal portfolio use and educational purposes.

This project uses a dataset from Udacity's Data Analyst Nanodegree course.