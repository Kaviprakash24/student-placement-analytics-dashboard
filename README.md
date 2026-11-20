# 🎓 Student Placement Analytics & Employability Insights Dashboard

[![R](https://img.shields.io/badge/R-4.6%2B-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-v1.10-blue?style=for-the-badge&logo=rstudio&logoColor=white)](https://shiny.posit.co/)
[![Bootstrap 5](https://img.shields.io/badge/Bootstrap-5.3-7952B3?style=for-the-badge&logo=bootstrap&logoColor=white)](https://getbootstrap.com/)
[![Plotly](https://img.shields.io/badge/Plotly-Interactive-3F4F75?style=for-the-badge&logo=plotly&logoColor=white)](https://plotly.com/r/)
[![NIT Jalandhar](https://img.shields.io/badge/NIT_Jalandhar-CSE_Department-orange?style=for-the-badge)](https://www.nitj.ac.in/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg?style=for-the-badge)](LICENSE)

An end-to-end, empirical data analytics study and interactive R Shiny dashboard investigating undergraduate employability, technical competency differentiators, preparation bottlenecks, and salary determinants across engineering cohorts at **Dr. B. R. Ambedkar National Institute of Technology (NIT), Jalandhar**.

---

## 📌 Executive Summary & Key Highlights

Campus recruitment outcomes are often assumed to be driven equally by academic performance (CGPA), soft skills, and coding practice. This empirical research analyzes survey data across undergraduate engineering students to determine the **true statistical differentiators** between placed and unplaced candidates.

### 🌟 Signature Empirical Findings

| Metric / Dimension | Placed Students | Unplaced Students | Statistical Gap | Primary Analytical Implication |
| :--- | :---: | :---: | :---: | :--- |
| **Data Structures & Algorithms (DSA)** | **3.94 / 5.0** | **2.57 / 5.0** | **+1.37 (+53.3%)** | **The #1 Technical Differentiator** for campus placement offers. |
| **Object-Oriented Programming (OOP)** | **4.00 / 5.0** | **2.63 / 5.0** | **+1.37 (+52.1%)** | Essential core requirement for passing technical interviews. |
| **Communication & Soft Skills** | **3.35 / 5.0** | **3.13 / 5.0** | **+0.22 (+7.0%)** | **Statistically indistinguishable**; sets an entry baseline but does not decide outcomes. |
| **Premium Salary Threshold (>12 LPA)** | **4.33 / 5.0 DSA** | N/A | **Monotonic Staircase** | Breaking the 12+ LPA salary ceiling demands near-perfect algorithmic mastery. |
| **The "Mock Wakeup" Shift** | **12%–34% Confidence** | **80% Overconfidence** | **-46% to -68%** | Students with 0 mock interviews suffer from extreme false readiness that collapses under live grilling. |
| **The Resume Reality Disconnect** | **3+ Projects / Active Git** | **7.7% Zero Projects** | Structural Void | Many students with "fully updated" resumes lack actual projects or active GitHub repositories. |

---

## 🎯 Problem Statement & Research Objectives

### Problem Statement
In tier-1 engineering institutions, students dedicate hundreds of hours to placement preparation across coding platforms, mock sessions, and academic coursework. However, many students face rejections due to misaligned preparation priorities—such as overemphasizing cosmetic resume formatting or superficial communication skills while neglecting deep algorithmic competencies or project-based practical implementations.

### Core Objectives
1. **Assess Cohort Baseline:** Evaluate self-reported technical and soft skill proficiencies across 9 key engineering domains.
2. **Identify Hiring Differentiators:** Statistically contrast placed vs. unplaced students to determine which skills dictate recruitment success.
3. **Analyze Practical ROI:** Measure how completed projects, LeetCode/HackerRank coding practice, and weekly prep hours translate into core competence.
4. **Expose Preparation Roadblocks:** Quantify the primary bottlenecks (lack of internships, aptitude hurdles, guidance deficit) affecting students.
5. **Decouple Compensation Drivers:** Model the relationship between core CS competency ratings and full-time offer packages (LPA).
6. **Formulate Institutional Interventions:** Deliver a structured, semester-by-semester placement enhancement framework for academia.

---

## 🔬 Dataset Overview & Survey Methodology

The empirical findings are grounded in survey responses collected from **100 engineering students** across 6 academic disciplines, encompassing **32 variables** across 26 distinct survey dimensions:

- **Demographics:** Academic Year (1st, 2nd, 3rd, 4th Year), Engineering Branch (CSE, IT, ECE, EEE, Mechanical, Civil), CGPA range.
- **Core Technical Competencies (1–5 Likert Scale):** Data Structures & Algorithms (DSA), Object-Oriented Programming (OOP), Database Management Systems (DBMS / SQL), Operating Systems (OS), Computer Networks (CN), Full-Stack Web Development, Data Science & Machine Learning.
- **Cognitive & Soft Skills (1–5 Scale):** Quantitative Aptitude & Logical Reasoning, Verbal Communication & Interview Articulation.
- **Practical Experience:** Completed technical projects (0, 1–2, 3–4, 5+), industrial internships status, coding problems solved (0–50 to >300), weekly preparation hours (1–3 hrs to >10 hrs).
- **Placement Readiness & Habits:** Self-assessed resume readiness, GitHub version control frequency, mock interview count, primary career track.
- **Recruitment Outcomes:** Placement status (Placed with Full-Time Offer, In-Process / No Offer Yet, Not Eligible), Highest compensation bracket (`<5 LPA`, `5–8 LPA`, `8–12 LPA`, `>12 LPA`).

---

## 🛠️ Technology Stack & Architecture

- **Backend & Core Engine:** [R Programming Language](https://www.r-project.org/) (v4.6.1+)
- **Interactive Web Framework:** [R Shiny](https://shiny.posit.co/) with modular architecture (`page_navbar`, reactive data pipeline)
- **UI System & Design:** [bslib](https://rstudio.github.io/bslib/) (Bootstrap 5) with custom light executive theme, modern glassmorphism/card UI, and Plus Jakarta Sans typography
- **Interactive Visualizations:** [Plotly](https://plotly.com/r/) & [ggplot2](https://ggplot2.tidyverse.org/) (responsive tooltips, scatterplots, grouped bars, polar radars)
- **Data Tables & Auditing:** [DT](https://rstudio.github.io/DT/) (HTML5 pagination, column searching, instant CSV/Excel export)
- **Data Wrangling:** [tidyverse](https://www.tidyverse.org/) (`dplyr`, `tidyr`, `readr`, `stringr`, `janitor`, `scales`)
- **Machine Learning & Data Mining:**
  - **K-Means Clustering:** Optimal 2-cluster segmentation (`cluster`, `stats`, PCA dimensional reduction via `prcomp`)
  - **Association Rule Mining:** Frequent itemset mining via Apriori algorithm (`arules`, `arulesViz`)
  - **Classification Logic:** Decision boundaries evaluated with `rpart`

---

## 📁 Repository Structure

```text
student-placement-analytics/
├── app.R                             # Main Shiny application entry point
├── install_dependencies.R            # One-click automated dependency installer
├── R/                                # Modular R Shiny architecture
│   ├── data_loader.R                 # Robust data loading, validation & factor engineering
│   ├── mod_overview.R                # Module: Executive KPI Cards & Cohort Summary
│   ├── mod_exploration.R             # Module: Interactive Data Table & Quality Audit
│   ├── mod_visualizations.R          # Module: 6-Tab Interactive Visualizations Suite
│   ├── mod_insights.R                # Module: Placed Blueprint, Mock Wakeup & Roadmap
│   ├── mod_clustering.R              # Module: K-Means Clustering & 2D PCA Projection
│   ├── mod_association_rules.R       # Module: Association Rule Mining (Apriori Engine)
│   └── mod_calculator.R              # Module: Interactive Placement Readiness Scorecard
├── www/
│   └── styles.css                    # Modern CSS (Plus Jakarta Sans, card styling, gradients)
├── 24103076 Code/                    # READ-ONLY ORIGINAL ACADEMIC SOURCE CODE & ARTIFACTS
│   ├── 01_data_exploration.R         # Original EDA & summary script
│   ├── 02_clustering_preparation.R   # Original clustering & outcome script
│   ├── 03_association_rules.R        # Original Apriori association rules script
│   ├── 04_decision_tree.R            # Original classification script
│   ├── app.R                         # Original visualization exporter
│   ├── data/
│   │   ├── student_placement.csv     # Primary survey response dataset
│   │   └── student_placement_synthetic_100.csv
│   ├── student_placement_clustered.csv
│   ├── student_placement_cluster_profiles.csv
│   ├── student_placement_association_rules.csv
│   ├── top_association_rules.csv
│   └── upload_photos/                # Original research visualization outputs
├── 24103076 Student Placement Insights.pptx # Original presentation deck
├── 24103076 std placement report.pdf         # Complete 24-page LaTeX research insights report
├── .gitignore                        # Git exclusion configuration
└── README.md                         # Project documentation and portfolio showcase
```

---

## 📊 Deep-Dive Analytical Findings

### 1. The True Differentiator: Technical Skills vs. Soft Skills
While popular advice focuses on communication and resume polishing, the data proves that **placed students outperform unplaced peers by +53.3% in DSA (3.94 vs 2.57)** and **+52.1% in OOP (4.00 vs 2.63)**. Conversely, communication skills show a negligible difference (**3.35 vs 3.13**). Soft skills prevent early disqualification, but technical mastery secures the offer.

![Placement Gap](24103076%20Code/Slide_2_Placement_Gap.png)

### 2. Project ROI: "Do Builders Make Better Coders?"
The analysis confirms a strong positive correlation between hands-on project creation and algorithmic competence:
- Students with **0 projects** exhibit a median DSA rating of **1.5**.
- Students with **1–2 projects** achieve a median rating of **2.5**.
- Students with **3–4 projects** reach a median rating of **3.0**.
- Students with **5 or more projects** achieve top-quartile DSA ratings (**4.0–5.0**).

![Projects ROI](24103076%20Code/Slide_3_Projects_ROI.png)

### 3. The Salary Staircase: Monotonic Compensation Progression
Breaking the **12+ LPA salary ceiling demands an average DSA rating of 4.33+**, accompanied by balanced systems competencies (OS & DBMS ratings of 4.10+). Offers between 5–8 LPA require a baseline DSA rating of 3.50.

![Salary Drivers](24103076%20Code/Slide_5_Salary_Drivers.png)

### 4. Primary Preparation Bottlenecks
- **Experience Void:** 18 students cited lack of internships as their single greatest roadblock.
- **Aptitude Hurdles:** 16 students were eliminated during quantitative/reasoning screening rounds.
- **Mentorship Gap:** 13 students reported lack of time and clear guidance on industry expectations.

![Bottlenecks](24103076%20Code/Slide_4_Bottlenecks.png)

### 5. Machine Learning Segmentation: K-Means & PCA (K = 2)
Using the Elbow Method on within-cluster sum of squares (WCSS), students naturally divide into two distinct operational segments:
- **Cluster 1 — Foundational Learners (52%):** Average DSA: 2.40, DBMS: 2.40, Prep: 5.4 hrs/wk, Average CGPA: 7.63.
- **Cluster 2 — High-Capability Technologists (48%):** Average DSA: 3.25, DBMS: 3.31, Prep: 6.8 hrs/wk, Average CGPA: 8.09.

---

## 🚀 How to Run the Dashboard Locally

### Prerequisites
Make sure you have [R](https://cloud.r-project.org/) (version 4.2 or higher) installed on your system.

### Option 1: Quick Automated Launch (Terminal)

```bash
# 1. Clone the repository
git clone https://github.com/your-username/student-placement-analytics.git
cd student-placement-analytics

# 2. Install all required R dependencies (one-time setup)
Rscript install_dependencies.R

# 3. Launch the Shiny application
Rscript -e "shiny::runApp('.')"
```

The application will start immediately and can be accessed at `http://127.0.0.1:3838` (or your assigned local port).

### Option 2: Run within RStudio IDE

1. Open `RStudio`.
2. Open the project root directory or `app.R`.
3. Click the **Run App** button in the top-right corner of the editor window.

---

## 📱 Dashboard Feature Walkthrough

| Tab / View | Key Features & Analytical Capabilities |
| :--- | :--- |
| **1. Executive Overview** | High-impact KPI cards, demographic distributions (Year & Branch), placement outcome rates, and executive takeaways. |
| **2. Data Explorer** | Dynamic multi-criteria filters (Year, Branch, Outcome, Projects), interactive searchable `DT` table, column metadata dictionary, and complete missingness/duplicate audit. |
| **3. Visualizations** | 6 specialized interactive tabs: Core Competency Baseline, Placement Gap, Projects & Coding ROI, Salary Staircase, Primary Bottlenecks, and Company Priorities vs Student Focus. |
| **4. Deep-Dive Insights** | Detailed Placed Student Blueprint, the "Mock Wakeup" analysis, Resume Disconnect audit, and Departmental Competency Heatmap. |
| **5. ML Clustering** | 2D Principal Component Analysis (PCA) projection with interactive tooltips, cluster profile comparisons, and WCSS Elbow curve analysis. |
| **6. Association Rules** | Apriori association rules catalog with interactive Support vs. Confidence scatter plot, minimum Lift filters, and key pattern interpretations. |
| **7. Readiness Scorecard** | Real-time interactive calculator: input your CGPA, DSA, OOP, projects, coding volume, and mock sessions to receive a placement readiness index (0–100), estimated salary bracket, and customized gap remediation plan. |

---

## 🏛️ Institutional Strategic Roadmap (NIT Jalandhar)

Derived directly from the analytical conclusions, the following structured action plan is proposed for academic institutions:

1. **Intensive Algorithmic Training (3–4 Sessions/Week):** Elevate median cohort DSA rating from 2.80 to 3.80+ to meet product company hiring thresholds.
2. **Core Systems Labs (Weekly):** Deepen conceptual foundations in Operating Systems, DBMS/SQL, and Computer Networks.
3. **Aptitude Acceleration (2–3 Sessions/Week):** Mitigate the second largest hurdle (16 students blocked at preliminary rounds) through timed tests.
4. **Mandatory Project Milestones (Every Semester):** Require 2–3 deployed technical projects with verifiable GitHub histories.
5. **Bi-Monthly Mock Interviews:** Puncture false readiness overconfidence early and provide actionable rubric-based feedback.

---

## 👥 Authors & Academic Credits

This study and analytics dashboard were developed as part of the academic curriculum at:

**Department of Computer Science & Engineering**  
**Dr. B. R. Ambedkar National Institute of Technology (NIT), Jalandhar**  
*G.T. Road, Amritsar Bypass, Jalandhar, Punjab 144008, India*

- **Kavi Prakash Ranjan** — *Roll No. 24103076*, Department of CSE
- **Kartik Saini** — *Roll No. 24103075*, Department of CSE
- **Faculty Supervisor:** **Dr. Jagdeep Kaur**, *Department of CSE, NIT Jalandhar*
- **Academic Year:** 2026–2027

---

## 📄 License

This project is licensed under the **MIT License** — feel free to explore, adapt, and build upon this research with proper academic attribution.
