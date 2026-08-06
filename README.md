# 🚇 Metro Dispatch Optimization

## 📌 Project Overview

This project was developed as part of an academic optimization assignment. The objective was to determine an efficient metro dispatch schedule based on expected passenger demand while considering operational constraints.

The project uses **Linear Programming (LP)** to optimize metro operations and analyzes passenger demand patterns throughout the day using R.

---

## 🎯 Objective

The main goal of this project is to:

- Analyze hourly passenger demand.
- Optimize metro dispatch using Linear Programming.
- Reduce unnecessary operational cost.
- Improve passenger service by matching train frequency with demand.

---

## 🛠️ Technologies Used

- R Programming
- lpSolve
- ggplot2
- dplyr
- readxl

---

## 📂 Project Structure

```
Metro-Dispatch-Optimization/
│
├── optimisation.R
├── Passenger_Footfall_Data.xlsx
├── Project_Report.pdf
├── Metro_Optimization_Presentation.html
├── README.md
├── requirements.txt
├── .gitignore
│
└── plots/
    ├── hourly_demand.png
    └── demand_distribution.png
```

---

## 📊 Dataset

The dataset contains expected hourly passenger footfall for different metro lines.

It is used to estimate passenger demand and formulate the optimization problem.

---

## 📈 Project Output

The project generates:

- Hourly passenger demand analysis
- Demand distribution visualization
- Optimal metro dispatch schedule
- Cost-efficient resource allocation

---

## 📷 Visualizations

### Hourly Passenger Demand

![Hourly Demand](plots/hourly_demand.png)

---

### Demand Distribution

![Demand Distribution](plots/demand_distribution.png)

---

## ▶️ How to Run

1. Open the project in **RStudio**.
2. Install the required packages.

```r
install.packages(c("lpSolve","ggplot2","dplyr","readxl"))
```

3. Open `optimisation.R`.

4. Run the script.

---

## 📄 Files Included

- **optimisation.R** → Main R script
- **Passenger_Footfall_Data.xlsx** → Input dataset
- **Project_Report.pdf** → Detailed project report
- **Metro_Optimization_Presentation.html** → Project presentation

---

## 📚 What I Learned

Through this project, I learned:

- Linear Programming
- Optimization modelling
- Data visualization in R
- Reading and processing Excel datasets
- Solving optimization problems using `lpSolve`

---

## 🔮 Future Improvements

Some possible future improvements are:

- Real-time passenger demand analysis
- Multiple metro route optimization
- Interactive dashboard using Shiny
- Automatic report generation

---

## 👨‍💻 Author

**Saurav Kumar**

BSDS Student

Indian Statistical Institute, Bangalore
