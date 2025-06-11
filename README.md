# 🎮 Turtle Games Analytics Case Study

**Author:** Phil Best  
**Tools Used:** Python, R, pandas, scikit-learn, matplotlib, seaborn, TextBlob, VADER, Tableau

## 🎯 Objective

Understand trends in customer loyalty points and review data to inform Turtle Games' customer strategy. The goal was to uncover insights and develop predictive models that could help improve marketing effectiveness and customer engagement.

---

## 🤨 Key Questions Explored

- What drives customer loyalty points?
- Can we predict high-value customers based on early behaviour?
- How do product reviews relate to customer sentiment and brand perception?
- Are there distinct customer segments with different loyalty profiles?

---

## 🧐 Analytical Methods

### 🔹 Data Cleaning & Preparation
- Removed irrelevant columns, handled missing values
- Standardized formats and encoded categorical variables

### 🔹 Regression & Prediction
- Built multiple linear regression models (OLS and in R)
- Identified pay and spending score as top predictors of loyalty
- Handled skew and outliers to improve model performance

### 🔹 Decision Tree Modeling
- Built regression trees to predict loyalty points
- Visualised tree depth and feature importance
- Removed outliers to boost model accuracy

### 🔹 Clustering (K-Means)
- Segmented customers into 5 clear groups based on pay and spending score
- Used Elbow and Silhouette methods to determine optimal cluster count
- Interpreted segments to inform marketing strategy

### 🔹 NLP & Sentiment Analysis
- Cleaned and tokenized customer reviews
- Compared TextBlob and VADER sentiment scoring
- Found TextBlob provided more balanced results
- Identified top- and bottom-rated products by sentiment

---

## 💡 Key Insights

- **Pay and Spending Score** together explain over 80% of variation in loyalty points
- **5 distinct customer segments** emerged from clustering, supporting targeted marketing
- **Top-rated products** can be promoted to drive higher satisfaction and loyalty
- **Loyalty points distribution is highly skewed**—modeling should adjust for outliers
- **Sentiment analysis** confirms no product received negative average sentiment

---

## 📈 Strategic Recommendations

- Focus marketing on retaining High Pay / High Spend customers
- Upsell Low Spend / High Pay customers to unlock growth
- Use predictive models to identify loyalty potential early in customer lifecycle
- Align loyalty point strategy with product preferences and customer sentiment

---

## 📂 Files in This Repository

- `turtle_games_analysis.ipynb` — Python notebook (EDA, modeling, clustering, NLP)
- `turtle_reviews_sentiment.Rmd` — R notebook (regression modeling and visualizations)
- `presentation_deck.pdf` — Summary of findings for stakeholders
- `loyalty_points_model.csv` — Cleaned dataset used in final models
- `README.md` — You are here!

---

## 🧭 Future Questions

- How is spending score calculated and when?
- Do loyalty points translate into actual customer spend?
- What are the business goals of the loyalty scheme?
- How are loyalty points redeemed, and do they drive repeat purchases?

---

🔗 Let me know if you’d like to walk through this case study or discuss how the analysis could be adapted for similar commercial problems.
