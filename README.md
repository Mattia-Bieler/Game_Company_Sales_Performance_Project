# Game Company Sales Performance Project
## Project Background 
This project will explore Turtle Games’ need to enhance its sales performance, which is their business problem, through utilising customer trends and provide recommendations. In essence, Turtle Games’ goal is to tailor their products, marketing approach, and sales strategies through clear data-driven decisions using the patterns, preferences, and behaviours of their customer base. The root cause of this business problem is that Turtle Games lacks a concrete data-driven decision-making culture that is central to their ethos, leading to inefficient sales performance. Therefore, this report will analyse and interpret the data, answering various questions, to create a foundation for data-driven decisions.  

## Analytical Approach 
After loading the turtle_reviews.csv and turtle_sales.csv files into Python and R respectively, I checked for __missing values and duplicates__, which did not exist. Furthermore, I checked the __data types and descriptive statistics__ to better understand the datasets. In Python, I used the __dtypes, info, and describe functions__. In R, I used the __sapply, summary, describe, and DataExplorer functions__. I also removed redundant columns from both datasets and simplified the column names ‘remuneration (k£)’ and ‘spending_score (1-100)’ from the turtle reviews dataset to ‘remuneration’ and ‘spending_score’. This preparation made it easier to work with the datasets.

To achieve Turtle Games’ objectives, I used Python libraries, such as __statsmodels, scikit-learn, and nltk__, for machine learning models, statistical tests, and natural language processing. In R, __tidyverse__ is an essential collection of packages, allowing for data manipulation and visualisation. The use of the __plotly package__ made the visualisations interactive, providing more depth to the graphs. Finally, the __caret package__ allowed me to train and evaluate the machine learning models.

![age_vs_loyalty](https://github.com/Mattia-Bieler/Game_Company_Sales_Performance_Project/assets/132078605/a5c65fb3-5ed5-4da5-aaa7-2522e4b29a95)

![remuneration_vs_loyalty](https://github.com/Mattia-Bieler/Game_Company_Sales_Performance_Project/assets/132078605/522d412a-3b39-45b2-8e83-309bc437c6ff)

![spending_vs_loyalty](https://github.com/Mattia-Bieler/Game_Company_Sales_Performance_Project/assets/132078605/a2a35cf2-3343-4c52-972c-193512e239de)

To access how customers accumulate loyalty points, I initially created three simple linear regression models, using age, remuneration, and spending score separately as the independent variables. Due to all three models having R-squared values below 0.5, I created a multiple linear regression (MLR) model, which once trained, had an R-squared value of 0.85. However, significant heteroscedasticity is present in the model, impacting the coefficients, and the moderately high mean squared error (MSE) value suggests that some substantial errors occur. This illustrates that improvements to the model are needed to improve its accuracy.

![elbow_method_plot](https://github.com/Mattia-Bieler/Game_Company_Sales_Performance_Project/assets/132078605/f9781233-1539-4179-bf1c-1ae528ab0513)

![silhouette_method_plot](https://github.com/Mattia-Bieler/Game_Company_Sales_Performance_Project/assets/132078605/b4ab0b43-07b4-4fc3-988b-09836e8cd121)

The elbow and silhouette methods, as well as testing various k-values as part of the k-means clustering process, guided me to conclude that five customer groups exist within Turtle Games’ customer base. For the sentiment analysis, I made the texts lower case and removed punctuation. Additionally, I made two executive decisions. First, to remove duplicates from both columns separately rather than together, as the sentiment analysis will be done on each column individually. Second, to keep stopwords because they provide added context. Regardless of my decisions, after testing all options, little changes to the overall sentiment of each column. 

An important initial step taken in R was to use the group_by function on the product column, as Turtle Games is specifically looking at the impact of sales per product. Therefore, using the group_by function simplifies the dataset and ensures that a product is not represented more than once on a graph.

## Visualisations and Insights
The rationale behind the chosen graphs was to provide detailed visualisations that are easy to follow for all types of stakeholders. A scatterplot was used to visualise the various customer groups within the customer base due to their ability to present clusters. As the variables are spending score (y-axis) and remuneration (x-axis), the scatterplot illustrates the behaviour of these groups. This shows the usefulness of the spending score and remuneration data. The cluster with the largest number of customers is ‘Middle Earners Middle Spenders’ (774). Whereas the cluster with the smallest number of customers is ‘Low Earners High Spenders’ (269), which also has the lowest average age and the third highest average loyalty points.

![k5_scatterplot](https://github.com/Mattia-Bieler/Game_Company_Sales_Performance_Project/assets/132078605/e7e307cb-2eb5-42ae-81ed-acc88e33f46e)

__Cluster 0 (Middle Earners Middle Spenders):__ Cluster 0 has the largest number of customers (774). These types of customers might go through a thinking process before buying a product, as they will consider whether they can afford the item and whether they need the item straightaway. Discounts and the possibility to place items on hold could be attractive to these types of customers.

__Cluster 1 (High Earners Low Spenders):__ Cluster 1 has the third largest number of customers (330). Although these customers have a high income, they spend more conservatively. These types of customers might appreciate long-term benefits. A system that links together loyalty points with better discounts could get these customers to spend more, as higher discount offers are very attractive to these types of customers.

__Cluster 2 (High Earners High Spenders):__ Cluster 2 has the second highest number of customers (356). These 'high-value' customers might care less about discounts than other types of customers. Instead, early access to items could attract these customers and is also a possibility for improving sales, as an added charge can be included for early access.

__Cluster 3 (Low Earners Low Spenders):__ Cluster 3 has the fourth highest number of customers (271). These customers will most likely search for discounts when it comes to books, games, and toys. To target this group of customers, marketing efforts should focus on discounts, budget-friendly options, and economic bundles.

__Cluster 4 (Low Earners High Spenders):__ Cluster 4 has the smallest number of customers (269). These types of customers are dedicated supporters of the franchises of books, games, and toys they buy, as they are willing to spend a significant amount of their income on these items. Providing these types of customers with the possibility of flexible payment options, early access, and sale events will most likely be popular.

![NA_EU_Global](https://github.com/Mattia-Bieler/Game_Company_Sales_Performance_Project/assets/132078605/2b30149d-c70b-4ceb-8a5d-53aa4c94fd7b)

To understand the turtle sales dataset, access the impact of sales per product, and any possible relationship between the sales columns, I used barplots, boxplots, histograms, and scatterplots. For example, I used a scatterplot to compare the sales columns per product with NA_Sales as the y-axis, EU_Sales as the x-axis, and Global_Sales as the hue. Strongly contrasting colours, yellow and black with a clear brownish middle colour, allowed for the different values of global sales to be presented. Furthermore, a clear purple line of best fit was also used. Although three distinct outliers exist in the scatterplot, the general trend, which is also supported by a MLR model, is that North American and European sales have a strong relationship in predicting global sales.

![BH_ALL](https://github.com/Mattia-Bieler/Game_Company_Sales_Performance_Project/assets/132078605/616aa15c-4a83-4203-a135-0336f5eabc5b)

The use of boxplots and histograms for each regional sales column aided in identifying the distribution of data and understanding the nature of the outliers. These are upper outliers, highlighting the major success of a few key products. To make it clear which column the boxplots and histograms represented, a colour theme was kept throughout the Rscript (red for NA_Sales, blue for EU_Sales, and green for Global_Sales). From interpreting the boxplots and histograms, as well as the use of Q-Q plots and the Shapiro-Wilk test, the turtle sales dataset does not have a normal distribution.

Histograms were also used to understand the distribution of sentiment for the review and summary columns. The describe function helped understand this distribution. Overall, both columns have a slightly positive sentiment. However, due to the mean score being slightly greater than the median score, notably negative reviews and summaries are present.

## Recommendations
Regarding the sentiment analysis, the usefulness of social data for marketing campaigns is mainly assessed by looking at the texts themselves rather than analysing the overall sentiment and the distribution of sentiment. I focused on the negative reviews, as the reviews contain more detail that can guide Turtle Games to improve its sales performance. Looking through the negative reviews, some customers have an issue with the difficulty or entertainment level of the product, while others criticise the product itself for missing pieces or its poor quality. Therefore, Turtle Games should focus on good quality games with an improved guidance for age range, as well as more prior market research and testing to ensure the product is entertaining. This would improve customer opinions and therefore increase sales. However, it should be noted that the sentiment analysis is not perfect, as some texts are assigned a sentiment score that does not match the text’s actual sentiment. Therefore, I would suggest a further analysis on the review and summary columns. 

For the different types of customers, the overall approach the marketing team should take is to promote discounts based on loyalty points, hold sales events, and provide budget-friendly options. The group least concerned with these options would be ‘High Earners High Spenders’, who would potentially consider paid-early-access to products. ‘Low Earners High Spenders’ might like this option as well but due to their low income, would benefit more from flexible payment options. ‘Middle Earners Middle Spenders’ and ‘Low Earners High Spenders’ would also appreciate the option to place items on hold, potentially boosting sales, as they would genuinely consider buying the product rather than instantly rejecting the product due to a substantial price tag. Overall, a holistic approach needs to be taken to attract all types of customers.  

[__CLICK HERE TO WATCH PRESENTATION__](https://drive.google.com/file/d/1bD7nU52A5NolxRF7My8-AC1vI-R8A6LA/view?usp=sharing)
![TGP](https://github.com/Mattia-Bieler/Game_Company_Sales_Performance_Project/assets/132078605/120bbec0-68cc-4d38-a66c-e82c7ed238a3)
