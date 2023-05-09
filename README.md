# Marketing Mix Model: Budget Allocation
Determine the effectiveness of advertising activities on sales 

### Overview

The objective is to develop appropriate models to help a cosmetics company assess the individual and the joint effectiveness of the different advertising channels on sales and determine the optimal allocation of its advertising budget.

### Focal Model (By Firm)

Firm determined a model (focal model) based on the diminishing effects of advertisement on sales as well as the impact of past sales on current sales. 
The focal model carries a square root functional form for its 6 independent variables which were chosen based on whether the variables were contributing in explaining the variation we see in sales. Excluded aggregated variables: “ADV_Total”, “ADV_Offline”, and “ADV_online” because of multicollinearity. Excluded variables “Social Media” and “Banner” because most of the data reported zero spent on these channels which would skew the model once added. Lastly, excluded the variable “Maillings” as it had a very small correlation with sales and didn’t contribute to explaining any variation in our model. **The focal model explained 38% of the variation in sales resulting in portals and the shopping catalogs sent to win-back customers as both significant variables.**

### Recommended Model

In order to produce a model that best fits our data, I ploted the advertising variables with the sales variable and determined the functional forms based on the plot. Adopted the quadratic functional form for all variables except for the “Portal” variable as it showed almost a linear relationship to sales. Additionally, added 4 major interaction effects that were crucial in improving the fit of the model and decreasing the variation. **The recommended model explained 80% of variation in the data, reporting the lowest AIC at 629.** Moreover,  included the intercept in the model a it represents the sales in the case of when advertising spend is zero.

### Price Elasticity

Based on the best model, calculated the elasticity and **recommended budget allocation including the synergy**. The firm should (increase/decrease budget in): 
- decrease catalog existing customer by 92%, 
- increase catalog win back by 35%, 
- decrease catalog new customer by 23%, 
- increase newsletter by 117% and 
- increase portals by 63%.
