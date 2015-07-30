# Kaggle: Don't Get Kicked!

**Team: Shiyi Chen, Steven Lin, Shawn Li, Ameer Khan, Sanjeevni Wanchoo**

* Predicted if a Car Purchased at an Auction is a Bad Buy

## Abstract

Auto community calls an unfortunate purchase a "kick" if an auto dealership purchases a used car with serious issues at an auction that prevents it from being sold to customers. In this report, 6 models were used to predict which cars had a high risk of being “kicked”. These models included logistic regression, decision tree, neural network, random forest, gradient tree boosting, and an ensemble method. Different criteria for model evaluation, such as misclassification rate and the Gini impurity index, were used.

External demographic data was also leveraged to improve the predictive power of the models. However, the vehicle purchase data played a more important role than the demographic information in predicting outcomes. The WheelType variable was the most critical predictor across different models that determined if a vehicle was a bad buy. The results also indicate misclassification errors of around 10% and Gini indices around 0.20. Based on the Gini index, Gradient Tree Boosting and Ensemble Model (Maximal Voting) were found to be the best models in predicting the probability of a car being a bad buy with Gradient Tree Boosting giving a misclassification rate of 10.11% and Gini index of 0.2357.