library( corrplot );
library( caret );
library( gbm );

setwd( "/sscc/home/a/amk202/Predictive" );

gini = function ( actual, predicted ) {
  n = length( actual );
  gini_data = as.data.frame( cbind( actual, predicted, row = 1:n ) );
  gini_data = gini_data[ with( gini_data, order( -predicted, row ) ), ];
  sum( cumsum( gini_data[ , 1 ] ) / sum( gini_data[ , 1 ] ) - ( 1:n ) / n ) / n;
}

car_data = read.csv( "car_data_clean.csv", header = T );
n = nrow( car_data );

var_pred = c( 3:6, 10:11, 13:18, 27, 30:32 );

# Bagging

sampleRows = sample( nrow( car_data ), 0.5 * n );
sample_data = car_data[ sampleRows, ];

startTime = proc.time( );
control = bagControl( fit = ctreeBag$fit, predict = ctreeBag$pred, aggregate = ctreeBag$aggregate, downSample = T  );
fit_bag = bag( x = car_data[ sampleRows, var_pred ], y = car_data[ sampleRows, 1 ], B =  10,
           bagControl = control );
proc.time( ) - startTime;

summary( fit_bag );

phat = predict( fit_bag, car_data[ -sampleRows, var_pred ], type = "response" );

# yhat = apply( phat, 1, function( x ) names( which.max( x ) ) );
# yhat = factor( yhat );
yhat = factor( as.numeric( phat > 0.5 ) );

MCE = sum( yhat != car_data[ -sampleRows, "IsBadBuy" ] ) / nrow( car_data[ -sampleRows, ] );

table( car_data[ -sampleRows, "IsBadBuy" ], yhat );

gini( as.numeric( car_data[ -sampleRows, "IsBadBuy" ] ), phat );

# Boosted Trees

startTime = proc.time( );
fit_gbm = gbm( IsBadBuy ~ ., data = car_data[ , c( 1, var_pred ) ], distribution = "bernoulli", n.trees = 2500,
               shrinkage = 0.02, interaction.depth = 2, bag.fraction = 0.5, train.fraction = 1,
               n.minobsinnode = 10, cv.folds = 10, keep.data = T );
proc.time( ) - startTime;

summary( fit_gbm );

bestIter = gbm.perf( fit_gbm, method = "cv" );
summary( fit_gbm, n.trees = bestIter );

print( pretty.gbm.tree( fit_gbm, bestIter ) );

phat = predict( fit_gbm, car_data, n.trees = bestIter, type = "response" );
yhat = factor( as.numeric( phat > 0.5 ) );

sum( yhat != car_data[ , "IsBadBuy" ] ) / n;
table( car_data[ , "IsBadBuy" ], yhat );

gini( as.numeric( car_data[ , "IsBadBuy" ] ), phat );
