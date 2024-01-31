#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List SimpLinCpp(NumericVector x, NumericVector y) {
  int n = x.size();
  double x_sum = sum(x);
  double x_mean = mean(x);
  double y_sum = sum(y);
  double xy_sum = sum(x * y);
  double xx_sum = sum(x * x);
  double x_mean_sum = xx_sum - n * x_mean * x_mean;
  
  // Compute estimated coefficients
  double beta1 = (n * xy_sum - x_sum * y_sum) / (n * xx_sum - x_sum * x_sum);
  double beta0 = (y_sum - beta1 * x_sum) / n;
  
  // Compute residuals and predicted values
  NumericVector residuals = y - (beta0 + beta1 * x);
  NumericVector predicted = beta0 + beta1 * x;
  
  // Compute the variance and standard errors
  double sse = sum(residuals * residuals); //sum of squared residuals
  double s2 = sse / (n - 2);
  double var_beta1 = s2 / (xx_sum - n * x_mean * x_mean);
  double var_beta0 = s2 * (1 / n + x_mean * x_mean / (xx_sum - n * x_mean * x_mean));
  
  double se_beta1 = sqrt(var_beta1);
  double se_beta0 = sqrt(var_beta0);
  
  // Compute the 95% confidence intervals
  double z = R::qt(0.975, n-2, true, false); // Approximate value for 95% confidence
  double ci_beta1_low = beta1 - z * se_beta1;
  double ci_beta1_high = beta1 + z * se_beta1;
  double ci_beta0_low = beta0 - z * se_beta0;
  double ci_beta0_high = beta0 + z * se_beta0;
  
  return List::create(Named("beta0") = beta0, Named("beta1") = beta1,
                      Named("se_beta0") = se_beta0, Named("se_beta1") = se_beta1,
                      Named("ci_beta0_low") = ci_beta0_low, Named("ci_beta0_high") = ci_beta0_high,
                      Named("ci_beta1_low") = ci_beta1_low, Named("ci_beta1_high") = ci_beta1_high,
                      Named("residuals") = residuals, Named("predicted") = predicted);
}