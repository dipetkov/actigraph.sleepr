#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Rolling window statistics over complete windows of width `n`.
// These replace `RcppRoll::roll_sum()`, `roll_mean()` and `roll_sd()` with
// `partial = FALSE` and no `fill`, so the result has length
// `length(x) - n + 1` and alignment does not matter. The sample standard
// deviation uses the `n - 1` denominator, as in RcppRoll.

// [[Rcpp::export]]
NumericVector roll_sum_(NumericVector x, int n) {
  int len = x.size();
  if (n < 1) stop("`n` must be positive.");
  int out_len = len - n + 1;
  if (out_len < 0) out_len = 0;
  NumericVector out(out_len);
  for (int i = 0; i < out_len; i++) {
    double sum = 0.0;
    for (int j = 0; j < n; j++) sum += x[i + j];
    out[i] = sum;
  }
  return out;
}

// [[Rcpp::export]]
NumericVector roll_mean_(NumericVector x, int n) {
  NumericVector out = roll_sum_(x, n);
  for (int i = 0; i < out.size(); i++) out[i] = out[i] / n;
  return out;
}

// [[Rcpp::export]]
NumericVector roll_sd_(NumericVector x, int n) {
  int len = x.size();
  if (n < 1) stop("`n` must be positive.");
  int out_len = len - n + 1;
  if (out_len < 0) out_len = 0;
  NumericVector out(out_len);
  for (int i = 0; i < out_len; i++) {
    bool has_na = false;
    double total = 0.0;
    for (int j = 0; j < n; j++) {
      double value = x[i + j];
      if (ISNAN(value)) has_na = true;
      total += value;
    }
    if (has_na || n < 2) {
      out[i] = NA_REAL;
      continue;
    }
    double mean = total / n;
    // Corrected two-pass algorithm: `residual` collects the rounding error
    // in `mean`, and subtracting it restores precision when the spread is
    // small relative to the mean (as in RcppRoll >= 0.4.0).
    double squares = 0.0;
    double residual = 0.0;
    for (int j = 0; j < n; j++) {
      double d = x[i + j] - mean;
      squares += d * d;
      residual += d;
    }
    if (squares == R_PosInf) {
      out[i] = R_PosInf;
      continue;
    }
    double var = squares - residual * residual / n;
    if (var < 0.0) var = 0.0;
    out[i] = std::sqrt(var / (n - 1));
  }
  return out;
}
