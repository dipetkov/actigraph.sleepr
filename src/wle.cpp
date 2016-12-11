#include <Rcpp.h>
using namespace Rcpp;

// For each timestamp x[i], find the length of the longest non-wear period
// to start at x[i]
// [[Rcpp::export]]
IntegerVector wle(NumericVector counts,
                  int activity_threshold,
                  int spike_tolerance,
                  int spike_stoplevel) {

  int n_epochs = counts.size();
  IntegerVector lengths(n_epochs);
  // Keep track of the current run of zeros in zeros[0] and
  // the next `spike_tolerance` runs of zeros which follow a spike.
  // For example, let counts = c(0, 1, 1, 0) and spike_tol = 2.
  // Start with zeros = c(0, 0, 0). Reading right to left,
  // c(0, 0, 0) -> c(1, 0, 0) -> c(0, 1, 0) -> c(0, 0, 1) -> (1, 0, 1)
  IntegerVector zeros(spike_tolerance + 1);
  for (int j = 0; j < spike_tolerance + 1; j++) {
    zeros[j] = 0;
  }
  for (int i = n_epochs - 1; i >= 0; i--) {

    if (counts[i] <= activity_threshold) {

      ++zeros[0];
      // Don't end the period with a nonzero epoch
      int len = zeros[0];
      bool add = false;
      for (int j = spike_tolerance; j > 0; j--) {
        if (zeros[j] > 0) { add = true; }
        if (add) len += zeros[j] + 1;
      }
      lengths[i] = len;

    } else {

      lengths[i] = 0;

      if (counts[i] > spike_stoplevel) {

        for (int j = 0; j < spike_tolerance + 1; j++) {
          zeros[j] = 0;
        }

      } else {

        for (int j = spike_tolerance; j > 0; j--) {
          zeros[j] = zeros[j - 1];
        }
        zeros[0] = 0;
      }
    }
  }
  return(lengths);
}
