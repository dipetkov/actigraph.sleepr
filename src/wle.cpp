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
  /*
   Keep track of the current run of zeros in run0s[0] and
   the next `spike_tolerance` runs of zeros which follow a spike.

   For example, let counts = c(0, 0, 1, 1, 0) and spike_tol = 2.

   Start with run0s = c(0, 0, 0). Processing counts right to left
   but filling in run0s left to right:
   1. (0, 0, 0) -> (1, 0, 0) : If x[i] <= activity, run0s[0]++
   2. (1, 0, 0) -> (0, 1, 0) : If x[i] > activity, shift run0s
                               one place to the right,
                               padding with 0s
   3. (0, 1, 0) -> (0, 0, 1) : Shift once more
   4. (0, 0, 1) -> (1, 0, 1) : Add 1 to the start of run0s
   5. (1, 0, 1) -> (2, 0, 1) : Increment run0s[0] again
  */
  IntegerVector run0s(spike_tolerance + 1);
  for (int j = 0; j < spike_tolerance + 1; j++) {
    run0s[j] = 0;
  }
  for (int i = n_epochs - 1; i >= 0; i--) {

    if (counts[i] <= activity_threshold) {

      ++run0s[0];

      // Each zero in run0s[1:end] corresponds to a nonzero epoch
      // that isn't followed by a run of zero epochs
      int len = run0s[0];
      bool add = false;
      for (int j = spike_tolerance; j > 0; j--) {
        // Don't end the period with a nonzero epoch
        if (run0s[j] > 0) { add = true; }
        if (add) len += run0s[j] + 1;
      }
      lengths[i] = len;

    } else {

      lengths[i] = 0;

      // Stop non-wear periods because a large spike of activity has occurred
      if (counts[i] > spike_stoplevel) {

        // Fill in run0s with 0s
        for (int j = 0; j < spike_tolerance + 1; j++) {
          run0s[j] = 0;
        }

      // There is a spike of activity but below the stop threshold, so
      // extend non-wear periods as long as the tolerance hasn't been exceeded.
      } else {

        // Shift run0s one place to the right, padding with 0
        for (int j = spike_tolerance; j > 0; j--) {
          run0s[j] = run0s[j - 1];
        }
        run0s[0] = 0;
      }
    }
  }
  return(lengths);
}
