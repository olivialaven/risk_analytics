# Extremogram Fix Summary

## Issue
Section 2(c) extremograms were not generating output figures due to incorrect function calls.

## Root Causes
1. **Missing `type` parameter**: The `extremogram1()` function requires a `type` parameter (not optional)
2. **Incorrect `extremogram2()` usage**: The cross-extremogram function requires a matrix input (using `cbind`), not two separate vectors
3. **Missing `ploting` parameter**: Should set `ploting = 0` to suppress automatic plotting
4. **Length mismatch**: Cross-extremogram returns different length than univariate extremograms

## Solutions Applied

### 1. Fixed extremogram1 calls
```r
# BEFORE (missing type parameter):
precip_extremogram <- extremogram1(df$precip, quant = 0.95, maxlag = 10)

# AFTER (with type=1 for upper tail):
precip_extremogram <- extremogram1(df$precip, quant = 0.95, maxlag = 10, type = 1, ploting = 0)
```

### 2. Fixed extremogram2 call
```r
# BEFORE (incorrect - two separate vectors):
cross_extremogram <- extremogram2(df$precip, df$discharge, 
                                 quant1 = 0.95, quant2 = 0.95, maxlag = 10)

# AFTER (correct - combined matrix):
data_matrix <- cbind(df$precip, df$discharge)
cross_extremogram <- extremogram2(data_matrix, 
                                 quant1 = 0.95, quant2 = 0.95, maxlag = 10, type = 1, ploting = 0)
```

### 3. Fixed data frame creation
```r
# BEFORE (assumed all same length):
extrem_data <- data.frame(
  lag = 0:10,
  precip_auto = precip_extremogram,
  discharge_auto = discharge_extremogram,
  cross = cross_extremogram
)

# AFTER (handle different lengths):
lag_length <- min(length(precip_extremogram), length(discharge_extremogram), length(cross_extremogram))

extrem_data <- data.frame(
  lag = 0:(lag_length-1),
  precip_auto = precip_extremogram[1:lag_length],
  discharge_auto = discharge_extremogram[1:lag_length],
  cross = cross_extremogram[1:lag_length]
)
```

### 4. Improved plot aesthetics
```r
# Added:
- geom_hline for 0.05 threshold reference
- Better color scheme with manual colors
- Increased line width and point size
- Added subtitle explaining what extremograms measure
- Moved legend to top for better visibility
```

## Results

### Output Created
✅ **File**: `practical_1/figures/part2c_extremograms.png` (63.6 KB)
- Created: October 16, 2025, 15:09:47
- Dimensions: 10" × 6" at 300 DPI
- Shows three extremogram series with proper formatting

### Extremogram Values
```
Lag  Precip  Discharge  Cross
0    1.0000  1.0000     0.0384
1    0.0921  0.5064     0.0665
2    0.0332  0.2916     0.2583
3    0.0665  0.2225     0.2327
4    0.0563  0.1790     0.1279
5    0.0486  0.1739     0.1125
6    0.0435  0.1509     0.0895
7    0.0409  0.1176     0.0767
8    0.0563  0.1023     0.0639
9    0.0460  0.0844     0.0537
```

### Key Findings
- **Discharge extremes cluster strongly**: 9 out of 9 lags above 0.05 threshold
- **Precipitation extremes cluster weakly**: 4 out of 9 lags above 0.05 threshold  
- **Cross-extremogram shows strong dependence**: Peak at lag 2 (0.2583), indicating that extreme precipitation leads to extreme discharge 2 days later
- **Temporal persistence**: Discharge extremes persist much longer than precipitation extremes

## Files Modified
1. ✅ `Practical1_solutions.R` - Lines 350-405 (extremogram section)
2. ✅ Created `test_extremogram.R` - Standalone test script

## Verification
- ✅ Script runs without errors
- ✅ Figure file created in correct location
- ✅ Plot displays correctly with all three series
- ✅ Statistical analysis outputs correctly
- ✅ No warnings related to extremogram functions

## Technical Notes
- **Package**: extremogram v1.0.2
- **Type parameter**: type=1 means upper tail extremes (exceedances above quantile)
- **Quantile**: 0.95 (95th percentile) identifies extreme events
- **Maxlag**: 10 days of temporal dependence examined
- **Threshold for clustering**: 0.05 (5% baseline probability)

---
**Date**: October 16, 2025
**Status**: ✅ FIXED and VERIFIED
