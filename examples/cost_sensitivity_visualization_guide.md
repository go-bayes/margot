# Cost Sensitivity Visualization Guide

## Overview
When treatment costs vary, QINI curves change shape, affecting optimal treatment allocation. The margot package provides several ways to visualize and interpret these changes.

## 1. Single Cost Visualization

When you run `margot_plot_qini()` with a specific treatment cost:
```r
plot <- margot_plot_qini(results, "model_anxiety", treatment_cost = 2.5)
```

The plot will show:
- **Title**: "Qini Curves for Anxiety"
- **Subtitle**: "Treatment cost = 2.5" (only shown when cost ≠ 1)
- **Curves**: CATE (targeted) vs ATE (uniform) allocation

## 2. Cost Sensitivity Overlay Plot

`margot_plot_qini_cost_sensitivity(..., plot_type = "overlay")` shows:
```
    Qini Curves Across Treatment Costs: Anxiety
         Costs evaluated: 0.2, 0.5, 1, 2, 5
    
    |  ╱─── Cost = 0.2 (steepest curve)
    | ╱╱─── Cost = 0.5
    |╱╱╱─── Cost = 1 (baseline)
    |╱╱╱╱── Cost = 2
    |╱╱╱╱╱─ Cost = 5 (flattest curve)
    |___________________
    0%                100%
    Proportion Treated
```

**Interpretation**: 
- Steeper curves (low cost) → treat more people
- Flatter curves (high cost) → treat fewer people
- Curves converge at 100% (everyone treated)

## 3. Cost Sensitivity Facet Plot

`margot_plot_qini_cost_sensitivity(..., plot_type = "facet")` shows:
```
┌─────────────┬─────────────┬─────────────┐
│ Cost = 0.2  │ Cost = 0.5  │ Cost = 1    │
│   ╱         │   ╱         │   ╱         │
│  ╱          │  ╱          │  ╱          │
│ ╱           │ ╱           │ ╱           │
├─────────────┼─────────────┼─────────────┤
│ Cost = 2    │ Cost = 5    │             │
│   ╱         │   ╱         │             │
│  ╱          │  ╱─────     │             │
│ ╱───────    │ ╱           │             │
└─────────────┴─────────────┴─────────────┘
```

**Interpretation**: 
- Each panel shows the same outcome at different costs
- Easy to see how curve shape changes with cost
- Y-axis scale may vary between panels

## 4. Cost Summary Plot

`margot_plot_qini_cost_summary()` shows:
```
    Treatment Gain vs Cost at 20% Spend Level
    
    Gain
    0.15 |●━━━━━━━━━━━━━━━━━━━━ Anxiety
         |  ●━━━━━━━━━━━━━━━━━━
    0.10 |    ●━━━━━━━━━━━━━━━━ Depression  
         |      ●━━━━━━━━━━━━━━
    0.05 |        ●━━━━━━━━━━━━
         |          ●━━━━━━━━━━
    0.00 |____________●________
         0.2  0.5  1  2  5
              Treatment Cost
```

**Interpretation**:
- Shows how treatment effectiveness decreases with cost
- Different models may have different sensitivities to cost
- Helps identify cost thresholds where treatment becomes ineffective

## 5. Interpretation Output

When running `margot_interpret_qini()` with different costs:

### Default cost (1):
```
"We computed average policy effects from prioritising individuals by CATE at 
10% and 40% spend levels. Treatment cost is set to the default value of 1."
```

### Non-default single cost:
```
"We computed average policy effects from prioritising individuals by CATE at 
10% and 40% spend levels. All models use a treatment cost of 2.5."
```

### Variable costs:
```
"We computed average policy effects from prioritising individuals by CATE at 
10% and 40% spend levels. Treatment costs vary across models, affecting the 
optimal allocation strategy. Higher costs result in more selective targeting, 
while lower costs enable broader treatment."
```

### Model-specific notes:
```
"At 10% spend: CATE prioritisation is beneficial (diff: 0.08 [95% CI 0.02, 0.14]). 
At 40% spend: CATE prioritisation is beneficial (diff: 0.12 [95% CI 0.05, 0.19]). 
(Treatment cost = 2.5)"
```

## Key Takeaways

1. **Cost affects curve shape**: Lower cost → steeper curve → more benefit from targeting
2. **Cost affects optimal fraction**: Higher cost → treat fewer people optimally
3. **Cost threshold exists**: Above certain cost, targeting provides no benefit
4. **Models differ in sensitivity**: Some outcomes remain worth targeting at higher costs than others
5. **Budget constraints matter**: Real-world budget determines which cost scenario to use