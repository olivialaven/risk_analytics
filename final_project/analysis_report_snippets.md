# Energy Demand & Temperature Extremes: Analysis Report Snippets

Generated: 2025-10-28 17:04:49 


## 1. Data Description

This analysis examines ** 43361 hourly observations** of electricity demand (American Electric Power - AEP) and regional temperature (Midwest US average: Chicago, Detroit, Indianapolis, Pittsburgh) from ** 2012-10-01 13:00:00 ** to ** 2017-11-29 23:00:00 ** (approximately 5 years).  AEP serves a large region across the Midwest and Mid-Atlantic US, making this dataset ideal for studying temperature-driven demand extremes.

**Key Statistics:**

- Mean demand: ** 14920  MW** (SD =  2501  MW)
- Peak demand: ** 24739  MW**
- Temperature range: ** -25.2 °C to  34.1 °C**


## 2. Exploratory Data Analysis

**Distribution Analysis:** The Q-Q plots reveal that electricity demand exhibits **heavy tails**,  with extreme values occurring more frequently than predicted by a normal distribution.  The **t-distribution** provides a better fit, confirming the presence of extreme events.

**Temperature-Demand Relationship:** A clear **U-shaped relationship** emerges (Figure 07),  showing increased demand at both temperature extremes due to heating (cold) and cooling (hot) needs.  Peak demand occurs during summer months (cooling) and winter cold snaps (heating).

