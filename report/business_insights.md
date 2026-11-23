# Business insights

## Executive summary

Among 62,897 fulfilled orders, 36,048 were classified late: 57.31%. Approximately 18.08 million in dataset-denominated net sales is associated with late fulfilled orders. This measures commercial activity exposed to an operational issue, not a proven financial loss.

## Business problem

A regional e-commerce operations manager needs to distinguish persistent fulfilment unreliability from short-lived variation, identify which shipping services underperform, and focus investigation where exposure is greatest.

## Key findings

**Monitor.** Monthly late-delivery rates remain elevated across the observed January 2015–January 2018 period; 2018 contains January only.

**Compare.** First Class is 100% late and Second Class approximately 79.99%. Same Day is approximately 48.37% and Standard Class 39.85%. The scheduled-versus-actual shipping context makes expedited modes the first place to investigate service configuration and hand-offs.

**Locate.** Western Europe has about 2.96 million in late-order sales exposure at a roughly 58.4% late rate, and Central America about 2.79 million at roughly 57.3%. Geographic exposure varies more than the regional late rates, making scale useful for prioritisation.

**Diagnose.** The dashboard's product-category and customer-segment view can narrow investigation within a selected geography. It describes concentrations; it does not identify a causal driver.

**Prioritise.** Late and on-time fulfilled orders have strongly overlapping order-level sales and profit distributions. The original report found no large raw commercial separation and does not claim lateness causes lower sales or profit.

## Operational implications

Use the service-level pattern to guide root-cause work, then use geographic exposure to decide where the investigation may have the greatest operational relevance. Keep reliability and associated sales distinct in decision-making.

## Recommendations

Start with First Class and Second Class service performance, examining schedule feasibility and fulfilment hand-offs. Monitor monthly results, then use the geographic and product/customer drill-downs to target investigation. Treat the order-level commercial view as descriptive context.

## Analytical limitations

The dashboard is observational. Late-order sales are exposure, not attributable lost revenue. Currency denomination and dataset licence are unverified. Missing profit contributions affect 112 orders; some shipping durations are missing. The app's CSV import is locale-sensitive, and browser interactions have not been fully verified. This is a portfolio demonstration, not a production-certified deployment.
