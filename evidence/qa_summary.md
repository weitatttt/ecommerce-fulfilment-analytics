# QA summary

The existing audit independently reconciled 180,519 item records, 65,752 orders, 62,897 fulfilled orders, 36,048 late fulfilled orders, a 57.31% late-delivery rate, and approximately 18.08 million in late-order net sales exposure. First Class was 100% late and Second Class approximately 79.99% late.

**Automated checks:** 18 passed, 3 failed. The failed data-quality assertions cover order-attribute consistency where duration values are missing, inability to verify late flags from duration on 142 item rows with missing duration, and missing KPI input values. Comparable late flags match. The failures were retained, not suppressed.

**Known limits:** Missing profit contributions affect 112 orders; 73 order-profit totals are partial and 39 are entirely unknown. Some shipping-duration observations are missing, so a representative first item can omit available duration values. CSV import is locale-sensitive and may silently truncate: the inherited invalid locale yielded only two rows in the audit. Browser interaction testing has not been fully verified. The dashboard is a portfolio demonstration, not a production-certified deployment.

The detailed private audit remains outside this public package. No raw data or customer-level audit exports are included.
