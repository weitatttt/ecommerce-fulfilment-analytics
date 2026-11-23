# Application requirements

- R with `shiny` and `ggplot2`.
- A locally held, authorised `data.csv` alongside `app.R`.
- A compatible UTF-8 R locale for the supplied CSV's Latin-1 import. The application reads a relative path and does not validate incomplete reads.

Required source columns used in the app's loading and calculations: `Order Id`, `Order Item Total`, `Order Profit Per Order`, `Delivery Status`, `Late_delivery_risk`, `Shipping Mode`, `Days for shipping (real)`, `Days for shipment (scheduled)`, `Market`, `Order Region`, `Order Country`, `Customer Segment`, `order date (DateOrders)`, and `Category Name`.

Expected input grain: one row per order item, with repeated `Order Id` for multi-item orders. Order dates are parsed as month/day/year and include time. The audited source had 180,519 rows, 53 columns and 65,752 unique orders; these counts are a reference for that specific supplied copy, not a general requirement of the application.

Run from the portfolio directory with `shiny::runApp('.')`. The source code has no `testthat` or `renv` runtime import.
