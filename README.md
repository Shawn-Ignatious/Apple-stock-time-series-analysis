# Apple Inc. (AAPL) Historical Stock Data

A comprehensive dataset containing historical stock price data for Apple Inc. (AAPL) from **December 1980 to May 1983**.

## Overview

This repository contains daily historical stock data for Apple Inc., including:
- **Open** - Opening price
- **High** - Highest price of the day
- **Low** - Lowest price of the day
- **Close** - Closing price
- **Adj Close** - Adjusted closing price (accounting for splits/dividends)
- **Volume** - Number of shares traded

## Files

| File | Description |
|------|-------------|
| `AAPL.csv` | Historical daily stock data for AAPL |

## Data Details

- **Date Range:** December 12, 1980 – May 11, 1983
- **Frequency:** Daily
- **Records:** 600+ trading days
- **Format:** CSV (Comma Separated Values)

### Column Descriptions

| Column | Type | Description |
|--------|------|-------------|
| `Date` | Date | Trading date (YYYY-MM-DD) |
| `Open` | Float | Opening price |
| `High` | Float | Daily high price |
| `Low` | Float | Daily low price |
| `Close` | Float | Closing price |
| `Adj Close` | Float | Adjusted closing price |
| `Volume` | Integer | Trading volume |

## Getting Started

### Prerequisites

```bash
pip install pandas matplotlib
