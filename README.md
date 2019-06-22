# Lyft Driver Data Analysis

by Alex Liebscher

Summer 2019

### Data

**LyftData.csv**

Contains all data that I've collected from driving with Lyft. This file is an export of a Google Spreadsheet I add to after each ride. More detailed description coming soon.

**CleanLyftData.csv**

Cleansed Lyft Data, ready to analyze.

### Scripts

**CleanData.R**

Quick script to clean up the raw data for analysis.

Uses `LyftData.csv` to create `CleanLyftData.csv`.

**Exploration.Rmd**

Break down the data into an interpretable and insightful collection of metrics and visualizations. The ultimate goal here is figure out what might maximize my driving earnings.

**random_assignments.R**

We must sample the driving space somehow, and the most controllable independent variables are day of week and time of day that I go drive. This script offers randomly selected times in the week to go drive. Ideally, this will prevent me from making implicit judgements about what might be a good time to go drive (these judgements aren't properly defined so it's hard to justify them as priors).