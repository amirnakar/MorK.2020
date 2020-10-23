# MorK.2020
Statistics on App usage data

Short Description: 
This was a small project to test some experiment results on app usage data for a new app

#Data:
Questionnaire answers. Either ranking or time difference measurements
- n = 50 for most parameters
- Rank variables:       5 (easy ; font1 ; font2 ; n.menu1 ; n.menu2)
- Continuous variable : 18 (9 for the old app and 9 for the new app)
- Binary/Categorical variables: gender, app use, laguage and others

Statistics in the code:
1. Test normal distribution: Shapiro Wilkes test ; histograms ; qqplots (for multiple parameters)
2. Test mean differencet   : Wilcoxon test ; box plots ; mean difference calculations
3. Test correlations       : Spearman ; Pearson ; dotplots
4. Permutation MANOVAs     : non-parametric MANOVA alternative for 3 varaibles, using permutation tests (Anderson method) to increase robustness

Vizualisations with ggplot: 
- Box plots
- QQ Plots
- correlation plots
- Distribution histograms

- Faceted plots for many parameters

The project was completed in October 2020
