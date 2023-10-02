import pandas as pd
import numpy as np
import scipy
from scipy import stats

# stats run on excel -- different tests
 

#Import Excel with values

data1 = pd.read_csv('Matched.csv');

 

### MODIFIED HARRIS HIP SCORE ####

print('mHHS Results')

print('')

 

bidata = data1['X5y.MHHS'][0:358].to_numpy()

unidata = data1['X5y.MHHS'][358:718].to_numpy()

#Shapiro-Wilk Test
unishap1, unishap2 = scipy.stats.shapiro(unidata)
bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)
print('Unilateral Shapiro Wilk p-value: ',unishap2)

#print('Bilateral Shapiro Wilk test statistic: ',bishap1)
print('Bilateral Shapiro Wilk p-value: ',bishap2)

# #Unpaired T-test
# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)
# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')

 
# #Paired T-test
# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)
# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

#Wilcoxon rank test
Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 
print('Wilcoxon P-value: ',Wp)
print('')

### NAHS ####
print('NAHS Results')
print('')

bidata = data1['X5y.NAHS'][0:358].to_numpy()
unidata = data1['X5y.NAHS'][358:718].to_numpy()

#Shapiro-Wilk Test
unishap1, unishap2 = scipy.stats.shapiro(unidata)
bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)
print('Unilateral Shapiro Wilk p-value: ',unishap2)
#print('Bilateral Shapiro Wilk test statistic: ',bishap1)
print('Bilateral Shapiro Wilk p-value: ',bishap2)

# #Unpaired T-test
# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)
# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')
 

# #Paired T-test

# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)

# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

 

#Wilcoxon rank test

Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 
print('Wilcoxon P-value: ',Wp)
print(' ')

# m1 = scipy.stats.tmean(bidata)

# m2 = scipy.stats.tmean(unidata)

# sd1 = scipy.stats.tstd(bidata)

# sd2 = scipy.stats.tstd(unidata)

# print(m1,m2,sd1,sd2)

 

### IHOT ####

print('iHOT-12 Results')
print('')

 

bidata = data1['X5y.IHOT'][0:358].to_numpy()

unidata = data1['X5y.IHOT'][358:718].to_numpy()

 

#Shapiro-Wilk Test

unishap1, unishap2 = scipy.stats.shapiro(unidata)

bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)

print('Unilateral Shapiro Wilk p-value: ',unishap2)

#print('Bilateral Shapiro Wilk test statistic: ',bishap1)

print('Bilateral Shapiro Wilk p-value: ',bishap2)

 

# #Unpaired T-test

# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)

# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')

 

# #Paired T-test

# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)

# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

 

#Wilcoxon rank test

Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 

print('Wilcoxon P-value: ',Wp)

print('')

 

### HOS-SSS ####

print('HOS-SSS Results')

print('')

 

bidata = data1['X5y.HOS.SSS'][0:358].to_numpy()

unidata = data1['X5y.HOS.SSS'][358:718].to_numpy()

 

#Shapiro-Wilk Test

unishap1, unishap2 = scipy.stats.shapiro(unidata)

bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)

print('Unilateral Shapiro Wilk p-value: ',unishap2)

#print('Bilateral Shapiro Wilk test statistic: ',bishap1)

print('Bilateral Shapiro Wilk p-value: ',bishap2)

 

# #Unpaired T-test

# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)

# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')

 

# #Paired T-test

# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)

# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

 

#Wilcoxon rank test

Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 

print('Wilcoxon P-value: ',Wp)

print('')

 

### VAS ####

print('VAS Results')

print('')

 

bidata = data1['X5y.VAS'][0:358].to_numpy()

unidata = data1['X5y.VAS'][358:718].to_numpy()

 

#Shapiro-Wilk Test

unishap1, unishap2 = scipy.stats.shapiro(unidata)

bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)

print('Unilateral Shapiro Wilk p-value: ',unishap2)

#print('Bilateral Shapiro Wilk test statistic: ',bishap1)

print('Bilateral Shapiro Wilk p-value: ',bishap2)

 

# #Unpaired T-test

# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)

# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')

 

# #Paired T-test

# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)

# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

 

#Wilcoxon rank test

Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 

print('Wilcoxon P-value: ',Wp)

print('')

 

### SF.M ####

print('SF.M Results')

print('')

 

bidata = data1['X5y.SF.M'][0:358].to_numpy()

unidata = data1['X5y.SF.M'][358:718].to_numpy()

 

#Shapiro-Wilk Test

unishap1, unishap2 = scipy.stats.shapiro(unidata)

bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)

print('Unilateral Shapiro Wilk p-value: ',unishap2)

#print('Bilateral Shapiro Wilk test statistic: ',bishap1)

print('Bilateral Shapiro Wilk p-value: ',bishap2)

 

# #Unpaired T-test

# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)

# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')

 

# #Paired T-test

# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)

# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

 

#Wilcoxon rank test

Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 

print('Wilcoxon P-value: ',Wp)

print('')

 

### SF.P ####

print('SF.P Results')

print('')

 

bidata = data1['X5y.SF.P'][0:358].to_numpy()

unidata = data1['X5y.SF.P'][358:718].to_numpy()

 

#Shapiro-Wilk Test

unishap1, unishap2 = scipy.stats.shapiro(unidata)

bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)

print('Unilateral Shapiro Wilk p-value: ',unishap2)

#print('Bilateral Shapiro Wilk test statistic: ',bishap1)

print('Bilateral Shapiro Wilk p-value: ',bishap2)

 

# #Unpaired T-test

# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)

# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')

 

# #Paired T-test

# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)

# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

 

#Wilcoxon rank test

Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 

print('Wilcoxon P-value: ',Wp)

print('')

 

### VR.M ####

print('VR.M Results')

print('')

 

bidata = data1['X5y.VR.M'][0:358].to_numpy()

unidata = data1['X5y.VR.M'][358:718].to_numpy()

 

#Shapiro-Wilk Test

unishap1, unishap2 = scipy.stats.shapiro(unidata)

bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)

print('Unilateral Shapiro Wilk p-value: ',unishap2)

#print('Bilateral Shapiro Wilk test statistic: ',bishap1)

print('Bilateral Shapiro Wilk p-value: ',bishap2)

 

# #Unpaired T-test

# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)

# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')

 

# #Paired T-test

# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)

# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

 

#Wilcoxon rank test

Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 

print('Wilcoxon P-value: ',Wp)

print('')

 

### VR.P ####

print('VR.P Results')

print('')

 

bidata = data1['X5y.VR.P'][0:358].to_numpy()

unidata = data1['X5y.VR.P'][358:718].to_numpy()

 

#Shapiro-Wilk Test

unishap1, unishap2 = scipy.stats.shapiro(unidata)

bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)

print('Unilateral Shapiro Wilk p-value: ',unishap2)

#print('Bilateral Shapiro Wilk test statistic: ',bishap1)

print('Bilateral Shapiro Wilk p-value: ',bishap2)

 

# #Unpaired T-test

# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)

# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')

 

# #Paired T-test

# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)

# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

 

#Wilcoxon rank test

Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 

print('Wilcoxon P-value: ',Wp)

print('')

 

### Satisfaction ####

print('Satisfaction Results')

print('')

 

bidata = data1['X5y.Satisfaction'][0:358].to_numpy()

unidata = data1['X5y.Satisfaction'][358:718].to_numpy()

 

#Shapiro-Wilk Test

unishap1, unishap2 = scipy.stats.shapiro(unidata)

bishap1, bishap2 = scipy.stats.shapiro(bidata)

#print('Unilateral Shapiro Wilk test statistic: ',unishap1)

print('Unilateral Shapiro Wilk p-value: ',unishap2)

#print('Bilateral Shapiro Wilk test statistic: ',bishap1)

print('Bilateral Shapiro Wilk p-value: ',bishap2)

 

# #Unpaired T-test

# stat1, pv1 = stats.ttest_ind(unidata, bidata, axis=0, equal_var=True, nan_policy='propagate', permutations=None, random_state=None, alternative='two-sided', trim=0, keepdims=False)

# print(f'Unpaired Two-sample t-test: s = {stat1:5.3f}, p = {pv1:5.3f}')

 

# #Paired T-test

# stat2, pv2 = scipy.stats.ttest_rel(unidata, bidata)

# print(f'Paired Two-sample t-test: s = {stat2:5.3f}, p = {pv2:5.3f}')

 

#Wilcoxon rank test

Wstat, Wp = scipy.stats.wilcoxon(bidata, unidata) 

print('Wilcoxon P-value: ',Wp)

print('')