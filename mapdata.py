
import numpy as np
import pandas as pd
from scipy import stats
import sys

path = r"C:\Users\Drashti.Sikligar\Documents\5 year pao\cod\stats\statsresults.txt"
sys.stdout = open(path, 'w', encoding = "utf-8")

data = pd.read_csv("5yrdata.csv", low_memory=False)
#print(data)

headers = ['MR #', 'Type of Surgery', 'Side', 'FU time', 'Age at Sx', 'Sex', 'BMI', 'Seldes Tear Type', 'ALAD Grade', 'A Outerbridge', 'FH Outerbridge', 'Villar Class', 'Labral Treatment', 'Capsular Treatment', 'Acetabuloplasty', 'Femoroplasty', 'A Microfracture', 'FH Microfracture', 'Pre mHHS', '≥5y mHHS', 'Pre NAHS', '≥5y NAHS', 'Pre HOS-SSS', '≥5y HOS-SSS', 'Pre VAS', '≥5y VAS', '≥5y Satisfaction']

charshead = ['Side', 'FU time', 'Age at Sx', 'Sex', 'BMI']
#radiohead = "i'm a creep. i'm a weirdo. i don't belong here. replace when we have xray data"
intraophead = ['Seldes Tear Type', 'ALAD Grade', 'A Outerbridge', 'FH Outerbridge', 'Villar Class']
ceedhead = ['Labral Treatment', 'Capsular Treatment', 'Acetabuloplasty', 'Femoroplasty', 'A Microfracture', 'FH Microfracture']
proshead = ['Pre mHHS', '≥5y mHHS', 'Pre NAHS', '≥5y NAHS', 'Pre HOS-SSS', '≥5y HOS-SSS', 'Pre VAS', '≥5y VAS', '≥5y Satisfaction', 'latest IHOT']

df_char = data.loc[:, charshead]
#df_radio = data.loc[:, radiohead]
df_intraop = data.loc[:, intraophead]
df_ceed = data.loc[:, ceedhead]
df_pro = data.loc[:, proshead]

#############################################################
side = df_char['Side'].value_counts()
sex = df_char['Sex'].value_counts()
age = df_char['Age at Sx'].mean(axis=0)
agestd = df_char['Age at Sx'].std(axis=0)
followup_time = df_char['FU time'].mean(axis=0)
followup_timestd = df_char['FU time'].std(axis=0)
BMI = df_char['BMI'].mean(axis=0)
BMIstd = df_char['BMI'].std(axis=0)
print(side, sex, 'age', age, agestd, 'followup', followup_time, followup_timestd, 'bmi', BMI, BMIstd)
print()

############################################################
intraophead = ['Seldes Tear Type', 'ALAD Grade', 'A Outerbridge', 'FH Outerbridge', 'Villar Class']
seldes = df_intraop['Seldes Tear Type'].value_counts()
alad = df_intraop['ALAD Grade'].value_counts()
aouter = df_intraop['A Outerbridge'].value_counts()
fhouter = df_intraop['FH Outerbridge'].value_counts()
lttear = df_intraop['Villar Class'].value_counts()
print(seldes, alad, aouter,fhouter,lttear)
print()

#############################################################
ceedhead = ['Labral Treatment', 'Capsular Treatment', 'Acetabuloplasty', 'Femoroplasty', 'A Microfracture', 'FH Microfracture']
labral = df_ceed['Labral Treatment'].value_counts()
captreat = df_ceed['Capsular Treatment'].value_counts()
acetab = df_ceed['Acetabuloplasty'].value_counts()
femoro = df_ceed['Femoroplasty'].value_counts()
amicro = df_ceed['A Microfracture'].value_counts()
fhmicro = df_ceed['FH Microfracture'].value_counts()
print(labral, captreat,acetab,femoro,amicro,fhmicro)
print()

#############################################################
proshead = ['Pre mHHS', '≥5y mHHS', 'Pre NAHS', '≥5y NAHS', 'Pre HOS-SSS', '≥5y HOS-SSS', 'Pre VAS', '≥5y VAS', '≥5y Satisfaction', 'latest IHOT']
mhhs = df_pro['Pre mHHS'].mean(axis=0)
mhhsstd = df_pro['Pre mHHS'].std(axis=0)
mhhs5 = df_pro['≥5y mHHS'].mean(axis=0)
mhhs5std = df_pro['≥5y mHHS'].std(axis=0)
nahs = df_pro['Pre NAHS'].mean(axis=0)
nahsstd = df_pro['Pre NAHS'].std(axis=0)
nahs5 = df_pro['≥5y NAHS'].mean(axis=0)
nahs5std = df_pro['≥5y NAHS'].std(axis=0)
hos = df_pro['Pre HOS-SSS'].mean(axis=0)
hosstd = df_pro['Pre HOS-SSS'].std(axis=0)
hos5 = df_pro['≥5y HOS-SSS'].mean(axis=0)
hos5std = df_pro['≥5y HOS-SSS'].std(axis=0)
vas = df_pro['Pre VAS'].mean(axis=0)
vasstd = df_pro['Pre VAS'].std(axis=0)
vas5 = df_pro['≥5y VAS'].mean(axis=0)
vas5std = df_pro['≥5y VAS'].std(axis=0)
sat = df_pro['≥5y Satisfaction'].mean(axis=0)
satstd = df_pro['≥5y Satisfaction'].std(axis=0)
ihot = df_pro['latest IHOT'].mean(axis=0)
ihotstd = df_pro['latest IHOT'].std(axis=0)
print(mhhs, mhhsstd,mhhs5,mhhs5std,nahs, nahsstd, nahs5, nahs5std,hos, hosstd, hos5, hos5std, vas, vasstd, vas5, vas5std, sat, satstd, ihot, ihotstd)
print()

##################################################################
#stats

#shapiro wilk: looking to disprove null of normality, so if p-value < 0.05, then distribution is not normal
mhhssw = stats.shapiro(df_pro['Pre mHHS'])
print('mhhs', mhhssw.pvalue > 0.05)
mhhs5sw = stats.shapiro(df_pro['≥5y mHHS']) 
print('mhhs 5 year', mhhs5sw.pvalue > 0.05)
nahssw = stats.shapiro(df_pro['Pre NAHS'])
print('nahs', nahssw.pvalue > 0.05)
nahs5sw = stats.shapiro(df_pro['≥5y NAHS'])
print('nahs 5 year', nahs5sw.pvalue > 0.05)
hossw = stats.shapiro(df_pro['Pre HOS-SSS'])
print('hos', hossw.pvalue > 0.05)
hos5sw = stats.shapiro(df_pro['≥5y HOS-SSS'])
print('hos 5 year', hos5sw.pvalue > 0.05)
vassw = stats.shapiro(df_pro['Pre VAS'])
print('vas', vassw.pvalue > 0.05)
vas5sw = stats.shapiro(df_pro['≥5y VAS'])
print('vas 5 year', vas5sw.pvalue > 0.05)
sat5sw = stats.shapiro(df_pro['≥5y Satisfaction'])
print('satisfaction', sat5sw.pvalue > 0.05)
hotsw = stats.shapiro(df_pro['latest IHOT'])
print('latest IHOT', hotsw.pvalue > 0.05)

##################################################################
#wilcoxon test
#seeing if there's a significant difference between pre and post
wmhhs = stats.wilcoxon(df_pro['Pre mHHS'], df_pro['≥5y mHHS'])
print('significance mhhs', wmhhs.pvalue < 0.05, wmhhs.pvalue)
wnahs = stats.wilcoxon(df_pro['Pre NAHS'], df_pro['≥5y NAHS'])
print('significance nahs', wnahs.pvalue < 0.05, wnahs.pvalue)
whos = stats.wilcoxon(df_pro['Pre HOS-SSS'], df_pro['≥5y HOS-SSS'])
print('significance hos', whos.pvalue < 0.05, whos.pvalue)
wvas = stats.wilcoxon(df_pro['Pre VAS'], df_pro['≥5y VAS'])
print('significance vas', wvas.pvalue < 0.05, wvas.pvalue)

"""
#non normal -- 5 years pros, satisfaction, ihot
#^^ these make sense because they should be overall improved so the distribution will be skewed

wmhhs = stats.wilcoxon(df_pro['Pre mHHS'], df_pro['≥5y mHHS'])
print(wmhhs.pvalue)
wnahs = stats.wilcoxon(df_pro['Pre NAHS'], df_pro['≥5y NAHS'])
print(wnahs.pvalue)
whos = stats.wilcoxon(df_pro['Pre HOS-SSS'], df_pro['≥5y HOS-SSS'])
print(whos.pvalue)
wvas = stats.wilcoxon(df_pro['Pre VAS'], df_pro['≥5y VAS'])
print(wvas.pvalue)


"""
#############################################################
"""
"""