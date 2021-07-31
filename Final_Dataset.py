#!/usr/bin/env python
# coding: utf-8

# In[61]:


import pandas as pd
import random
import numpy as np

df = pd.read_excel('/Users/sarah/Uni/Bachelorarbeit/neuesThema/Metaanalysis/Data/DatasetR.xlsx')


data_list = []

for col in df.columns:
        
    if "Pay-Off Structure" in col:
        data_list.append(col)
    
    elif "Number of agreement options" in col:
        data_list.append(col)
    
    elif "Priority Factor party 1" in col:
        data_list.append(col)
        
    elif "Max. individual points party 1" in col:
        data_list.append(col)
        
        
newdf = df[data_list]


# In[62]:


# Function to get lists of issues for each observation
row_nr = 0
cols2 = newdf.columns

def Issues(row_nr):
    i_list = []
    ineg_list = []
    d_list = []
    dneg_list = []
    c_list = []
    cneg_list = []
    nan_list = []
    it = 0
    
    # categorizes the issues for one row 
    for column_name in cols2:

        if newdf.at[row_nr,column_name] == 1:
            i_list.append(newdf.at[row_nr, newdf.columns[it+3]])
               

        elif newdf.at[row_nr, column_name] == 2:
            no_issues = newdf.at[row_nr, newdf.columns[it+1]]
            prio = newdf.at[row_nr, newdf.columns[it+2]]
            if newdf.at[row_nr, newdf.columns[it+3]] == 0:
                neg_value = -((no_issues-1)*prio)
                dneg_list.append(neg_value)
                d_list.append(newdf.at[row_nr, newdf.columns[it+3]])
            else:
                d_list.append(newdf.at[row_nr, newdf.columns[it+3]])
                

        elif newdf.at[row_nr, column_name] == 3:
            no_issues = newdf.at[row_nr, newdf.columns[it+1]]
            prio = newdf.at[row_nr, newdf.columns[it+2]]
            if newdf.at[row_nr, newdf.columns[it+3]] == 0:
                neg_value = -((no_issues-1)*prio)
                cneg_list.append(neg_value)
                c_list.append(newdf.at[row_nr, newdf.columns[it+3]])
            else:
                c_list.append(newdf.at[row_nr, newdf.columns[it+3]])

        else:
            nan_list.append('NA')
            
        it += 1
    
    return i_list, d_list, c_list, cneg_list, dneg_list


def Calc(row_nr):
    issues = Issues(row_nr)
    i_list = issues[0]
    d_list = issues[1]
    c_list = issues[2]
    cneg_list = issues[3]
    dneg_list = issues[4]

    # sort integrative list, beginning with greates issue
    i_list = sorted(i_list, reverse = True)

    try: 
        Max = sum(c_list) + sum(d_list) + sum(i_list) # max points each issue  
        Par = sum(c_list) + sum(d_list)/2 + sum(dneg_list)/2 + sum(i_list[:len(i_list)//2]) #  biggest integrative issues      
        Com = sum(c_list)/2 + sum(cneg_list)/2+ sum(d_list)/2  + sum(dneg_list)/2 + sum(i_list)/2 # all issues/2 

        Int = (Par - Com) / (Max - Com)
        CS = 1 - Int
        
    except:
        Max = None
        Par = None
        Com = None
        Int = None
        CS = None

        
    return Max, Par, Com, Int, CS

# Iterate through the whole data set 
Max_values = []
Par_values = []
Com_values = []
Int_values = []
CS_values = []


for elem in range(len(newdf)):
    Dilemma = Calc(elem)
    Max_values.append(Dilemma[0])
    Par_values.append(Dilemma[1])
    Com_values.append(Dilemma[2])
    Int_values.append(Dilemma[3])
    CS_values.append(Dilemma[4])


# In[63]:


# Calculation of Percentage of Integrative Potential achieved 
MeanComp = df['##Grand mean Joint Profits'].sub(df['Compromise Solution'], axis = 0)
MaxComp =df['Maximum Solution'].sub(df['Compromise Solution'], axis = 0)
achIntPot = MeanComp / MaxComp


# In[64]:


# Function to Calculate the Dilemma Strength

def Calc2(row_nr):
    """ 
    Takes in the row number and returns the four individual outcomes Reward, Punishment, Temptation, and Sucker
    as well as the resulting Dilemma Strengths Dg and Dr.
    """
    
    issues = Issues(row_nr)
    i_list = issues[0]
    d_list = issues[1]
    c_list = issues[2]
    cneg_list = issues[3]
    dneg_list = issues[4]

    # sort integrative list, beginning with greates issue
    i_list = sorted(i_list, reverse = True)
  
    try:
        R = sum(c_list) + sum(d_list)/2 + sum(dneg_list)/2 + sum(i_list[:len(i_list)//2])  #  biggest integrative issues 
        P = sum(c_list)/2 + sum(cneg_list)/2 + sum(d_list)/2 + sum(dneg_list)/2 + sum(i_list)/2  # half of all issues
        T = sum(c_list) + sum(d_list)/2 + sum(dneg_list)/2 + sum(i_list[:len(i_list)//2]) + sum(i_list[len(i_list)//2:])/2  # biggest fully & lowest half
        S = sum(c_list) + sum(d_list)/2 + sum(dneg_list)/2 + sum(i_list[:len(i_list)//2])/2  # biggest integrative issue only half
        
        Dg = (T - R)/(R - P) # Gamble-Intending Dilemma 
        Dr = (P - S)/(R - P) # Risk-Averting Dilemma 
  

    except:
        R = None
        P = None
        T = None
        S = None
        Dg = None
        Dr = None
        
    return Dg, Dr, R, P, T, S

# Iterate through the whole data set 
Dg_values = []
Dr_values = []
R_values = []
P_values = []
T_values = []
S_values = []


for elem in range(len(newdf)):
    DilemmaSt = Calc2(elem)
    Dg_values.append(DilemmaSt[0])
    Dr_values.append(DilemmaSt[1])
    R_values.append(DilemmaSt[2])
    P_values.append(DilemmaSt[3])
    T_values.append(DilemmaSt[4])
    S_values.append(DilemmaSt[5])


# In[65]:


all_df = df
all_df = all_df.assign(MaximumPoints_individual=Max_values)  
all_df = all_df.assign(ParetoOptimum_individual=Par_values)
all_df = all_df.assign(Compromise_individual=Com_values)
all_df = all_df.assign(IntegrativePotential=Int_values)
all_df = all_df.assign(ConflictStrength=CS_values)
all_df = all_df.assign(Percentage_IntegrativePotential_achieved=achIntPot)  
all_df = all_df.assign(Reward=R_values)
all_df = all_df.assign(Punishment=P_values)
all_df = all_df.assign(Temptation=T_values)
all_df = all_df.assign(Sucker=S_values)
all_df = all_df.assign(DilemmaStrength_Dg=Dg_values)
all_df = all_df.assign(DilemmaStrength_Dr=Dr_values)

all_df.rename(columns = {'Unnamed: 328':'Comments'}, inplace = True)

# export new data frame 
all_df.to_csv('/Users/sarah/Uni/Bachelorarbeit/neuesThema/Metaanalysis/Data/final/Final_data.csv')


# In[66]:


all_df


# In[ ]:




