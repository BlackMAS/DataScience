
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt 
import scipy as sp
import seaborn as sns
plt.style.use('fivethirtyeight')
import warnings
warnings.filterwarnings('ignore')
get_ipython().run_line_magic('matplotlib', 'inline')


# In[2]:


train=pd.read_csv('train.csv', low_memory=False)


# In[3]:


train.head()


# In[4]:


test=pd.read_csv('test.csv', low_memory=False)


# In[5]:


test.head()


# In[6]:


#check the null values of training data
train.isnull().sum()
#cabin in missing 687
#embarked is missing 2
#age is missing 177


# In[9]:


test.isnull().sum()


# In[40]:


plt.subplots(figsize=(18,8))
plt.subplot(1, 2, 1)
train.Survived.value_counts().plot(kind='pie',explode=[0.1,0],autopct='%1.1f%%',shadow=True)
plt.title('Survival- Pie Chart')


plt.subplot(1, 2, 2)
train.Survived.value_counts().plot(kind='bar')
plt.title('Survival- Bar Chart')
plt.xlabel('Survival Rate')
plt.ylabel('Survival Count')

#It is evident that not many passengers survived the accident.

# Out of 891 passengers in training set, only around 350 survived i.e 
# Only 38.4% of the total training set survived the crash. We need to dig down more to get better 
# insights from the data and see which categories of the passengers did survive and who didn't.
# We will try to check the survival rate by using the different features of the dataset.
# Some of the features being Sex, Port Of Embarcation, Age,etc.
# First let us understand the different types of features.


# ## Analyzing Sex- Categorical Variable

# In[33]:


train.groupby(['Sex','Survived'])['Survived'].count()


# In[51]:


f,ax=plt.subplots(1,2,figsize=(18,8))

sns.countplot('Sex',data=train,hue='Survived',ax=ax[1])
ax[1].set_title('Survival Breakdown per Sex')

train[['Sex','Survived']].groupby(['Sex']).mean().plot(kind='bar',ax=ax[0])
ax[0].set_title('Male vs Female Survival Rate')
plt.show()


# ## Plcass vs survival

# In[55]:


train.Pclass.value_counts()


# In[67]:


f,ax =plt.subplots(1,2,figsize=(18,8))
sns.countplot('Pclass',data=train,hue='Survived',ax=ax[0])
train.Pclass.value_counts().plot(kind='pie',explode=[0.1,0.1,0.1],autopct='%1.1f%%',shadow=True,ax=ax[1])


# In[69]:


pd.crosstab([train.Sex,train.Survived],train.Pclass,margins=True).style.background_gradient(cmap='summer_r')


# ## Age -Continuous 

# In[71]:


#lets find the youngest, oldest, and the average ages of the passenter
train.columns


# In[79]:


print('The oldest person was:', train.Age.max(),'years old')
print('The youngest person was:', round(train.Age.min(),2),'years old')
print('The average age was:', round(train.Age.mean(),2),'years old')


# In[96]:


#lets look at age and passenger vs survival & age and sex vs survival
f,ax=plt.subplots(2,2,figsize=(18,10))
sns.swarmplot('Pclass','Age',data=train,ax=ax[0,0],hue='Survived')
ax[0,0].set_title('Pclass vs Age and Survival')
ax[0,0].set_yticks(range(0,110,10))

sns.swarmplot('Sex','Age',data=train,ax=ax[0,1],hue='Survived')
ax[0,1].set_title('Sex vs Age and Survival')
ax[0,1].set_yticks(range(0,110,10))

sns.violinplot("Pclass","Age", hue="Survived", data=train,split=True,ax=ax[1,0])
ax[1,0].set_title('Pclass and Age vs Survived')
ax[1,0].set_yticks(range(0,110,10))
sns.violinplot("Sex","Age", hue="Survived", data=train,split=True,ax=ax[1,1])
ax[1,1].set_title('Sex and Age vs Survived')
ax[1,1].set_yticks(range(0,110,10))

plt.show()


# In[99]:


# we should see if we can replace the missing age values or impute them with an average value

for i in train:
    train['Title']=train.Name.str.extract('([A-Za-z]+)\.')
train.Title.head()


# In[103]:


pd.crosstab(train.Title,train.Sex,).T.style.background_gradient(cmap='summer_r')

