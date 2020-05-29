import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

from sklearn import svm, datasets
from sklearn.metrics import auc
from sklearn.metrics import plot_roc_curve
from sklearn.model_selection import StratifiedKFold

cases_covid = pd.read_csv('input/cases_covid.csv', encoding ='latin1')
print(cases_covid.columns)
print(cases_covid.info())

del cases_covid['Unnamed: 0']
dict = {'Estado':{'Recuperado':1,'Fallecido':0}}
cases_covid[["Ciudad.de.residencia", "Sexo","Tipo.de.caso", "Ubicación"]] = cases_covid[["Ciudad.de.residencia", "Sexo","Tipo.de.caso", "Ubicación"]].astype(str)
cases_covid.replace(dict,inplace = True)
# print(cases_covid.columns)
print(cases_covid.info())
# print(cases_covid)
x = cases_covid.loc[:, cases_covid.columns != 'Estado']
y = cases_covid.loc[:, 'Estado']

n_samples, n_features = x.shape

random_state = np.random.RandomState(0)
x = np.c_[x, random_state.randn(n_samples, 200 * n_features)]

cv = StratifiedKFold(n_splits=10)
classifier = svm.SVC(kernel='linear', probability=True, random_state=random_state)

tprs = []
aucs = []
mean_fpr = np.linspace(0,1,100)

fig, ax = plt.subplots()
for i, (train, test) in enumerate(cv.split(x,y)):
    classifier.fit(x[train], y[train])
    viz = plot_roc_curve(classifier, x[test], y[test],
                         name='ROC fold {}'.format(i),
                         alpha=0.3, lw=1, ax=ax)
    interp_tpr = np.interp(mean_fpr, viz.fpr, viz.tpr)
    interp_tpr[0] = 0.0
    tprs.append(interp_tpr)
    aucs.append(viz.roc_auc)

ax.plot([0, 1], [0, 1], linestyle='--', lw=2, color='r',
        label='Chance', alpha=.8)
mean_tpr = np.mean(tprs, axis=0)
mean_tpr[-1] = 1.0
mean_auc = auc(mean_fpr, mean_tpr)
std_auc = np.std(aucs)
ax.plot(mean_fpr, mean_tpr, color='b',
        label=r'Mean ROC (AUC = %0.2f $\pm$ %0.2f)' % (mean_auc, std_auc),
        lw=2, alpha=.8)

std_tpr = np.std(tprs, axis=0)
tprs_upper = np.minimum(mean_tpr + std_tpr, 1)
tprs_lower = np.maximum(mean_tpr - std_tpr, 0)
ax.fill_between(mean_fpr, tprs_lower, tprs_upper, color='grey', alpha=.2,
                label=r'$\pm$ 1 std. dev.')

ax.set(xlim=[-0.05, 1.05], ylim=[-0.05, 1.05],
       title="Receiver operating characteristic example")
ax.legend(loc="lower right")
plt.show()
