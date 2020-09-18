import sys, getopt
import pandas as pd
import numpy as np
import pickle
from sklearn.ensemble import ExtraTreesRegressor
import sys, getopt
#Read the data for fitting
def emulator(argv):
    model = ExtraTreesRegressor()
    args = sys.argv
    L = []
    print(argv[1])
    for i in range(1,len(argv)):
        L.append(float(argv[i]))           
    X_test=np.array([L])
    if (float(argv[1])==1):
        with open('./out_eq1.pkl', 'rb') as file:
            out_eq1 = pickle.load(file)
            ypred= out_eq1.predict(X_test)     
    elif (float(argv[1])>1):
        with open('./out_gt1.pkl', 'rb') as file:
            out_gt1 = pickle.load(file)
            ypred= out_gt1.predict(X_test)
    else:
        with open("./out_lt1.pkl", 'rb') as file:
            out_lt1 = pickle.load(file)
            ypred= out_lt1.predict(X_test)
     
    data =   pd.DataFrame({'q5': np.round(ypred[: ,0],3) ,
                           'q10':np.round(ypred[: ,1],3),
                            'q25':np.round(ypred[: ,2],3),   
                           'q50': np.round(ypred[: ,3],3),
                            'q75':np.round(ypred[: ,4],3),   
                           'q90': np.round(ypred[: ,5],3), 
                             'q95':np.round(ypred[: ,6] ,3)})    
    print( data)
    
if __name__ == "__main__":
    emulator(sys.argv)
