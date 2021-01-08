import numpy as np
import pandas as pd

def read_data(name):
	with(open("problems/Problem"+str(name)+".txt", "r")) as f:
		l = f.readline()
		l = l.split(',')
		sites = int(l[0])
		carry = int(l[1][:-1])
		data_sites = pd.DataFrame(columns=["x", "y", "organic", "plastic", "paper"])
		for i in range(sites):
			l = f.readline()
			l = l.split(',')
			df = pd.DataFrame([float(l[1]), float(l[2]), float(l[3]), float(l[4]), float(l[5][:-1])])
			data_sites.append(df)
		print(data_sites.head())


read_data(1)