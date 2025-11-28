#!/usr/bin/env python3

import pandas as pd
import math
from playsound import playsound
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import colormaps
import seaborn as sns
import csv 
import os
# ^^^ Imports required modules

def import_csv(file):
	"""
	Imports a .csv file into a pandas data frame, to allow error handling

	Args:
	file (str) - name of file wanting to import

	Output:
	Returns file as object ready to assing
	"""
	try:
		df = pd.read_csv(file)
		return df
	except pd.errors.EmptyDataError:
		print("####ERROR import_csv: File is not a .csv. Cannot be read####")
		print("####                  Quitting program                  ####")
		quit() 
	except FileNotFoundError:
		print("####ERROR import_csv: File not found. Cannot be read####")
		print("####                 Quitting program               ####")
		quit() 


def lbs_to_kg(df, col, new_col = 'new_column'):
	"""
	Converts weights in a column which are in lbs into kg

	Args:
	df - Dataset to look in
	col - Column wanting to convert
	new_col - name of new column for conversion data to go

	Output
	Adds a new column with converted weights
	"""

	try:
		df[new_col] = (df[col]*0.4536)
		return df # Creates weights_kg column, converst lbs to kg and adds it in

	except KeyError as e:
		print(f"####ERROR lbs_to_kg: a/multiple provided args do not exist####")
		print(f"####Args: {e.args}####\n")
		return None
	except TypeError:
		print("####ERROR lbs_to_kg: col does not contain int/float####")
		print("####        	  new_col not created              ####\n")
		return None


def classify(df, col, new_col, classes = [0, 1, 2]):
	"""
	Adds a new column with classes for a chosen column

	Args:
	df - Dataset to look in
	col - Column wanting to classify
	new_col - name of new column to add classes in
	classes - Classes wanted to be assigned in new_col. Must be length of 3 

	Output
	Adds a new column to df with classes. Returns the new dataset
	"""
	try:
		mean = df[col].mean() #Calculates mean of weights in kg
		sd = df[col].std() #Calculates standard deviation of weights in kg
		df[new_col] = '' #Creates new column 
		

		for i in range(len(df[col])): #Loops through the weight_kg column
		
			if df[col][i] < (mean - sd): #Selects rows with weight = mean-sd
				df.iloc[i, df.columns.get_loc(new_col)] = classes[0] #Adds first value class to new_col
			
			elif df[col][i] > (mean + sd): #Selects rows with weight = mean+sd
				df.iloc[i, df.columns.get_loc(new_col)] = classes[2] #Adds 3rd value of class to new_col

			elif math.isnan(df[col][i]) == True:
				df.iloc[i, df.columns.get_loc(new_col)] = np.nan

			else:
				df.iloc[i, df.columns.get_loc(new_col)] = classes[1] #Else, rest get 2nd value of class added to weight_class column

		return df

	except TypeError:
		print("####ERROR classify: col is not int/float####")
		print("####      df returned unchanged       ####\n")
		return df
	except KeyError as e:
		print("####ERROR classify: a/multiple provided args do not exist####")
		print("####                df returned unchanged              ####")
		print(f"####Args: {e.args}####\n")
		return df
	except AttributeError:
		print("####ERROR classify: Cannot pull from df as not a df frame####")
		print("####                      Returned None                      ####\n")
		return None


def filter_col(df, col, filt = [], file_name='filter_output'):
	"""
	Creates filtered .csv file for pumpkins for chosen filters

	Args:
	df (data frame)- Dataset to look in
	col (str) - Column to filter by
	filt (list) - Selected items to filter for
	file_name (str) - Chosen name for file to be outputed. Does not require .csv extention

	Output:
	.csv - Contains only pumpkin df from chosen countries
	"""
	try:
		df_filtered = pd.DataFrame(columns=list(df.columns.values)) #Creates blank data frame with same 

		for i in range(len(filt)): #For loop iterates through all items in filt.
			df_filtered = df_filtered._append(df[df[col] == filt[i]]) #Adds all rows contain item i in filt into the blank data frame

		df_filtered.to_csv(file_name+'.csv', sep=',', index=False, header=True) #Exports the data frame as a .csv file

		return df_filtered

	except KeyError as e:
		print("####ERROR filter_col: a/multiple provided args do not exist.####")
		print("####       Unable to filter, df returned unflitered       ####\n")
		print(f"####Args: {e.args}####")
		return df
	except TypeError as e:
		print("####´ERROR filter_col: col does not contain int/float.####")
		print("####    Unable to filter, df returned unflitered    ####\n")
		return df
	except AttributeError:
		print("####ERROR filter_col: Cannot pull from df as not a data frame####")
		print("####                        Returned None                      ####\n")
		return None


def summary_mean(df, col, groups):
	"""
	Summarises df by selected groupings and operation

	Args:
	df (data frame) - Dataset to summarise
	col (str) - Column in df to summarise
	group (list) - Columns to summarise by. First vaule is the main column, gives individual sumary.

	Output:
	Table with dffor country and variety
	"""
	try:
		summ1 = str(df.groupby(groups[0])[col].mean()) #Prints summary
		if len(groups) > 1: #Checks if more than one value is in groups. If yes, provides the multiple summary
			summ2 = str(df.groupby(groups)[col].mean()) #Prints multiple summary
			return f"{summ1}\n\n{summ2}"
		else:
			return summ1

	except KeyError as e:
		print("####ERROR filter_col: a/multiple provided args do not exist.####")
		print("####      Unable to get mean, Error message returned        ####")
		print(f"####Args: {e.args}####\n")
		return "####ERROR filter_col: a/multiple provided args do not exist.####\n\n"

	except TypeError as e:
		print("####ERROR filter_col: col provided does not contain int/float.####")
		print("####       Unable to get mean, Error message returned         ####")
		print(f"####Args: {e.args}####\n")
		return "####ERROR filter_col: col provided does not contain int/float.####\n\n"
	except AttributeError:
		print("####ERROR filter_col: Cannot pull from df as not a data frame####\n")
		print("####                        Returned None                      ####")
		return None


def main():

	os.system("osascript -e \"set volume output volume 100\"")
	playsound('./.surprise.mp3') #Plays mystery sound
	pumpkin = import_csv('pumpkins_08.csv')
	
	output = open('output.txt', 'w')

	#Finding heaviest pumpkin
	try:
		heaviest = pumpkin.loc[pumpkin['weight_lbs'].idxmax()] # Returns row of heaviest pumpkin

		output.write(f"The heaviest pumkin was in {str(heaviest['id'])[:4]}. Variety: {heaviest['variety']}, Location: {heaviest['city']}, {heaviest['country']}.\n\n")	
		# ^^^ Prints information. Takes first 4 characters of id for the year
	except KeyError as e:
		print("####ERROR main (find heaviest): a/multiple provided args do not exist.####")
		print("####    Unable to get mean, Error message returned    ####\n")
		output.write("####ERROR main (find heaviest): a/multiple provided args do not exist.####")
		output.write("####    Unable to get mean, Error message returned    ####\n\n")
	except AttributeError as e:
		print("####ERROR main: (find heaviest) Trying to locate in a none df object####\n")
		output.write("####ERROR main (find heaviest): Trying to locate in a none df object####\n")


	lbs_to_kg(pumpkin, 'weight_lbs', 'weight_kg') #Creates new column with converted weights named 'weight_kg'


	classify(pumpkin, 'weight_kg', 'weight_class', ['light', 'medium', 'heavy'])


	pumpkin_filtered = filter_col(pumpkin, 'country', ['Spain', 'Japan', 'France'], 'pumpkins_filtered')


	summary_out = summary_mean(pumpkin_filtered, 'weight_kg', ['country', 'variety'])
	output.writelines(str(summary_out))


	#Fig 1
	#filter_col(pumpkin, 'est_weight', [0], 'pumpkin_0_est') #Used to investigate whether to remove zeros from plot
	lbs_to_kg(pumpkin, 'est_weight', 'est_weight_kg') #Creates new column with converted weights named 'est_weight_kg'

	try:
		pumpkin['est_weight'] = pumpkin['est_weight'].replace({0:np.nan}) #Sets 0 values to NaN within 'estimated_weight'
		
	except Exception as e:
		print("####ERROR main (remove 0): a/multiple provided args do not exist.####")
		print(f"####Args: {e.args}####\n")
	
	try:
		fig1 = sns.scatterplot(
			x='est_weight_kg', y='weight_kg', #Sets axis
			hue='weight_class', #Colours by class
			data=pumpkin) #Data is pumpkins dataset
		plt.title('A scatter graph of Estimated Weight vs Recorded Weight') #Gives title
		plt.xlabel('Estimated weight/kg') #Labels x-axis
		plt.ylabel('Recorded weight/kg') #Labels y-axis
		plt.legend(title='Weight Class') #Gives title to legend
		plt.savefig('Figure_1.png') #Saves plot to png

	except Exception as e:
		print(f"####ERROR Fig1: Trying to plot none correct data####")
		print(f"####{e.args}####\n")

	#Fig2
	try:
		fig2 = pumpkin_filtered.boxplot(column='weight_kg', by='country', grid=False) #Generates boxplot without a grid
		plt.title('A Boxplot of 3 countries and the Weights of Pumpkins') #Gives title
		plt.ylabel('Country') #Labels x axis
		plt.xlabel('Weight/kg') #Labels y axis
		plt.savefig('Figure_2.png') #Saves as png

	except Exception as e:
		print(f"####ERROR Fig2: Trying to plot none correct data####")
		print(f"####{e.args}####\n")

	#Fig3
	try:
		fig3 = sns.FacetGrid(pumpkin_filtered, #Data taken from pumkin_filtered and produces facet grid
			col='country', #Facets the data by country
			hue='variety', #Colours boxplots by pumpkin variety
			height=6, aspect=0.75, #Sets size of the axis
			palette='tab20') #Sets colour for hue
		fig3.map_dataframe(sns.boxplot, 'variety', 'weight_kg') #Creates the boxplots using seaborn
		fig3.fig.suptitle('Boxplots of Variety Against Weights, Grouped by Country') #Gives title
		fig3.set_titles(col_template='{col_name}', row_template='{row_name}') #Labels facet grid row and column
		fig3.set_axis_labels('Variety', 'Weight/kg') #Labels axis
		fig3.set_xticklabels(rotation=90) #Writes x axis tick lables vertically
		fig3.figure.tight_layout(rect=[0,0,1,0.95]) #Gives more room for plot
		plt.savefig('Figure_3.png') #Saves as png

	except Exception as e:
		print(f"####ERROR Fig3: Trying to plot none correct data####")
		print(f"####{e.args}####\n")

	output.close()
	exit()
	pass


if __name__ == "__main__":
	main() 
