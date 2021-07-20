import csv
import pandas as pd
from polyglot.text import Text

# preprocessing
def process_txt(text): 
	pp_text = []
	if len(text) > 0:
		# read text with polyglot
		txt = Text(text, hint_language_code='de') 
		for word in txt.words:
			# remove punctuation & numbers, append lower version of word
			if word.isalpha(): pp_text.append(word.lower()) 
	return pp_text # return list of words

# initialize RAUH dictionary
def initialize_RAUH():
	rauh_file = '/path_to_sentiment_dictionary/RAUH_sentiment_dictionary.csv'
	RAUH_csv = pd.read_csv(rauh_file)
	rauh_dictionary = {}
	for row in RAUH_csv.itertuples():
		rauh_dictionary[row[1].strip()] = row[2]
	return rauh_dictionary

# calculate sentiment score for RAUH
def sentiment_RAUH(text,rauh_dictionary): 
    # negating words according to RAUH
	negation = ['nicht', 'nichts', 'kein', 'keine', 'keinen'] 
	word_count = 0
	all_pos = 0
	all_neg = 0
	for word in text:
		word_count += 1
		if word in rauh_dictionary: 
			# if word in dictionary: check if bigram is a negation
			if word_count > 1:
				ind = word_count - 2
				p_neg = text[ind]
    			# if negated: change score
				if p_neg in negation: 
					score = rauh_dictionary[word] * -1
				else: 
					score = rauh_dictionary[word]
			else: 
				score = rauh_dictionary[word]
			if score > 0:
				all_pos = all_pos + score
			elif score < 0: 
				all_neg = all_neg + score
	# calculate overall score for text
	score_total = (all_neg + all_pos) / word_count 
	return score_total

# read input data
input_data = '/input_path/input_file.csv'
df = pd.read_csv(input_data)
new_file = '/output_path/sentiment_output_file.csv'
header = ['init_id', 'sentiment_RAUH']

# create output file, write header
with open(new_file, 'w') as w:
	writer = csv.DictWriter(w, header)
	writer.writeheader() 

# initialize RAUH sentiment dictionary
rauh_dic = initialize_RAUH()

with open(new_file, 'a') as f:
	writer = csv.writer(f)
	for row in df.itertuples():
		# id & text 
		tid = row[4]
		text = row[9]
		# preprocess text
		pp_text = process_txt(text)
		# annotate sentiment
		sent_RAUH = sentiment_RAUH(pp_text,rauh_dic)
		# write to output file
		entry = [tid, sent_RAUH]
		writer.writerow(entry)