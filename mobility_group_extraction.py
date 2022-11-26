import pandas as pd

FILENAME = 'data.xlsx'
UNIVERSITY_GROUPS = ["Other Russell Group", "Post-1992 \(least selective\)", "Old Universities \(Pre-1992\)",
                     "Post-1992 \(more selective\)", "Most Selective Russell"]

data = pd.read_excel(FILENAME, sheet_name='mobility')

data['group'] = data['university'].str.extract('(' + '|'.join(UNIVERSITY_GROUPS) + ')', expand=False)
data.to_excel('data.xlsx', sheet_name='mobility', index=False)
