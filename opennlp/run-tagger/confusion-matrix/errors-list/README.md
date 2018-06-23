# File description #

Each file is a JSON file with a list of JSON objects with the following fields:

- `sentence-text`: sentence text with the wrongly tagged token in special format
- `original-tag`: the correct tag for this token
- `predicted-tags`: the tags for the taggers that had a wrong output
- `sentence-id`: sentence id
- `token-id`: token id in sentence
