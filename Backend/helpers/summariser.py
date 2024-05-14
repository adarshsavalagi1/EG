from .main_helper import *

def summarize(code):
    code = remove_comments(code)
    data_structure = [describe_data_structure(i) for i in identify_data_structures(code)]
    data_tables = extract_variables_and_types(extract_data_tables(code))
    
    variables = get_cobol_variables(code)

    return {"data_structure":data_structure,"variables": variables,"data_tables" :data_tables}


def generate_code(summary):
    return summary

