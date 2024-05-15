from .main_helper import *

def summarize(code):
    # code = remove_comments(code)
    data_structure = [describe_data_structure(i) for i in identify_data_structures(code)]
    data_tables = extract_variables_and_types(extract_data_tables(code))
    
    variables = get_cobol_variables(code)
    answer=''
    # for data structure
    for i in identify_data_structures(code):
        answer+=generate_code_for_data_structure(i)
        answer+='\n'
    # for variables
    for i in variables[1]:
        answer+=f"{convert_to_python_var(i[0])}='{i[1]}'"+'\n'
    functions = get_procedures(code)
    list1 = ['IF','ACCEPT','ACCEPT','BY','END-IF','UNTIL','COMPUTE']
    return {"data_structure":data_structure,
            'variables_without_data': [element for element in variables[0] if element not in list1],
            "variables_with_data": variables[1],
            "data_tables" :data_tables,"code":answer,'functions':functions}


def generate_code(summary):
    return summary

