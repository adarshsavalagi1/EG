import re

pic_mapping = {
    'X': 'alphanumeric (string)',
    '9': 'numeric digits',
    'A': 'alphabetic characters',
    'S': 'signed numeric digits',
    'V': 'numeric digits with implied decimal'
}

def remove_comments(cobol_code):
    """
    A function that removes comments from COBOL code. 
    It takes a string of COBOL code as input and returns the code without comments.
    """
    if '/*' in cobol_code and '*/' in cobol_code:
        cobol_code = cobol_code.split('/*')[0] + cobol_code.split('*/')[1]
    pattern = re.compile(r'\*.*$|/\*.*?\*/|//.*$', re.MULTILINE)
    cleaned_code = re.sub(pattern, '', cobol_code)
    cleaned_code = '\n'.join(line.strip() for line in cleaned_code.split('\n'))
    return cleaned_code

def identify_data_structures(cobol_code):
    '''
    This function is used to detect all data structure defined in a program and return them in a list
    '''
    pattern = r'(?<=\b01\s)[A-Za-z0-9-]+\.\n(?:\s+(?:05|10)\s+[A-Za-z0-9-]+\s+PIC\s.+\.\n)+'
    data_structures = re.findall(pattern, cobol_code)
    return  [f'01 {ds.strip()}' for ds in data_structures]

def describe_data_structure(cobol_code):
    '''
    This function is used describe each data structure defined in a program
    '''
    pattern = r'\b\d{2}\s+(\S+)\s+PIC\s+([A-Z0-9\(\)]+)\.'
    matches = re.findall(pattern, cobol_code)
    record_pattern = r'\b01\s+(\S+)\.'
    record_match = re.search(record_pattern, cobol_code)
    record_name = record_match.group(1) if record_match else "unknown record"
    attributes = []
    for field_name, pic_clause in matches:
        pic_type = pic_clause[0]  
        description = pic_mapping.get(pic_type, 'unknown type')
        attributes.append(f"{field_name} whose datatype is {description}")
    if attributes:
        attributes_str = "; ".join(attributes)
        paragraph = f"{record_name} is a data structure which consists of attributes: {attributes_str}."
    else:
        paragraph = f"{record_name} is a data structure with no defined attributes."
    return paragraph


def get_cobol_variables(cobol_code):
    """
    A function that extracts COBOL variables from the given COBOL code.
    It takes a string of COBOL code as input and returns a tuple containing two lists:
    - The first list contains variables without assigned values.
    - The second list contains variables with assigned values.
    """
    pattern_without_values = r"[0-9]+\s+([A-Z0-9-]+)(?!.*VALUE.*)"
    matches_without_values = re.findall(pattern_without_values, cobol_code)
    pattern_with_values = r"01\s+([A-Z0-9-]+).*VALUE\s+'?([^'.]*)'?.*"
    matches_with_values = re.findall(pattern_with_values, cobol_code)
    return matches_without_values, matches_with_values

def extract_data_tables(cobol_code):
   """
   A function that extracts data tables from the given COBOL code.
   
   Parameters:
   - cobol_code: a string containing COBOL code from which data tables need to be extracted.
   
   Returns:
   - A list of tuples where each tuple contains the table name and the content of the table.
   """
   pattern = r'\b\d{2}\s+(\w+)-TABLE\.\n((?:\s{3,}.*\n)+)'
   matches = re.findall(pattern, cobol_code)
   return matches

def extract_variables_and_types(cobol_tables):
    """
    A function that extracts variables and their types from COBOL tables.

    Parameters:
    - cobol_tables: a list of tuples where each tuple contains the table name and the content of the table.

    Returns:
    - A dictionary where each key is a table name and the value is a list of tuples containing variables and their corresponding data types.
    """
    pattern = r'\b\d{2}\s+(\S+)\s+PIC\s+([A-Z0-9\(\)]+)\.'
    variables_and_types = {}
    for table_name, table_content in cobol_tables:
        matches = re.findall(pattern, table_content)
        processed_matches = []
        for var, dtype in matches:
            desc = dtype
            for key, value in pic_mapping.items():
                desc = re.sub(rf'{key}(\(\d+\))?', lambda m: value + m.group(1) if m.group(1) else value, desc)
            processed_matches.append((var, desc))
        variables_and_types[table_name] = processed_matches
    return variables_and_types



def convert_to_python_var(cobol_var):
    return cobol_var.lower().replace('-','_')

def generate_code_for_data_structure(cobol_code):
    '''
    This function is used describe each data structure defined in a program
    '''
    code = ''' 
class {}:
    def __init__(self, {}):
        {}
'''
    pattern = r'\b\d{2}\s+(\S+)\s+PIC\s+([A-Z0-9\(\)]+)\.'
    matches = re.findall(pattern, cobol_code)
    record_pattern = r'\b01\s+(\S+)\.'
    record_match = re.search(record_pattern, cobol_code)
    record_name = record_match.group(1) if record_match else "unknown record"
    attributes = []
    for field_name, _ in matches:
        attributes.append(f"{convert_to_python_var(field_name)}")
    return code.format(convert_to_python_var(record_name),",".join(attributes),'\n\t\t'.join([f'self.{i}={i}' for i in attributes]))




def procedure_division_paragraphs(COBOL_CODE):
    paragraphs = [para.strip() for para in COBOL_CODE.split('\n\n') if para.strip()]

    procedure_division_paragraphs = []

    within_procedure_division = False

    for para in paragraphs:
        if para.startswith('PROCEDURE DIVISION.'):
            within_procedure_division = True
        elif para.startswith('IDENTIFICATION DIVISION.') or para.startswith('ENVIRONMENT DIVISION.') or para.startswith('DATA DIVISION.'):
            within_procedure_division = False

        if within_procedure_division:
            procedure_division_paragraphs.append(para)
            
    return procedure_division_paragraphs 
      



def describe_function(function):
    description = []
    function = function.split('\n')
    for i in function:
        if i.__contains__('PROCEDURE DIVISION.'):
            continue
        elif i.__contains__('PERFORM'):
            description.append(f'{i.strip()}{i}\n\n\n')
        elif i.__contains__('DISPLAY'):
            description.append(f'Print: {i.split("DISPLAY")[1]}\n\n\n')
        elif i.__contains__('COMPUTE'):
            description.append(f'{i.split("COMPUTE")[1]}\n\n\n')
        elif i.__contains__('MOVE'):
            description.append(f'{i }\n\n\n')
        elif i.__contains__('ACCEPT'):
            description.append(f'{i.split("ACCEPT")[1]}\n\n\n')
        elif i.__contains__('STRING'):
            description.append(f'{i }\n\n\n')
        elif i.__contains__('END-PERFORM'):
            description.append('End of function')
            break
        elif i.__contains__('ELSE'):
            description.append(f'{i}\n\n\n')
        elif i.__contains__('END-IF'):
            description.append('End of condition\n\n\n')
        elif i.__contains__('END'):
            description.append('End of function\n\n\n')
            break
        elif i.__contains__('STOP RUN'):
            description.append('End of program\n\n\n')
            break
        elif i.__contains__('END PROGRAM'):
            description.append('End of program\n\n\n')
            break
        elif i.__contains__('UNTIL'):
            description.append(f'Until condition: {i}\n\n\n')
        elif i.__contains__('END UNTIL'):
            description.append('End of condition\n\n\n')
        elif i.__contains__('END'):
            description.append('End of function\n\n\n')
            break
        elif i.__contains__('IF'):
            description.append(f'{i}\n\n\n')
    return description

def get_procedures(cobol_code):
    procedures = procedure_division_paragraphs(cobol_code)
    answer=[]
    for index,procedure in enumerate(procedures):
        answer.append( describe_function(procedure))
    return answer