import re

def describe_data_structure(cobol_code):
    """
    A function that describes the structure of COBOL data based on the provided COBOL code.
    It takes the COBOL code as input and returns a descriptive paragraph about the data structure.
    """
    pattern = r'\b\d{2}\s+(\S+)\s+PIC\s+([A-Z0-9\(\)]+)\.'
    
    # Define a mapping from PIC types to descriptions
    pic_mapping = {
        'X': 'alphanumeric (string)',
        '9': 'numeric digits',
        'A': 'alphabetic characters',
        'S': 'signed numeric digits',
        'V': 'numeric digits with implied decimal'
    }

    # Find all matches using the regex pattern
    matches = re.findall(pattern, cobol_code)

    # Extract the record name
    record_pattern = r'\b01\s+(\S+)\.'
    record_match = re.search(record_pattern, cobol_code)
    record_name = record_match.group(1) if record_match else "unknown record"

    # Create descriptive sentences for each match
    attributes = []
    for field_name, pic_clause in matches:
        pic_type = pic_clause[0]  # Extract the type part (e.g., 'X' from 'X(10)')
        description = pic_mapping.get(pic_type, 'unknown type')
        attributes.append(f"{field_name} whose datatype is {description}")
    
    # Construct the paragraph
    if attributes:
        attributes_str = "; ".join(attributes)
        paragraph = f"{record_name} is a data structure which consists of attributes: {attributes_str}."
    else:
        paragraph = f"{record_name} is a data structure with no defined attributes."

    return paragraph

cobol_code = '''
01 WAREHOUSE-RECORD.
   05 WAREHOUSE-ID       PIC X(10).
   05 WAREHOUSE-NAME     PIC X(30).
   05 WAREHOUSE-LOCATION PIC X(50).

'''

# Generate and print the paragraph description
description = describe_data_structure(cobol_code)
print(description)
