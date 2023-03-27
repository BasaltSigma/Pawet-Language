# utility script to convert strings, numbers, etc into colour pixel values

def convert_string(val):
    hex_literals = [hex(ord(x)).replace('0x', '') for x in val]
    remainder = len(hex_literals) % 3
    if remainder != 0:        
        for _ in range(0, 3 - remainder):
            hex_literals.insert(0, '00')
    print(hex_literals)
    pixel_vals = []
    index = 0
    while index < len(hex_literals) - 2:
        pixel_vals.append(hex_literals[index] + hex_literals[index + 1] + hex_literals[index + 2])
        index = index + 3
    print(pixel_vals)

def convert_int(val):
    hex_val = hex(val).replace('0x', '')
    remainder = len(hex_val) % 6
    if remainder != 0:        
        for _ in range(0, 6 - remainder):
            hex_val = "0" + hex_val
    index = 0
    ls = []
    while index < len(hex_val) - 5:
        ls.append(hex_val[index] + hex_val[index + 1] + hex_val[index + 2] + hex_val[index + 3] + hex_val[index + 4] + hex_val[index + 5])
        index = index + 6
    print(ls)

def convert_float(val):
    parts = val.split('.')
    ls = []
    whole_val = hex(int(parts[0])).replace('0x', '')
    remainder = len(whole_val) % 6
    if remainder != 0:
        for _ in range(0, 6 - remainder):
            whole_val = "0" + whole_val
    index = 0
    while index < len(whole_val) - 5:
        ls.append(whole_val[index] + whole_val[index + 1] + whole_val[index + 2] + whole_val[index + 3] + whole_val[index + 4] + whole_val[index + 5])
        index = index + 6
    ls.append("c9e0ff")
    decimal_val = [hex(ord(x)).replace('0x', '') for x in parts[1]]
    remainder = len(decimal_val) % 3
    if remainder != 0:
        for _ in range(0, 3 - remainder):
            decimal_val.insert(0, '00')
    index = 0
    while index < len(decimal_val) - 2:
        ls.append(decimal_val[index] + decimal_val[index + 1] + decimal_val[index + 2])
        index = index + 3
    print(ls)


while True:
    value = input("Enter a string (or 'exit' to stop): ")

    if value.lower() == 'exit':
        break

    t = input("enter type (str, float, int): ")

    if t == "str":
        convert_string(value)
    elif t == 'int':
        try:
            convert_int(int(value))
        except:
            print("Invalid integer number format")
    elif t == 'float':
        try:
            convert_float(value)
        except:
            print("Invalid float number format")
    else:
        print("Invalid type, exiting")
