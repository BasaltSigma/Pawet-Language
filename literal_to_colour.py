# utility script to convert strings, numbers, etc into colour pixel values

def convert_string(val):
    hex_literals = [hex(ord(x)).replace('0x', '') for x in val]
    remainder = len(hex_literals) % 3
    if remainder % 3 == 1:
        offset = 1
    else:
        offset = 0
    for _ in range(0, remainder + offset):
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
    for _ in range(0, remainder + 2):
        hex_val = "0" + hex_val
    index = 0
    ls = []
    while index < len(hex_val) - 5:
        ls.append(hex_val[index] + hex_val[index + 1] + hex_val[index + 2] + hex_val[index + 3] + hex_val[index + 4] + hex_val[index + 5])
        index = index + 6
    print(ls)

def convert_float(val):
    pass

while True:
    value = input("Enter a string (or 'exit' to stop): ")

    if value.lower() == 'exit':
        break

    t = input("enter type (str, float, int): ")

    if t == "str":
        convert_string(value)
    elif t == 'int':
        convert_int(int(value))
    elif t == 'float':
        convert_float(float(value))
    else:
        print("Invalid type, exiting")
