from PIL import Image
from enum import Enum
import os

class PawetInterpreterError(Exception):
    
    def __init__(self, index_of_error: int, error_rgb_val: int, message: str):
        self.index_of_error = index_of_error
        self.error_rgb_val = error_rgb_val
        self.message = message
        super().__init__(f"{self.error_rgb_val} at pos {self.index_of_error}: {self.message}")

class Colours(Enum):
    SKIP = 0xffffff
    STATEMENT_END = 0x102010
    BRACKET_START = 0x000000
    BRACKET_END = 0xafafaf
    STRING_LITERAL = 0xff0000
    ARRAY = 0x450caf
    INDEX = 0xfe1437
    IF = 0x007f0e
    END_IF = 0x4cff00
    ELSE = 0x56ff94
    END_ELSE = 0xffbe47
    TRUE = 0x7f6a00
    FALSE = 0xff006e
    FUNCTION = 0xb200ff
    END_FUNCTION = 0x57007f
    VAR_FUN_NAME = 0xffd800
    ESCAPE_NEXT = 0x7f0037  # escape a bracket or string or function demarcator in case a colour is that
    BOOL_TYPE = 0x0037ff
    INT_TYPE = 0x7f3300
    FLOAT_TYPE = 0x404040
    STRING_TYPE = 0x3f3400
    VOID_TYPE = 0xff7fed
    RETURN = 0xff7f42
    LOOP = 0xffb584
    END_LOOP = 0x007f7f
    COMMA = 0x00a510
    ASSIGNMENT = 0xffc200
    INT_LITERAL = 0xffc2e8
    FLOAT_LITERAL = 0xff7fb6
    POINT = 0xc9e0ff
    INCREMENT_VAL = 0x328bff
    DECREMENT_VAL = 0x328b27
    ADD = 0x5c2589
    SUBTRACT = 0x108967
    MULTIPLY = 0x89403b
    DIVIDE = 0x788956
    MODULO = 0x896765
    POWER = 0x00137f
    LOGICAL_NOT = 0x3f7f47
    BITWISE_AND = 0x1f0052
    BITWISE_OR = 0x0b514e
    BITWISE_SHIFT_LEFT = 0x4e4f51
    BITWISE_SHIFT_RIGHT = 0x787c51
    LOGICAL_AND = 0x7a296c
    LOGICAL_OR = 0xf5ff3f
    GREATER = 0xae63ff
    GREATER_EQUAL = 0xff0094
    EQUALS = 0xadffe2
    LESS_EQUAL = 0xffd6a5
    LESS = 0xff5111
    NOT_EQUAL = 0xff3538
    BREAK = 0xffd9cc
    CONTINUE = 0xff5eae
    END_OF_PROGRAM = 0xedfff2
    PRINT = 0xf18eff
    EQUATION = 0x16741a
    IMPORT = 0x42f301

class PawetInterpreter:

    def __init__(self, image_file_path: str):
        im = Image.open(image_file_path)
        im = im.convert("RGB")
        self.width, self.height = im.size
        self.image = list(im.getdata())
        im.close()
        self.current_index = 0
        self.x = 0
        self.y = 0
        self.variables = {}
        self.functions = {}
    
    def interpret(self):
        while self.current_index < self.width * self.height:
            pix = self.read_next_pixel()
            if pix == Colours.END_OF_PROGRAM.value:
                break
            elif pix == Colours.SKIP.value or pix == Colours.STATEMENT_END.value:
                continue
            elif pix == Colours.IMPORT.value:
                pix = self.read_next_pixel()
                if pix != Colours.STRING_LITERAL.value:
                    raise PawetInterpreterError(self.current_index, pix, "Invalid import statement - must be a string literal after the import pixel")
                to_import = self.parse_string_literal()
                pix = self.read_next_pixel()
                if pix != Colours.STATEMENT_END:
                    raise PawetInterpreterError(self.current_index, pix, "Missing end of statement pixel for import statement")
                if not os.path.isfile(to_import):
                    raise PawetInterpreterError(self.current_index, pix, "That pawet image file is not found")
                paw = PawetInterpreter(to_import)
                self.functions.update(paw.functions)
                self.variables.update(paw.variables)
            elif pix == Colours.BOOL_TYPE.value:
                self.add_bool_var(self.variables)
                continue
            elif pix == Colours.INT_TYPE.value:
                self.add_int_var(self.variables)
                continue
            elif pix == Colours.FLOAT_TYPE.value:
                self.add_float_var(self.variables)
                continue
            elif pix == Colours.STRING_TYPE.value:
                self.add_str_var(self.variables)
                continue
            elif pix == Colours.PRINT.value:
                self.parse_print(self.variables)
                continue
            elif pix == Colours.VAR_FUN_NAME.value:
                var_ref = self.parse_var_fun_reference()
                if not var_ref in self.variables:
                    raise PawetInterpreterError(self.current_index, pix, "Variable not initialized")
                pix = self.read_next_pixel()
                if pix == Colours.INCREMENT_VAL.value:
                    if self.variables[var_ref][0] not in [int, float]:
                        raise PawetInterpreterError(self.current_index, pix, "Not an incrementable type")
                    self.variables[var_ref] = (self.variables[var_ref][0], self.variables[var_ref][1] + 1)
                elif pix == Colours.DECREMENT_VAL.value:
                    if self.variables[var_ref][0] not in [int, float]:
                        raise PawetInterpreterError(self.current_index, pix, "Not a decrementable type")
                    self.variables[var_ref] = (self.variables[var_ref][0], self.variables[var_ref][1] - 1)
                if self.read_next_pixel() != Colours.STATEMENT_END.value:
                    raise PawetInterpreterError(self.current_index, pix, "Statement not closed")
                continue
            else:
                raise PawetInterpreterError(self.current_index, pix, "Unexpected pixel, Did you maybe forget to escape?")
    
    def read_next_pixel(self) -> int:
        if self.current_index >= self.width * self.height:
            raise PawetInterpreterError(self.current_index, None, "End of image reached before end of program")
        val: tuple = self.image[self.current_index]
        self.current_index = self.current_index + 1
        output = (val[0] << 16) + (val[1] << 8) + val[2]
        return output

    def parse_int_literal(self) -> int:
        pix = self.read_next_pixel()
        prev_pix = None
        rgb = []
        while True:
            if pix == Colours.ESCAPE_NEXT.value:
                prev_pix = pix
                pix = self.read_next_pixel()
                continue
            if pix == Colours.INT_LITERAL.value and prev_pix != None and prev_pix != Colours.ESCAPE_NEXT.value:
                break
            rgb.append(pix)
            prev_pix = pix
            pix = self.read_next_pixel()
        val: int = 0
        for i in range(0, len(rgb)):
            val = val + (rgb[i] << ((len(rgb) - i - 1) * 24))
        return val
    
    def parse_float_literal(self) -> float:
        pix = self.read_next_pixel()
        prev_pix = None
        whole = []
        decimal = []
        decimal_point_reached = False
        while True:
            if pix == Colours.ESCAPE_NEXT.value:
                prev_pix = pix
                pix = self.read_next_pixel()
                continue
            if pix == Colours.FLOAT_LITERAL.value and prev_pix != Colours.ESCAPE_NEXT.value:
                break
            if pix == Colours.POINT.value:
                decimal_point_reached = True
                prev_pix = pix
                pix = self.read_next_pixel()
                continue
            if decimal_point_reached:
                decimal.append(pix)
            else:
                whole.append(pix)
            prev_pix = pix
            pix = self.read_next_pixel()
        first: int = 0
        second: int = 0
        for i in range(0, len(whole)):
            first = first + (whole[i] << ((len(whole) - i - 1) * 24))
        for i in range(0, len(decimal)):
            second = second + (decimal[i] << ((len(decimal) - i - 1) * 24))
        return float(f"{first}.{second}")
    
    def parse_string(self, rgb: list) -> str:
        val: str = ""
        for p in rgb:
            r = p >> 16 & 0x0000ff
            g = p >> 8 & 0x0000ff
            b = p & 0x0000ff
            if r == 0 and g != 0 and b != 0:
                val = val + chr(g) + chr(b)
            elif r == 0 and g == 0 and b != 0:
                val = val + chr(b)
            else:
                val = val + chr(r) + chr(g) + chr(b)
        return val
    
    def parse_string_literal(self) -> str:
        val: list = []
        pix = self.read_next_pixel()
        prev_pix = None
        while True:
            if pix == Colours.ESCAPE_NEXT.value:
                prev_pix = pix
                pix = self.read_next_pixel()
                continue
            if pix == Colours.STRING_LITERAL.value and prev_pix != None and prev_pix != Colours.ESCAPE_NEXT.value:
                break
            val.append(pix)
            prev_pix = pix
            pix = self.read_next_pixel()
        return self.parse_string(val)
    
    def add_bool_var(self, context: dict):
        pix = self.read_next_pixel()
        var_name: str = ""
        val: bool = False
        past_assignment: bool = False
        ref_var = None
        while not pix == Colours.STATEMENT_END.value:
            if pix == Colours.SKIP.value:
                pix = self.read_next_pixel()
                continue
            if pix == Colours.VAR_FUN_NAME.value:
                var_name = self.parse_var_fun_reference()
                pix = self.read_next_pixel()
                continue
            if pix == Colours.ASSIGNMENT.value:
                past_assignment = True
                pix = self.read_next_pixel()
                continue
            if past_assignment:
                if pix == Colours.TRUE.value:
                    val = True
                elif pix == Colours.FALSE.value:
                    val = False
                elif pix == Colours.VAR_FUN_NAME.value:
                    ref_var = self.parse_var_fun_reference()
                elif pix == Colours.EQUATION.value:
                    val = self.parse_equation(context)
                else:
                    raise PawetInterpreterError(self.current_index, pix, "Invalid pixel, expecting true or false pixel")
            pix = self.read_next_pixel()
        if var_name in context:
            raise PawetInterpreterError(self.current_index, pix, "Variable already declared")
        if ref_var != None:
            if not ref_var in context:
                raise PawetInterpreterError(self.current_index, pix, "Variable not found, has it been initialized?")
            if not context[ref_var][0] in [int, bool]:
                raise PawetInterpreterError(self.current_index, pix, "The variable you are trying to assign is not an int or bool")
            if context[ref_var][0] == int:
                context[var_name] = (bool, context[ref_var][1] != 0)
            else:
                context[var_name] = (bool, context[ref_var][1])
        else:
            context[var_name] = (bool, val)

    def add_int_var(self, context: dict):
        pix = self.read_next_pixel()
        past_assignment = False
        assignment_name = ""
        literal = None
        target_ref = None
        while not pix == Colours.STATEMENT_END.value:
            if pix == Colours.SKIP.value:
                pix = self.read_next_pixel()
                continue
            if pix == Colours.VAR_FUN_NAME.value and not past_assignment:
                assignment_name = self.parse_var_fun_reference()
                pix = self.read_next_pixel()
                continue
            if pix == Colours.ASSIGNMENT.value:
                past_assignment = True
                pix = self.read_next_pixel()
                continue
            if past_assignment:
                if pix == Colours.INT_LITERAL.value:
                    literal = self.parse_int_literal()
                elif pix == Colours.VAR_FUN_NAME.value:
                    target_ref = self.parse_var_fun_reference()
                elif pix == Colours.EQUATION.value:
                    literal = self.parse_equation(context)
            pix = self.read_next_pixel()
        if assignment_name in context:
            raise PawetInterpreterError(self.current_index, pix, "Variable already declared")
        if target_ref != None and literal == None:
            if not target_ref in context:
                raise PawetInterpreterError(self.current_index, pix, "Variable not found, has it been initialized?")
            if not context[target_ref][0] in [int, float, bool]:
                raise PawetInterpreterError(self.current_index, pix, "The variable you are trying to assign is not an int, float, or bool")
            context[assignment_name] = (int, int(context[target_ref][1]))
        elif target_ref == None and literal != None:
            context[assignment_name] = (int, literal)

    def add_float_var(self, context):
        pix = self.read_next_pixel()
        past_assignment = False
        assignment_name = ""
        literal = None
        target_ref = None
        while not pix == Colours.STATEMENT_END.value:
            if pix == Colours.SKIP.value:
                pix = self.read_next_pixel()
                continue
            if pix == Colours.VAR_FUN_NAME.value and not past_assignment:
                assignment_name = self.parse_var_fun_reference()
                pix = self.read_next_pixel()
                continue
            if pix == Colours.ASSIGNMENT.value:
                past_assignment = True
                pix = self.read_next_pixel()
                continue
            if past_assignment:
                if pix == Colours.FLOAT_LITERAL.value:
                    literal = self.parse_float_literal()
                elif pix == Colours.VAR_FUN_NAME.value:
                    target_ref = self.parse_var_fun_reference()
                elif pix == Colours.EQUATION.value:
                    literal = self.parse_equation(context)
            pix = self.read_next_pixel()
        if assignment_name in context:
            raise PawetInterpreterError(self.current_index, pix, "Variable already declared")
        if target_ref != None and literal == None:
            if not target_ref in context:
                raise PawetInterpreterError(self.current_index, pix, "Variable not found, has it been initialized?")
            if not context[target_ref][0] in [float, int]:
                raise PawetInterpreterError(self.current_index, pix, "The variable you are trying to assign is not of type float or int")
            context[assignment_name] = (float, float(context[target_ref][1]))
        elif target_ref == None and literal != None:
            context[assignment_name] = (float, literal)

    def add_str_var(self, context):
        pix = self.read_next_pixel()
        past_assignment = False
        assignment_name = ""
        literal = None
        target_ref = None
        while not pix == Colours.STATEMENT_END.value:
            if pix == Colours.SKIP.value:
                pix = self.read_next_pixel()
                continue
            if pix == Colours.VAR_FUN_NAME.value and not past_assignment:
                assignment_name = self.parse_var_fun_reference()
                pix = self.read_next_pixel()
                continue
            if pix == Colours.ASSIGNMENT.value:
                past_assignment = True
                pix = self.read_next_pixel()
                continue
            if past_assignment:
                if pix == Colours.STRING_LITERAL.value:
                    literal = self.parse_string_literal()
                elif pix == Colours.VAR_FUN_NAME.value:
                    target_ref = self.parse_var_fun_reference()
                elif pix == Colours.EQUATION.value:
                    literal = self.parse_equation(context)
            pix = self.read_next_pixel()
        if assignment_name in context:
            raise PawetInterpreterError(self.current_index, pix, "Variable already declared")
        if target_ref != None and literal == None:
            if not target_ref in context:
                raise PawetInterpreterError(self.current_index, pix, "Variable not found, has it been initialized?")
            if not context[target_ref][0] == str:
                raise PawetInterpreterError(self.current_index, pix, "The variable you are trying to assign is not a string")
            context[assignment_name] = (str, context[target_ref][1])
        elif target_ref == None and literal != None:
            context[assignment_name] = (str, literal)

    def parse_var_fun_reference(self):
        vals_rgb = []
        pix = self.read_next_pixel()
        prev_pix = None
        while True:
            if pix == Colours.ESCAPE_NEXT.value:
                prev_pix = pix
                pix = self.read_next_pixel()
                continue
            if pix == Colours.VAR_FUN_NAME.value and prev_pix != None and prev_pix != Colours.ESCAPE_NEXT:
                break
            vals_rgb.append(pix)
            prev_pix = pix
            pix = self.read_next_pixel()
        return self.parse_string(vals_rgb)
    
    def parse_print(self, context):
        following = self.read_next_pixel()
        if following == Colours.VAR_FUN_NAME.value:
            ref = self.parse_var_fun_reference()
            if not ref in context:
                raise PawetInterpreterError(self.current_index, following, "Variable is not defined")
            print(context[ref][1])
        elif following == Colours.INT_LITERAL.value:
            val = self.parse_int_literal()
            print(val)
        elif following == Colours.EQUATION.value:
            print(self.parse_equation(context))
        elif following == Colours.TRUE.value:
            print(True)
        elif following == Colours.FALSE.value:
            print(False)
        elif following == Colours.STRING_LITERAL.value:
            print(self.parse_string_literal())
        elif following == Colours.FLOAT_LITERAL.value:
            print(self.parse_float_literal())
        if self.read_next_pixel() != Colours.PRINT.value and self.read_next_pixel() != Colours.STATEMENT_END.value:
            raise PawetInterpreterError(self.current_index, following, "Print function is not closed properly")
    
    def parse_equation(self, context):
        val = self.solve_brackets(context, base=True)
        return val
        
    def solve_brackets(self, context, base=False):
        pix = self.read_next_pixel()
        prev_pix = None
        vals = []
        operators = []
        not_next_val = False
        while True:
            if pix == Colours.SKIP.value or pix == Colours.ESCAPE_NEXT.value:
                prev_pix = pix
                pix = self.read_next_pixel()
                continue
            if base and pix == Colours.EQUATION.value and prev_pix != Colours.ESCAPE_NEXT.value:
                break
            if not base and pix == Colours.BRACKET_END.value and prev_pix != Colours.ESCAPE_NEXT.value:
                break
            if pix == Colours.LOGICAL_NOT.value:
                prev_pix = pix
                pix = self.read_next_pixel()
                not_next_val = True
                continue
            if pix == Colours.TRUE.value:
                if not_next_val:
                    vals.append(False)
                    not_next_val = False
                else:
                    vals.append(True)
            elif pix == Colours.FALSE.value:
                if not_next_val:
                    vals.append(True)
                    not_next_val = False
                else:
                    vals.append(False)
            elif pix == Colours.VAR_FUN_NAME.value:
                ref = self.parse_var_fun_reference()
                if not ref in context:
                    raise PawetInterpreterError(self.current_index, pix, "Variable not found")
                if context[ref][0] == bool and not_next_val:
                    not_next_val = False
                    vals.append(not context[ref][1])
                else:
                    vals.append(context[ref][1])
            elif pix == Colours.STRING_LITERAL.value:
                vals.append(self.parse_string_literal())
            elif pix == Colours.INT_LITERAL.value:
                vals.append(self.parse_int_literal())
            elif pix == Colours.FLOAT_LITERAL.value:
                vals.append(self.parse_float_literal())
            elif pix == Colours.BRACKET_START.value:
                if not_next_val:
                    vals.append(not self.solve_brackets(context, base=False))
                    not_next_val = False
                else:
                    vals.append(self.solve_brackets(context, base=False))
            elif pix == Colours.LOGICAL_AND.value:
                operators.append('&&')
            elif pix == Colours.LOGICAL_OR.value:
                operators.append('||')
            elif pix == Colours.BITWISE_AND.value:
                operators.append('&')
            elif pix == Colours.BITWISE_OR.value:
                operators.append('|')
            elif pix == Colours.BITWISE_SHIFT_LEFT.value:
                operators.append('<<')
            elif pix == Colours.BITWISE_SHIFT_RIGHT.value:
                operators.append('>>')
            elif pix == Colours.ADD.value:
                operators.append('+')
            elif pix == Colours.SUBTRACT.value:
                operators.append('-')
            elif pix == Colours.MULTIPLY.value:
                operators.append('*')
            elif pix == Colours.DIVIDE.value:
                operators.append('/')
            elif pix == Colours.MODULO.value:
                operators.append('%')
            elif pix == Colours.POWER.value:
                operators.append('**')
            elif pix == Colours.GREATER.value:
                operators.append('>')
            elif pix == Colours.GREATER_EQUAL.value:
                operators.append('>=')
            elif pix == Colours.EQUALS.value:
                operators.append('==')
            elif pix == Colours.LESS_EQUAL.value:
                operators.append('<=')
            elif pix == Colours.LESS.value:
                operators.append('<')
            elif pix == Colours.NOT_EQUAL.value:
                operators.append('!=')
            prev_pix = pix
            pix = self.read_next_pixel()
        if len(vals) != len(operators) + 1:
            raise PawetInterpreterError(self.current_index, pix, "Invalid amount of values verses operators")
        final_value = None
        while len(vals) > 1:
            vals[1] = self.__parse_vals_operators(vals[0], vals[1], operators[0])
            vals.pop(0)
            operators.pop(0)
        final_value = vals[0]
        return final_value

    def __parse_vals_operators(self, a, b, operator):
        numbers = [float, int]
        if operator == '&&' and type(a) is bool and type(b) is bool:
            return a and b
        elif operator == '||' and type(a) is bool and type(b) is bool:
            return a or b
        elif operator == '&' and type(a) is int and type(b) is int:
            return a & b
        elif operator == '|' and type(a) is int and type(b) is int:
            return a | b
        elif operator == '<<' and type(a) is int and type(b) is int:
            return a << b
        elif operator == '>>' and type(a) is int and type(b) is int:
            return a >> b
        elif operator == '+':
            return a + b
        elif operator == '-' and type(a) in numbers and type(b) in numbers:
            return a - b
        elif operator == '*' and type(a) in numbers and type(b) in numbers:
            return a * b
        elif operator == '/' and type(a) in numbers and type(b) in numbers:
            if b == 0:
                raise PawetInterpreterError(self.current_index, None, "Cannot divide by zero")
            return a / b
        elif operator == '%' and type(a) in numbers and type(b) in numbers:
            if b == 0:
                raise PawetInterpreterError(self.current_index, None, "Cannot divide by zero")
            return a % b
        elif operator == '**' and type(a) in numbers and type(b) in numbers:
            return a ** b
        elif operator == '<' and type(a) in numbers and type(b) in numbers:
            return a < b
        elif operator == '<=' and type(a) in numbers and type(b) in numbers:
            return a <= b
        elif operator == '>=' and type(a) in numbers and type(b) in numbers:
            return a >= b
        elif operator == '>' and type(a) in numbers and type(b) in numbers:
            return a > b
        elif operator == '==':
            return a == b
        elif operator == '!=':
            return a != b
        else:
            raise PawetInterpreterError(self.current_index, None, "Failed to use operator, check your types")
