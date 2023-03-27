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
    THEN = 0xfaff3e
    END_THEN = 0x1c1cff
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
    STRUCT_START = 0x4affff
    STRUCT_END = 0xceff96

class PawetBodyBase:

    def __init__(self, parameters: dict, body: list, name: str, parent_scope: dict):
        self.parameters = parameters
        self.body = body
        self.name = name
        self.visible_variables = parent_scope

    def execute(self, parameter_values: list):
        # paramater match error checking
        if len(parameter_values) != len(self.parameters):
            raise PawetInterpreterError(0, 0, f"{self.name}: Parameters supplied do not match function parameter count")
        for index in range(0, len(parameter_values)):
            if type(parameter_values[index]) != list(self.parameters.values())[index]:
                raise PawetInterpreterError(0, 0, f"{self.name}: Value(s) supplied does not match parameter types")

class PawetFunction(PawetBodyBase):

    def __init__(self, parameters: dict, body: list, name: str, parent_scope: dict, return_type: str):
        super(PawetFunction, self).__init__(parameters, body, name, parent_scope)
        self.return_type = return_type

    def execute(self, parameter_values: list):
        super(PawetFunction, self).execute(parameter_values)
        index = 0
        for elem in self.parameters:
            self.visible_variables[elem] = (self.parameters[elem], parameter_values[index])
            index = index + 1
        pawet = PawetInterpreter(self.name, False, body=self.body, parent_scope=self.visible_variables)
        if self.return_type != 'void':
            return pawet.interpret()
        else:
            pawet.interpret()


class PawetIfElseBody(PawetBodyBase):

    def __init__(self, body: list, parent_scope: dict, else_if_conditions = [], else_if_bodies = [], else_body = []):
        super(PawetIfElseBody, self).__init__({'val': bool}, body, "if-statement", parent_scope)
        self.else_if_conditions = else_if_conditions
        self.else_if_bodies = else_if_bodies
        self.else_body = else_body

    def execute(self, parameter_values: list):
        super(PawetIfElseBody, self).execute(parameter_values)
        if parameter_values[0]:
            pawet = PawetInterpreter("if-statement", False, body=self.body, parent_scope=self.visible_variables)
            pawet.interpret()
            return
        if len(self.else_if_conditions) != 0 and len(self.else_if_bodies) != 0:
            if len(self.else_if_conditions) != len(self.else_if_bodies):
                raise PawetInterpreterError(0, 0, "The amount of else if conditions do not match the amount of else if bodies. This should never happen...")
            for index in range(0, len(self.else_if_conditions)):
                if self.else_if_conditions[index]:
                    pawet = PawetInterpreter("then-statement", False, body=self.else_if_bodies[index], parent_scope=self.visible_variables)
                    pawet.interpret()
                    return
        # only the else body to execute now as everything else should have exited if conditions were true
        if len(self.else_body) != 0:
            pawet = PawetInterpreter("else-statement", False, body=self.else_body, parent_scope=self.visible_variables)
            pawet.interpret()

class PawetLoopBody(PawetBodyBase):

    def __init__(self, body: list, parent_scope: dict, condition_body: list):
        super(PawetLoopBody, self).__init__({}, body, "loop", parent_scope)
        self.condition_body = condition_body
    
    def execute(self, parameter_values: list):
        super(PawetLoopBody, self).execute(parameter_values)
        pawet_condition = PawetInterpreter("loop-condition", False, body=self.condition_body, parent_scope=self.visible_variables, is_condition=True)
        pawet_body = PawetInterpreter("loop", False, body=self.body, parent_scope=self.visible_variables)
        while pawet_condition.eval_condition():
            command = pawet_body.interpret()
            pawet_body.current_index = 0
            if type(command) is str and command == "continue-loop":
                pawet_condition.current_index = 0
                continue
            if type(command) is str and command == "break-loop":
                break
            
        

class PawetInterpreter:

    def __init__(self, context_name: str, base: bool, image_file_path: str = "", body: list = [], parent_scope = {}, is_condition=False):
        self.base = base
        if self.base:
            im = Image.open(image_file_path)
            im = im.convert("RGB")
            self.image = [(x[0] << 16) + (x[1] << 8) + x[2] for x in list(im.getdata())]
            im.close()
        else:
            self.image = body
        self.current_index = 0
        self.variables = parent_scope
        self.functions = {}
        self.context_name = context_name
        self.structs = {}
        self.is_condition = is_condition

    def eval_condition(self) -> bool:
        evaluated = False
        pix = self.read_next_pixel()
        if pix == Colours.EQUATION.value:
            evaluated = self.parse_equation(self.variables)
        elif pix == Colours.TRUE.value:
            evaluated = True
        elif pix == Colours.FALSE.value:
            evaluated = False
        elif pix == Colours.VAR_FUN_NAME.value:
            ref, is_func, func_params = self.parse_var_fun_reference()
            if is_func:
                if not ref in self.functions:
                    raise PawetInterpreterError(self.current_index, pix, "Function in loop condition not defined")
                evaluated = self.functions[ref].execute(func_params)
            else:
                if not ref in self.variables:
                    raise PawetInterpreterError(self.current_index, pix, "Variable reference not found")
                evaluated = self.variables[ref][1]
        if not type(evaluated) is bool:
            raise PawetInterpreterError(0, None, "Function condition value not a bool")
        self.current_index = 0
        return evaluated

    def interpret(self):
        while self.current_index < len(self.image):
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
                paw = PawetInterpreter(to_import, True, image_file_path=to_import)
                paw.interpret()
                self.functions.update(paw.functions)
                self.variables.update(paw.variables)
                self.structs.update(paw.structs)
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
                var_ref, is_func, func_params = self.parse_var_fun_reference()
                if is_func:
                    if not var_ref in self.functions:
                        raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                    self.functions[var_ref].execute(func_params)
                else:
                    if not var_ref in self.variables:
                        raise PawetInterpreterError(self.current_index, pix, f"{self.context_name} Variable not initialized")
                pix = self.read_next_pixel()
                was_modified = False
                if pix == Colours.INCREMENT_VAL.value:
                    if self.variables[var_ref][0] not in [int, float]:
                        raise PawetInterpreterError(self.current_index, pix, "Not an incrementable type")
                    self.variables[var_ref] = (self.variables[var_ref][0], self.variables[var_ref][1] + 1)
                    was_modified = True
                elif pix == Colours.DECREMENT_VAL.value:
                    if self.variables[var_ref][0] not in [int, float]:
                        raise PawetInterpreterError(self.current_index, pix, "Not a decrementable type")
                    self.variables[var_ref] = (self.variables[var_ref][0], self.variables[var_ref][1] - 1)
                    was_modified = True
                if was_modified:
                    pix = self.read_next_pixel()
                if pix != Colours.STATEMENT_END.value:
                    raise PawetInterpreterError(self.current_index, pix, "Statement not closed")
                continue
            elif pix == Colours.FUNCTION.value:
                if not self.base:
                    raise PawetInterpreterError(self.current_index, pix, "Cannot have functions within functions")
                name, signature, return_type, body = self.parse_function()
                self.functions[name] = PawetFunction(signature, body, name, dict(self.variables), return_type)
                continue
            elif pix == Colours.RETURN.value:
                pix = self.read_next_pixel()
                val = None
                if pix == Colours.STATEMENT_END.value:
                    return
                if pix == Colours.EQUATION.value:
                    val = self.parse_equation(self.variables)
                elif pix == Colours.TRUE.value:
                    val = True
                elif pix == Colours.FALSE.value:
                    val = False
                elif pix == Colours.INT_LITERAL.value:
                    val = self.parse_int_literal()
                elif pix == Colours.FLOAT_LITERAL.value:
                    val = self.parse_float_literal()
                elif pix == Colours.STRING_LITERAL.value:
                    val = self.parse_string_literal()
                elif pix == Colours.VAR_FUN_NAME.value:
                    val, is_function, fun_name, params = self.parse_var_fun_reference()
                    if is_function:
                        if not fun_name in self.functions:
                            raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                        return self.functions[fun_name].execute(params)
                return val
            elif pix == Colours.IF.value:
                if_statement, condition = self.parse_if_then_else()
                if_statement.execute([condition])
                continue
            elif pix == Colours.LOOP.value:
                loop: PawetLoopBody = self.parse_loop()
                loop.execute([])
                continue
            elif self.context_name == "loop" and pix == Colours.CONTINUE.value:
                return "continue-loop"
            elif self.context_name == "loop" and pix == Colours.BREAK.value:
                return "break-loop"
            else:
                raise PawetInterpreterError(self.current_index, pix, "Unexpected pixel, Did you maybe forget to escape?")
    
    def parse_loop(self):
        loop_condition = []
        loop_body = []
        open_bracket_passed = False
        closed_bracket_passed = False
        while True:
            pix = self.read_next_pixel()
            if pix == Colours.SKIP.value:
                continue
            elif pix == Colours.END_LOOP.value:
                break
            elif pix == Colours.BRACKET_START.value:
                open_bracket_passed = True
                continue
            elif pix == Colours.BRACKET_END.value and open_bracket_passed:
                closed_bracket_passed = True
                continue
            if open_bracket_passed and not closed_bracket_passed:
                loop_condition.append(pix)
            if open_bracket_passed and closed_bracket_passed:
                loop_body.append(pix)
        return PawetLoopBody(loop_body, self.variables, loop_condition)

    def parse_if_then_else(self):
        bracket_started = False
        bracket_ended = False
        if_condition = False
        if_body = []
        while True:
            pix = self.read_next_pixel()
            if pix == Colours.SKIP.value:
                continue
            elif pix == Colours.BRACKET_START.value:
                bracket_started = True
                continue
            elif pix == Colours.END_IF.value:
                if not bracket_started:
                    raise PawetInterpreterError(self.current_index, pix, "Invalid syntax for if statement")
                break
            elif pix == Colours.EQUATION.value:
                if_condition = self.parse_equation(self.variables)
                continue
            elif pix == Colours.VAR_FUN_NAME.value:
                name, is_func, func_params = self.parse_var_fun_reference()
                if is_func:
                    if not name in self.functions:
                        raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                    if_condition = self.functions[name].execute(func_params)
                else:
                    if not name in self.variables:
                        raise PawetInterpreterError(self.current_index, pix, "Variable not defined")
                    if_condition = self.variables[name][1]
                continue
            elif pix == Colours.TRUE.value:
                if_condition = True
                continue
            elif pix == Colours.FALSE.value:
                if_condition = False
                continue
            elif pix == Colours.BRACKET_END.value and bracket_started:
                bracket_ended = True
                continue
            if bracket_ended:
                if_body.append(pix)
        if not type(if_condition) is bool:
            raise PawetInterpreterError(self.current_index, pix, "Value in condition not a bool")
        while self.peek_next_pixel() == Colours.SKIP.value:
            self.read_next_pixel()
        then_bodies = []
        then_conditions = []
        while self.peek_next_pixel() == Colours.THEN.value:
            bracket_ended = False
            bracket_started = False
            self.read_next_pixel()  # skip the THEN pixel value
            this_body = []
            this_condition = False
            while True:
                pix = self.read_next_pixel()
                if pix == Colours.SKIP.value:
                    continue
                elif pix == Colours.END_THEN.value:
                    break
                elif pix == Colours.BRACKET_START.value:
                    bracket_started = True
                    continue
                elif pix == Colours.BRACKET_END.value and bracket_started:
                    bracket_ended = True
                    continue
                elif pix == Colours.TRUE.value:
                    this_condition = True
                    continue
                elif pix == Colours.FALSE.value:
                    this_condition = False
                    continue
                elif pix == Colours.EQUATION.value:
                    this_condition = self.parse_equation(self.variables)
                    continue
                elif pix == Colours.VAR_FUN_NAME.value:
                    name, is_func, func_params = self.parse_var_fun_reference()
                    if is_func:
                        if not name in self.functions:
                            raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                        this_condition = self.functions[name].execute(func_params)
                    else:
                        if not name in self.variables:
                            raise PawetInterpreterError(self.current_index, pix, "Variable not defined")
                        this_condition = self.variables[name][1]
                    continue
                if bracket_ended:
                    this_body.append(pix)
            then_bodies.append(this_body)
            if not type(this_condition) is bool:
                raise PawetInterpreterError("Then-statement input is not a bool")
            then_conditions.append(this_condition)
            while self.peek_next_pixel() == Colours.SKIP.value:
                self.read_next_pixel()
        else_body = []
        if self.peek_next_pixel() == Colours.ELSE.value:
            self.read_next_pixel() # skip the ELSE pixel
            while True:
                pix = self.read_next_pixel()
                if pix == Colours.SKIP.value:
                    continue
                if pix == Colours.END_ELSE.value:
                    break
                else_body.append(pix)
        func = PawetIfElseBody(if_body, dict(self.variables), then_conditions, then_bodies, else_body)
        return func, if_condition

    def parse_struct_def(self):
        struct_name: str = ""
        struct_params = {}
        while True:
            pix = self.read_next_pixel()
            if pix == Colours.SKIP.value:
                continue
            elif pix == Colours.STRUCT_END.value:
                break
            
    def parse_function_call(self):
        param_vals = []
        current_val = None
        while True:
            pix = self.read_next_pixel()
            if pix == Colours.SKIP.value:
                continue
            if pix == Colours.BRACKET_END.value:
                if current_val != None:
                    param_vals.append(current_val)
                break
            if pix == Colours.INT_LITERAL.value:
                current_val = self.parse_int_literal()
                continue
            elif pix == Colours.FLOAT_LITERAL.value:
                current_val = self.parse_float_literal()
                continue
            elif pix == Colours.TRUE.value:
                current_val = True
                continue
            elif pix == Colours.FALSE.value:
                current_val = False
                continue
            elif pix == Colours.STRING_LITERAL.value:
                current_val = self.parse_string_literal()
                continue
            elif pix == Colours.EQUATION.value:
                current_val = self.parse_equation()
                continue
            elif pix == Colours.VAR_FUN_NAME.value:
                name, is_function, func_params = self.parse_var_fun_reference()
                if is_function:
                    if not func_params in self.functions:
                        raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                    current_val = self.functions[name].execute(func_params)
                else:
                    if not name in self.variables:
                        raise PawetInterpreterError(self.current_index, pix, "Variable not defined")
                    current_val = self.variables[name][1]
                continue
            if current_val != None and pix == Colours.COMMA.value:
                param_vals.append(current_val)
                current_val = None
        return param_vals


    def parse_function(self):
        name: str = ""
        return_type: str = ""
        parameters = {}
        body = []
        past_return_type = False
        past_func_name = False
        while True:
            pix = self.read_next_pixel()
            if pix == Colours.SKIP.value:
                continue
            elif pix == Colours.END_FUNCTION.value:
                break
            elif pix == Colours.VOID_TYPE.value:
                return_type = "void"
                past_return_type = True
                continue
            elif pix == Colours.STRING_TYPE.value:
                return_type = "str"
                past_return_type = True
                continue
            elif pix == Colours.FLOAT_TYPE.value:
                return_type = "float"
                past_return_type = True
                continue
            elif pix == Colours.BOOL_TYPE.value:
                return_type = "bool"
                past_return_type = True
                continue
            elif pix == Colours.INT_TYPE.value:
                return_type = "int"
                past_return_type = True
                continue
            elif pix == Colours.VAR_FUN_NAME.value and past_return_type and not past_func_name:
                past_func_name = True
                name, _, _ = self.parse_var_fun_reference(is_function_def=True)
                continue
            elif pix == Colours.BRACKET_START.value:
                parameters = self.parse_function_signature()
                continue
            body.append(pix)
        if name == "":
            raise PawetInterpreterError(self.current_index, None, "Function does not have a name")
        if return_type == "":
            raise PawetInterpreterError(self.current_index, None, f"{name} function has no return type (if none is needed, use void)")
        return name, parameters, return_type, body
        
    def parse_function_signature(self):
        signature = {}
        current_type = None
        param_name: str = ""
        while True:
            pix = self.read_next_pixel()
            if pix == Colours.SKIP.value:
                continue
            if pix == Colours.BRACKET_END.value:
                if current_type != None and param_name != "":
                    if param_name in signature:
                        raise PawetInterpreterError(self.current_index, pix, "Function parameter already declared")
                    signature[param_name] = current_type
                break
            if pix == Colours.STRING_TYPE.value:
                current_type = str
                continue
            elif pix == Colours.BOOL_TYPE.value:
                current_type = bool
                continue
            elif pix == Colours.INT_TYPE.value:
                current_type = int
                continue
            elif pix == Colours.FLOAT_TYPE.value:
                current_type = float
                continue
            elif pix == Colours.COMMA.value and current_type != None and param_name != "":
                if param_name in signature:
                    raise PawetInterpreterError(self.current_index, pix, "Function parameter already declared")
                signature[param_name] = current_type
                param_name = ""
                current_type = None
                continue
            elif pix == Colours.VAR_FUN_NAME.value:
                param_name, _, _ = self.parse_var_fun_reference()
            else:
                raise PawetInterpreterError(self.current_index, pix, "Invalid function signature decleration")
        return signature

    def read_next_pixel(self) -> int:
        if self.current_index >= len(self.image):
            raise PawetInterpreterError(self.current_index, None, "End of image reached before end of program")
        val = self.image[self.current_index]
        self.current_index = self.current_index + 1
        return val
    
    def peek_next_pixel(self) -> int:
        if self.current_index >= len(self.image):
            raise PawetInterpreterError(self.current_index, None, "End of image reached before end of program")
        return self.image[self.current_index]

    def parse_int_literal(self) -> int:
        pix = self.read_next_pixel()
        prev_pix = None
        rgb = []
        negate = False
        while True:
            if pix == Colours.SUBTRACT.value:
                negate = True
                prev_pix = pix
                pix = self.read_next_pixel()
                continue
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
        if negate:
            return -val
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
        for i in range(0, len(whole)):
            first = first + (whole[i] << ((len(whole) - i - 1) * 24))
        second: str = self.parse_string(decimal)
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
            if pix == Colours.VAR_FUN_NAME.value and not past_assignment:
                var_name, _, _ = self.parse_var_fun_reference()
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
                    ref_var, is_func, func_params = self.parse_var_fun_reference()
                    if is_func:
                        if not ref_var in self.functions:
                            raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                        val = self.functions[ref_var].execute(func_params)
                        ref_var = None
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
        negate_value = False
        while not pix == Colours.STATEMENT_END.value:
            if pix == Colours.SKIP.value:
                pix = self.read_next_pixel()
                continue
            if pix == Colours.VAR_FUN_NAME.value and not past_assignment:
                assignment_name, _, _ = self.parse_var_fun_reference()
                pix = self.read_next_pixel()
                continue
            if pix == Colours.ASSIGNMENT.value:
                past_assignment = True
                pix = self.read_next_pixel()
                continue
            if past_assignment:
                if pix == Colours.SUBTRACT.value:
                    negate_value = True
                    continue
                if pix == Colours.INT_LITERAL.value:
                    literal = self.parse_int_literal()
                elif pix == Colours.VAR_FUN_NAME.value:
                    target_ref, is_func, func_params = self.parse_var_fun_reference()
                    if is_func:
                        if not target_ref in self.functions:
                            raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                        literal = self.functions[target_ref].execute(func_params)
                        target_ref = None
                elif pix == Colours.EQUATION.value:
                    literal = self.parse_equation(context)
            pix = self.read_next_pixel()
        if negate_value:
            literal = -literal
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
        negate_value = False
        while not pix == Colours.STATEMENT_END.value:
            if pix == Colours.SKIP.value:
                pix = self.read_next_pixel()
                continue
            if pix == Colours.VAR_FUN_NAME.value and not past_assignment:
                assignment_name, _, _ = self.parse_var_fun_reference()
                pix = self.read_next_pixel()
                continue
            if pix == Colours.ASSIGNMENT.value:
                past_assignment = True
                pix = self.read_next_pixel()
                continue
            if past_assignment:
                if pix == Colours.SUBTRACT.value:
                    negate_value = True
                    continue
                if pix == Colours.FLOAT_LITERAL.value:
                    literal = self.parse_float_literal()
                elif pix == Colours.VAR_FUN_NAME.value:
                    target_ref, is_func, func_params = self.parse_var_fun_reference()
                    if is_func:
                        if not target_ref in self.functions:
                            raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                        literal = self.functions[target_ref].execute(func_params)
                        target_ref = None
                elif pix == Colours.EQUATION.value:
                    literal = self.parse_equation(context)
            pix = self.read_next_pixel()
        if negate_value:
            literal = -literal
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
                assignment_name, _, _ = self.parse_var_fun_reference()
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
                    target_ref, is_func, func_params = self.parse_var_fun_reference()
                    if is_func:
                        if not target_ref in self.functions:
                            raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                        literal = self.functions[target_ref].execute(func_params)
                        target_ref = None
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

    def parse_var_fun_reference(self, is_function_def=False):
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
        name = self.parse_string(vals_rgb)
        is_function_call = False
        param_vals = None
        if self.peek_next_pixel() == Colours.BRACKET_START.value and not is_function_def:
            is_function_call = True
            param_vals = self.parse_function_call()
        return name, is_function_call, param_vals
    
    def parse_print(self, context):
        following = self.read_next_pixel()
        if following == Colours.VAR_FUN_NAME.value:
            ref, is_func, func_params = self.parse_var_fun_reference()
            if is_func:
                if not ref in self.functions:
                    raise PawetInterpreterError(self.current_index, following, "Function is undefined")
                print(self.functions[ref].execute(func_params))
            else:
                if not ref in context:
                    raise PawetInterpreterError(self.current_index, following, f"{ref}, Variable is not defined")
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
            if (self.base or self.is_condition) and pix == Colours.EQUATION.value and prev_pix != Colours.ESCAPE_NEXT.value:
                break
            if not self.base and pix == Colours.BRACKET_END.value and prev_pix != Colours.ESCAPE_NEXT.value:
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
                ref, is_func, func_params = self.parse_var_fun_reference()
                if is_func:
                    if not ref in self.functions:
                        raise PawetInterpreterError(self.current_index, pix, "Function not defined")
                    func_ret = self.functions[ref].execute(func_params)
                    if type(func_ret) is bool and not_next_val:
                        vals.append(not func_ret)
                    else:
                        vals.append(func_ret)
                else:
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
