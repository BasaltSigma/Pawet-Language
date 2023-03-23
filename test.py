from pawet import PawetInterpreter


if __name__ == "__main__":
    pawet = PawetInterpreter("main", True, image_file_path="./tests/function_tests.png")
    pawet.interpret()
    print(pawet.variables)
    print(pawet.functions)