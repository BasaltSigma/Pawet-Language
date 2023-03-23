from pawet import PawetInterpreter


if __name__ == "__main__":
    pawet = PawetInterpreter("main", True, image_file_path="./tests/if_tests.png")
    pawet.interpret()
    print(f"Variables: {pawet.variables}")
    print(f"Functions: {pawet.functions}")
    print(f"Structs: {pawet.structs}")