from pawet import PawetInterpreter


if __name__ == "__main__":
    pawet = PawetInterpreter("test.png")
    pawet.interpret()
    print(pawet.variables)