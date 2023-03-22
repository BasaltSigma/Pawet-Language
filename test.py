from pawet import PawetInterpreter


if __name__ == "__main__":
    pawet = PawetInterpreter("example.png")
    pawet.interpret()
    print(pawet.variables)