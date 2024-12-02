build: src/Lexer.hs src/Parser.hs
	@echo "Building compiler..."
	cd src && ghc Main.hs -o Main

src/Lexer.hs: src/Lexer.x
	@echo "Generating lexer..."
	cd src && alex Lexer.x -o Lexer.hs

src/Parser.hs: src/Parser.y
	@echo "Generating parser..."
	cd src && happy Parser.y -o Parser.hs

clean:
	@echo "Cleaning generated files..."
	cd src && rm -f *.hi *.o Lexer.hs Parser.hs Parser.info
	rm -f Main

run: build
	@echo "\nRunning all test examples..."
	@echo "=========================="
	@echo "\nTesting example1.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example1.kt || true
	@echo "------------------------"
	@echo "\nTesting example2.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example2.kt || true
	@echo "------------------------"
	@echo "\nTesting example3.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example3.kt || true
	@echo "------------------------"
	@echo "\nTesting example4.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example4.kt || true
	@echo "------------------------"
	@echo "\nTesting example5.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example5.kt || true
	@echo "------------------------"
	@echo "\nAll tests completed.\n"


.PHONY: build clean run