build: src/Lexer.hs src/Parser.hs
	@echo "Building compiler..."
	cd src && ghc Main.hs IR.hs IRTranslator.hs MIPSGenerator.hs -o Main

src/Lexer.hs: src/Lexer.x
	@echo "Generating lexer..."
	cd src && alex Lexer.x -o Lexer.hs

src/Parser.hs: src/Parser.y
	@echo "Generating parser..."
	cd src && happy Parser.y -o Parser.hs

clean:
	@echo "Cleaning generated files..."
	cd src && rm -f *.hi *.o Lexer.hs Parser.hs Parser.info
	cd src && rm -f IR.hi IR.o IRTranslator.hi IRTranslator.o MIPSGenerator.hi MIPSGenerator.o
	rm -f Main output.asm

run: build
	@echo "Running all test examples..."
	@echo "=========================="
	@echo "Testing example1.kt:"
	@echo "------------------------"
	cd src && ./Main ./examples/example1.kt || true
	@echo "------------------------"
	@echo "Testing example2.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example2.kt || true
	@echo "------------------------"
	@echo "Testing example3.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example3.kt || true
	@echo "------------------------"
	@echo "Testing example4.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example4.kt || true
	@echo "------------------------"
	@echo "Testing example5.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example5.kt || true
	@echo "------------------------"
	@echo "Testing example6.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example6.kt || true
	@echo "------------------------"
	@echo "Testing example7.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example7.kt || true
	@echo "------------------------"
	@echo "Testing example8.kt:"
	@echo "------------------------"
	cd src && ./Main ../examples/example8.kt || true
	@echo "------------------------"
	@echo "All tests completed."

.PHONY: build clean run