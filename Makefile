.PHONY: install
install:
	@echo "\n🎄  AOC 2024 | day $(day)\n"
	@clj -M -m day$(day).solution
	@echo ""