PACKAGE_MANAGER = cargo
RUN_COMMAND = run
BUILD_COMMAND = build
TEST_COMMAND = test
FMT_COMMAND = fmt
CLIPPY_COMMAND = clippy

# Compile example and run
SRC_FILE = source.dn
OUTPUT_FILE = out
DEEN_ARGS =
CODE_EDITOR = nvim

# Compile to binary/llvm
run:
	$(CODE_EDITOR) $(SRC_FILE)
	$(PACKAGE_MANAGER) $(RUN_COMMAND) -- $(SRC_FILE) $(OUTPUT_FILE) $(DEEN_ARGS)

llvm:
	$(CODE_EDITOR) $(SRC_FILE)
	$(PACKAGE_MANAGER) $(RUN_COMMAND) -- $(SRC_FILE) $(OUTPUT_FILE) $(DEEN_ARGS) --llvm

# Release build
build:
	$(PACKAGE_MANAGER) $(BUILD_COMMAND) --release

# Unit tests
TEST_ARGS = -- --show-output

test:
	$(PACKAGE_MANAGER) $(TEST_COMMAND) $(TEST_ARGS)

# Formatting
FMT_ARGS = --emit=files

fmt:
	$(PACKAGE_MANAGER) $(FMT_COMMAND) -- $(FMT_ARGS)

# Clippy
CLIPPY_ARGS = --fix --allow-dirty -- -D warnings

clippy:
	$(PACKAGE_MANAGER) $(CLIPPY_COMMAND) $(CLIPPY_ARGS)

# With backtrace
backtrace:
	$(CODE_EDITOR) $(SRC_FILE)
	RUST_BACKTRACE=1 $(PACKAGE_MANAGER) $(RUN_COMMAND) -- $(SRC_FILE) $(OUTPUT_FILE) $(DEEN_ARGS)

clean:
	rm -f $(OUTPUT_FILE) $(OUTPUT_FILE).ll

docker-build:
	sudo docker-compose build

docker:
	$(CODE_EDITOR) $(SRC_FILE)
	sudo docker-compose run --rm compiler
