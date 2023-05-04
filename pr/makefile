all: test

clean:
	rm -rf .benchmarks .hypothesis .pytest_cache __pycache__ myrustlib.so

test: myrustlib.so
	pytest --benchmark-verbose doubles.py

myrustlib.so:
	cd pyext-myrustlib/ && cargo build --release && cd ..
	cp pyext-myrustlib/target/release/libmyrustlib.dylib myrustlib.so

.PHONY: all clean test
