# zauber

## Overview
A reimagining of `file` and `libmagic` in Rust.
NOTE: Nothing of substance here yet!

`file`/`libmagic` can predict "file type" given an input bytestring.
However:
- They are C programs that parse complicated inputs according to user-specified rules, and have had crashes, undefined behavior, and security issues in the past
- They use a classification rules DSL that is very complicated and badly documented
- Their predictions are sometimes wrong and frequently non-specific
- They are slow to execute, executing hundreds or thousands of tests to predict the type of a single input; they process on the order of 10s of MiB/s
- They are implemented using a naive interpreter that does no optimization

The big ideas with Zauber:
- It will be written in safe Rust, avoiding the hazards of implementing parsing logic in C
- It will be able to use existing `magic` rules for classification
- It will compile rules to a rule-matching intermediate representation
- It will apply optimizations to the intermediate representation
- It will have an efficient runtime that can do pattern matching very quickly, building on top of [vectorscan-rs](https://github.com/bradlarsen/vectorscan-rs)


## License
This project is dual-licensed under either the Apache License, Version 2.0, or the MIT License, at your choice.
Any contribution you make to this project will be licensed under the same terms.
