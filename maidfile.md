## build

Build app using ASDF as seen [here](https://lispcookbook.github.io/cl-cookbook/scripting.html)

```bash
sbcl --load stego-wave.asd \
    	--eval '(ql:quickload :stego-wave)' \
		--eval '(asdf:make :stego-wave)' \
		--eval '(quit)'
```

## run

Runs the compiled binary. I have not yet found how to use a relative path for the generated binary.

Run task `build` before this

```bash
/home/jawa/.cache/common-lisp/sbcl-1.3.1.debian-linux-x64/home/jawa/quicklisp/local-projects/stego-wave/stegowave.app "${@:1}"
```

## echo

Just a syntax example for future reference...

```bash
echo "${@:1}"
```