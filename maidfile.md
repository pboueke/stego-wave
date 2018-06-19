## build

Build app using ASDF as seen [here](https://lispcookbook.github.io/cl-cookbook/scripting.html)

```bash
sbcl --load stego-wave.asd \
    	--eval '(ql:quickload :stego-wave)' \
		--eval '(asdf:make :stego-wave)' \
		--eval '(quit)'
```

## run

Runs the compiled binary.

Run task `build` before this

```bash
~/.quicklisp/local-projects/stego-wave/stegowave.app "${@:1}"
```