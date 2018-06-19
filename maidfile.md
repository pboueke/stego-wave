## build

Build app using ASDF as seen [here](https://lispcookbook.github.io/cl-cookbook/scripting.html)

```bash
sbcl    --non-interactive \
		--load stego-wave.asd \
    	--eval '(ql:quickload :stego-wave)' \
		--eval '(asdf:make :stego-wave)' \
		--eval '(quit)'
```

## run

Runs the compiled binary.

```bash
~/.quicklisp/local-projects/stego-wave/stegowave.app "${@:1}"
```

## bar

Build and run the compiled binary.

Run task `build` before this

```bash
~/.quicklisp/local-projects/stego-wave/stegowave.app "${@:1}"
```

## bar:write

Run task `build` before this

```bash
~/.quicklisp/local-projects/stego-wave/stegowave.app --host=sounds/smashingbaby.wav --message=sounds/test.txt --result=sounds/testresult.wav --write
```