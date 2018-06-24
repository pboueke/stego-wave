# [WIP] stego-wave

Simple tool for .wav LSB steganography.

## Usage

### Command Line Parameters

* ```--write```, ```--read```
    * Operation type. One, and only one, of those flags bust be used, either to write a steganographic object or to read from one.

* ```--host=<filepath>```
    * The path to the ```.wav``` file used as a host, i.e., the file containing the hidden data. Must be used for reading from a file or writing to one.

* ```--result=<filepath>```
    * The path to the file containing the result of the operation. For write operations, the stego object and, for read operations, the hidden message.

* ```message=<filepath>```
    * Used only for write operations. The path to the file containing the message you want to hide.

### Example

Writing the contents of ```hello.txt``` to ```stego.wav``` over ```music.wav```:

```
stego-wave --write --host=music.wav --message=hello.txt --resut=stego.wav
```

And to read the message form the result:

```
stego-wave --read --host=stego.wav --result=hello.txt
```

## Setup

If you don't have common-lisp installed and is using linux, follow these steps for a quick setup:

* install ```sbcl```
* install [quicklisp](https://www.quicklisp.org/beta/)
* clone this repository at ```~/.quicklisp/local-projects```

In theory, you should be ready to go. For additional ease of use, install [maid](https://github.com/egoist/maid) and use the ```maidfile.md``` to run development tasks.

## Roadmap

### Header:

- [ ] Detect and handle RIFX files. Currently only working with RIFF
- [ ] Detect bts per sample, so that files with lower bits per sample get more capacity. Currently assuming all files have 16 bits per sample.
- [ ] Detect the exact size of header, so that we can start writing the message exactly after the header is read. Currently assumig headers of 64 bytes.

### General

- [ ] Generalize to work with any RIFF file. Currently assumes only .WAV files.
- [ ] Add more steganographic methods, such as frequency spread,

## References

1. [WAVE Audio File Format](https://www.loc.gov/preservation/digital/formats/fdd/fdd000001.shtml), Library of Congress

2. [WAVE PCM soundfile format, soundfile++](http://soundfile.sapp.org/doc/WaveFormat/): A Soundfile Reading/Writing Library in C++

3. Hiding Data in Wave Files. International Conference in Recent Trends in Information Technology and Computer Science (ICRTITCS - 2012). By Pushpa Aigal, Pramod Vasambekar.