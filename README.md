# [WIP] stego-wave

Simple tool for .wav LSB steganography.

## Usage

* ```--write```, ```--read```
    * Operation type. One, and only one, of those flags bust be used, either to write a steganographic object or to read from one.

* ```host```
    * The path to the ```.wav``` file used as a host, i.e., the file containing the hidden data. Must be used for reading from a file or writing to one.

* ```result```
    * The path to the file containing the result of the operation. For write operations, the stego object and, for read operations, the hidden message.

* ```message```
    * Used only for write operations. The path to the file containing the message you want to hide.

### Setup

If you don't have common-lisp installed and is using linux, follow these steps for a quick setup:

* install ```sbcl```
* install [quicklisp](https://www.quicklisp.org/beta/)
* clone this repository at ```~/.quicklisp/local-projects```

In theory, you should be ready to go. For additional ease of use, install [maid](https://github.com/egoist/maid) and use the ```maidfile.md``` to run development tasks.