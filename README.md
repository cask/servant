# Emacs Servant

Emacs Servant is an ELPA server written in Emacs Lisp.

## Installation

Add `servant` to your [Cask](https://github.com/rejeep/servant.el) file:

```lisp
(source melpa)

(depends-on "servant")
```

## Usage

Start by initializing Servant for the project.

```sh
$ cask exec servant init
```

This will create a directory called `servant`. To serve packages,
simply add `.el` or `.tar` files to the `servant/packages` directory.

Before starting the server, the index needs to be built:

```sh
$ cask exec servant index
```

Then, start the server:

```sh
$ cask exec servant start
```

This can be done in the same command:

```sh
$ cask exec servant start --index
```

For more information:

```sh
$ cask exec servant help
```

## Contribution

Contribution is much welcome!

Install [cask](https://github.com/rejeep/cask) if you haven't
already, then:

    $ cd /path/to/servant.el
    $ cask

Run all tests with:

    $ make
