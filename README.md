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

If you have lots of packages and need to boost performance, build the
index manually. If you don't, the index will be built automatically at
runtime.

```sh
$ cask exec servant index
```

Then, start the server:

```sh
$ cask exec servant start
```

Each command described above can change the target directory with the
`--path` option.

```sh
$ cask exec servant index --path /path/to
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
