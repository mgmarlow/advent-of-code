# Advent of Code 2022

[Advent of code 2022](https://adventofcode.com/) with Common lisp.

## Install

Install [SBCL](http://www.sbcl.org/):

```
brew install sbcl
```

Install [Quicklisp](https://www.quicklisp.org/beta/):

```
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
     --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
     --eval '(ql:add-to-init-file)' \
     --quit
```

## Run

Open up a file in Emacs and run [`M-x sly-eval-buffer`](https://github.com/joaotavora/sly).

