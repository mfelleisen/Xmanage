# Xmanage

a benchmark 

## Usage

Tells all: 

```
xmanage -h
```

## Files 

- "xmanage" is the script. 

It accepts a small number of on-line commands to manage a checking
account. The purpose of the script is to recognize to the desired
command from the commandline; if applicable, read the account file;
and to dispatch to the action. The latter returns an effect callback,
which is run under protection of a suitable exception handler.

- ".sample.act" is a sample account. 

## Files (private)

- actions.rkt implements the actions on the internal representation of
  accounts. Each action returns an account and an effect. 
  
- data.rkt implements the internal data representation of accounts. 
  (The plan was to batch transactions, but I never implemented this.)

- file-io.rkt implements the parsing of files into accounts and
  writing of accounts to files. 

- date.rkt a stupid rendering of dates 

## Unit Tests 

All files come with unit tests that achieve a high degree of
expression coverage. 

## Contracts 

Most modules come with reasonably simple contracts, often just
type-like, sometimes a bit more. 




