
# Cashflow

Cashflow is simple command line tool personal finance application.

## Installation

Only two dependencies beside `base` are `time` and `parsec`, and they are 
both part of `haskell-platform`, so if you have `haskell-platform` installed,
you should be good to go.

Check out source

    > git clone https://github.com/StarvingMarvin/cashflow.git

Build

    > cd cashflow
    > ./Setup.lhs configure
    > ./Setup.lhs build
    > ./Setup.lhs install

## Usage

Program takes input file as parameter

    > cashflow input-file
    
if none given, stdin is read.

## Input format

Input is a very simple, human readable file, that closely matches the way
I was writing things down anyway for 'manual analysis'.

Input file is split into five sections: 

### Expenses

### Monthly expenses

### Debt

### Income

### Assets

## Limitations

Cashflow is designed not for absolute precision, but for answering questions 
like 'is this kind of spending even realistic?' To keep overhead at minimum, 
numerous simplifying assumptions are introduced:

* Month is the finest time resolution available, and it is also the coarsest
* If is recurring on period different than month, you need to sum it 
/ average it yourself
* All numbers and calculations are integer only


## How it relates to ledger?

[Ledger](http://ledger-cli.org/) is an awesome tool that helps you track 
your finances and precisely track where your money is going. Once you figure
out where do you spend money and what are you left with, Cashflow can help
you do a back of a napkin sort of calculation to help you plan your future
expenses.


## How stable, tested, etc this software is?

This is my first Haskell project, and it's made in a week or so, draw
your own conclusions.

