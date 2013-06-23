
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

Records are line oriented and most fields are separated by 'whitespaces' 
which are any combination of spaces, tabs and comas. Only exception is 
description that is separated with column.

Input file is split into five sections: 

### Expenses

Expenses come after `[expenses]` heading and has a format:

_description_":" _amount_ [["~"] _month_]

Examples:

    [expenses]
    # Summer vacation is planed and exact time is known
    # as soon as July passes, it will not be taken in account 
    # when calculating estimates
    Summer vacation:	 2000 Jul
    
    
    # Car will probably be bought some time by the end of May
    # for the purpose of estimates expense is split equally from current 
    # month to May. When May passes, full amount is taken as expense.
    # When car is finally bought, you will either remove it from list, or
    # set exact month so that it stops being taken into account.
    car:		 6000 ~May
    
    # After some poking around the Internet, testing couple of devices etc.
    # you will go out and actually by a stereo, but no exact date is known 
    # or planed. For the purpose of calculation it behaves same as ~Dec.
    
    Stereo:		 1500
    
    

Tilda means approximately and those records, for purpose of estimation are

### Monthly expenses

_description_":" _amount_

example:

   [monthly expenses]
   Rent:		800

### Debt

_description_":" _amount_ _month_ _n-of-payments_

example:

    [debt]
    (bank x visa)
    item 1:		500 Apr 10
    item 2:		700 Jun 12
    
    (bank y master)
    item 3:		300 Mar 3

Similar to monthly expense, except it has start (month) and end 
(month + n-of-payments). You can group by creditor as shown in example.


### Income

_description_":" _amount_

Monthly income

### Assets

_description_":" _amount_


## Limitations

Cashflow is designed not for absolute precision, but for answering questions 
like 'is this kind of spending realistic?' To keep overhead at minimum, 
numerous simplifying assumptions are introduced:

* Month is the finest time resolution available, and it is also the coarsest
* If is recurring on period different than month, you need to sum it 
  or average it yourself
* All numbers and calculations are integer only
* There is only one currency


## TODO

I would like to add some sort of "transaction" feature for situations like "I 
plan to by something around August, but I'll by it on credit card therefore
turning it into debt", or "I am putting $X monthly on my savings account"

There is also no notion of one-of income. I could probably make it by adding 
optional month on asset record, or something...

## How it relates to ledger?

[Ledger](http://ledger-cli.org/) is an awesome tool that helps you track 
your finances and precisely track where your money is going. Once you figure
out where do you spend money and what are you left with, Cashflow can help
you do a back of a napkin sort of calculation to help you plan your future
expenses.


## How stable, tested, etc this software is?

This is my first Haskell project, and it's made in a week or so, draw
your own conclusions.

