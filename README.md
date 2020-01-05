# HAL

Hal is a scheme interpreter



# Building with make
```
make
```



# Usage


## REPL

The REPL is an interactive interpreter.
You can launch it with `-i` flag:
```
./hal -i
```


## Files

The hal interpreter can also interpret scheme files.

```
./hal [filenames]
```

Hal allow you to launch REPL after or before interpreting files and keep the context between files and REPL's inputs.

```
./hal [filenames] -i
```
or
```
./hal -i [filenames]
```

## Stdlib

You can load hal stdlib by setting `HAL_STD` environment variable to the path of stdlib folder.
Only files with `.scm, .sps, .sls, .sld, .lisp` extensions will be interpreted in this folder.



# Features

## Types

<table style="width: 100%">
    <tr>
        <th>Type</th>
        <th>Description</th>
        <th>Example</th>
        <th>Note</th>
    </tr>
    <tr>
        <td>number</td>
        <td>An arbitrary precision integer</td>
        <td>42</td>
        <td>In next versions number will fit floats and rationals</td>
    </tr>
    <tr>
        <td>char</td>
        <td>A Char is a single unicode character</td>
        <td>#\a</td>
        <td></td>
    </tr>
    <tr>
        <td>boolean</td>
        <td>A boolean can be equal to #t (true) or #f (false)</td>
        <td>#t</td>
        <td></td>
    </tr>
    <tr>
        <td>string</td>
        <td>A type which represent a text</td>
        <td>"hello world"</td>
        <td>In next versions string will be a list of chars</td>
    </tr>
    <tr>
        <td>pair</td>
        <td>A pair is a type that can hold 2 elements of any types</td>
        <td>'(6 . 7)</td>
        <td>An explicit pair should be quoted</td>
    </tr>
    <tr>
        <td>list</td>
        <td>A list is a linked list made of pairs</td>
        <td>'(6 7 #\b "hello")</td>
        <td>An explicit list should be quoted</td>
    </tr>
    <tr>
        <td>symbol</td>
        <td>A symbol is an atomic value that represented like an identifier preceded with `'`</td>
        <td>'hello</td>
        <td></td>
    </tr>
    <tr>
        <td>procedure</td>
        <td>A procedure is a function</td>
        <td>(sort '(3 2 1))</td>
        <td>A procedure should be apply with `(..)` to be executed</td>
    </tr>
    <tr>
        <td>void</td>
        <td>Void is an empty type</td>
        <td></td>
        <td></td>
    </tr>
</table>


## Statements

<table style="width: 100%">
    <tr>
        <th>Name</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>cond</td>
        <td>A cond statement take lists, if an expression of a list is false cond jump to the next list</td>
    </tr>
    <tr>
        <td>else</td>
        <td>Else can be used in cond statement. Else is the last cond clause executed.</td>
    </tr>
    <tr>
        <td>=></td>
        <td>`=>` can be used in cond statement. The arrow apply procedure on the right side on expression on the left side.</td>
    </tr>
    <tr>
        <td>if</td>
        <td>If work like standard if statement in other languages</td>
    </tr>
    <tr>
        <td>and</td>
        <td>And take many expressions. If one expression is false `and` return false otherwise return the last argument.</td>
    </tr>
    <tr>
        <td>or</td>
        <td>Or take many expressions. Return the first not false expression.</td>
    </tr>
</table>


## Procedures

### Primitives

<table>
    <tr>
        <th>Name</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>(<b>apply</b> procedure [exprs])</td>
        <td>Apply a procedure on list of exprs as arguments</td>
    </tr>
    <tr>
        <td>(<b>eval</b> expr)</td>
        <td>Evaluate an expression</td>
    </tr>
    <tr>
        <td>(<b>+</b> ...numbers)</td>
        <td>Return sum of all arguments</td>
    </tr>
    <tr>
        <td>(<b>-</b> ...numbers)</td>
        <td>Return negative sum of all arguments</td>
    </tr>
    <tr>
        <td>(<b>*</b> ...numbers)</td>
        <td>Return product of all arguments</td>
    </tr>
    <tr>
        <td>(<b>div</b> n1 n2)</td>
        <td>Divide n1 by n2</td>
    </tr>
    <tr>
        <td>(<b>mod</b> n1 n2)</td>
        <td>Return n1 modulo n2</td>
    </tr>
    <tr>
        <td>(<b><</b> ...numbers)</td>
        <td>Return true if arguments are strictly increasing</td>
    </tr>
    <tr>
        <td>(<b>></b> ...numbers)</td>
        <td>Return true if arguments are strictly decreasing</td>
    </tr>
    <tr>
        <td>(<b>cons</b> x1 x2)</td>
        <td>Take 2 expressions and return a pair made of them</td>
    </tr>
    <tr>
        <td>(<b>car</b> list)</td>
        <td>Return the first element of a list/pair called `car`</td>
    </tr>
    <tr>
        <td>(<b>cdr</b> list)</td>
        <td>Return values from the second element of list/pair called `cdr`</td>
    </tr>
    <tr>
        <td>(<b>eq?</b> x1 x2)</td>
        <td>Return true if x1 and x2 are equals</td>
    </tr>
    <tr>
        <td>(<b>eqv?</b> x1 x2)</td>
        <td>Return true if x1 and x2 are equivalents</td>
    </tr>
    <tr>
        <td>(<b>=</b> n1 n2)</td>
        <td>Return true if n1 and n2 are equals</td>
    </tr>
    <tr>
        <td>(<b>string=?</b> ...strings)</td>
        <td>Return true if all strings are equals</td>
    </tr>
    <tr>
        <td>(<b>string->symbol</b> str)</td>
        <td>Convert string to symbol</td>
    </tr>
    <tr>
        <td>(<b>symbol->string</b> symbol)</td>
        <td>Convert symbol to string</td>
    </tr>
    <tr>
        <td>(<b>string->list</b> str)</td>
        <td>Convert string to list</td>
    </tr>
    <tr>
        <td>(<b>list->string</b> str)</td>
        <td>Convert list to string</td>
    </tr>
    <tr>
        <td>(<b>string->number</b> str)</td>
        <td>Convert string to number</td>
    </tr>
    <tr>
        <td>(<b>type?</b> expr)</td>
        <td>Return type of an expression</td>
    </tr>
</table>


### Stdlib - Lists
<table>
    <tr>
        <th>Name</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>(<b>list</b> ...lists)</td>
        <td>Make a list composed with arguments</td>
    </tr>
    <tr>
        <td>(<b>make-list</b> k expr)</td>
        <td>Make a list of size `k` fill with `expr`</td>
    </tr>
    <tr>
        <td>(<b>sort</b> list)</td>
        <td>Sort a list in ascending order</td>
    </tr>
    <tr>
        <td>(<b>fold-right</b> fn end lst)</td>
        <td>Recude list from first to last</td>
    </tr>
    <tr>
        <td>(<b>fold-left</b> fn acc lst)</td>
        <td>Reduce list from last to first</td>
    </tr>
    <tr>
        <td>(<b>map</b> p lst)</td>
        <td>Apply a procedure over list elements</td>
    </tr>
    <tr>
        <td>(<b>filter</b> p lst)</td>
        <td>Filter a list with a procedure</td>
    </tr>
    <tr>
        <td>(<b>member</b> x lst)</td>
        <td>If `x` is in `lst` return list from associated pair of `x` else return false</td>
    </tr>
    <tr>
        <td>(<b>list-ref lst idx</b>)</td>
        <td>Get element in list at index `idx`</td>
    </tr>
    <tr>
        <td>(<b>append</b> ...lists)</td>
        <td>Concatenate lists</td>
    </tr>
    <tr>
        <td>(<b>reverse</b> lst)</td>
        <td>Reverse list elements</td>
    </tr>
    <tr>
        <td>(<b>length</b> lst)</td>
        <td>Return length of a list</td>
    </tr>
</table>


### Stdlib - Math

<table>
    <tr>
        <th>Name</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>(<b>fact</b> n)</td>
        <td>Factorial n</td>
    </tr>
    <tr>
        <td>(<b>expt</b> x n)</td>
        <td>x^n</td>
    </tr>
    <tr>
        <td>(<b>fib</b> n)</td>
        <td>Fibonacci sequence at n</td>
    </tr>
    <tr>
        <td>(<b>odd?</b> n)</td>
        <td>Check if number is odd</td>
    </tr>
    <tr>
        <td>(<b>even?</b> n)</td>
        <td>Check if number is even</td>
    </tr>
    <tr>
        <td>(<b>positive?</b> n)</td>
        <td>Check if number is positive</td>
    </tr>
    <tr>
        <td>(<b>negative?</b> n)</td>
        <td>Check if number is negative</td>
    </tr>
    <tr>
        <td>(<b>zero?</b> n)</td>
        <td>Check if number is nul</td>
    </tr>
</table>



### Stdlib - Strings

<table>
    <tr>
        <th>Name</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>(<b>string-null?</b> str)</td>
        <td>Check if string is null</td>
    </tr>
    <tr>
        <td>(<b>make-string</b> k c)</td>
        <td>Make string of size `k` fill with char `c`</td>
    </tr>
    <tr>
        <td>(<b>string</b> ...chars)</td>
        <td>Make string from `chars`</td>
    </tr>
    <tr>
        <td>(<b>string-length</b> str)</td>
        <td>Return size of string</td>
    </tr>
    <tr>
        <td>(<b>string-ref</b> str i)</td>
        <td>Return char at position `i` in string `str`</td>
    </tr>
    <tr>
        <td>(<b>string-append</b> ...strs)</td>
        <td>Concat some string</td>
    </tr>
</table>



### Stdlib - Types

<table>
    <tr>
        <th>Name</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>(<b>string?</b> x)</td>
        <td>Check if `x` is a string</td>
    </tr>
    <tr>
        <td>(<b>number?</b> x)</td>
        <td>Check if `x` is a number</td>
    </tr>
    <tr>
        <td>(<b>list?</b> x)</td>
        <td>Check if `x` is a list</td>
    </tr>
    <tr>
        <td>(<b>pair?</b> x)</td>
        <td>Check if `x` is a pair</td>
    </tr>
    <tr>
        <td>(<b>boolean?</b> x)</td>
        <td>Check if `x` is a boolean</td>
    </tr>
    <tr>
        <td>(<b>procedure?</b> x)</td>
        <td>Check if `x` is a procedure</td>
    </tr>
    <tr>
        <td>(<b>symbol?</b> x)</td>
        <td>Check if `x` is a symbol</td>
    </tr>
    <tr>
        <td>(<b>char?</b> x)</td>
        <td>Check if `x` is a char</td>
    </tr>
    <tr>
        <td>(<b>atom?</b> x)</td>
        <td>Check if `x` is an atom</td>
    </tr>
</table>


### Stdlib - Miscellaneous

<table>
    <tr>
        <th>Name</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>(<b>not</b> x)</td>
        <td>If `x` equal false return true otherwise return false</td>
    </tr>
</table>



## To do

- IO
- Modules with export/import system
- Some chez-scheme procedures implementation
