# Language folder layout
Every language has a numeric id which serves to internally distinguish it from other languages. It doesn't matter what this number is, as long as it is a positive integer.

At present, all language files must be located in ```potential-doodle/langs/#/``` which ```#``` is the language's id. The directory structure is as follows:
- ```potential-doodle/langs/#/ ```
  - ```.generated/```
  - ```.temporary/```
  - [```translate/ ```](#translation-files)
    - ```#.txt```
    - ```##.txt```
  - [```lang.txt```](#lang.txt)
  - [```lexicon.txt```](#lexicon.txt)
  - [```morphology.lexc```](#.lexc-and-.twol)
  - [```morphology.twol```](#.lexc-and-.twol)

If you are using Lttoolbox rather than HFST, replace ```morphology.lexc``` and ```morphology.twol``` with ```lang.dix```.

All of this will be set up automatically by running ```python3 setuplang.py ID NAME```.
# File layout
Language data files are plaintext files. Each line in the file is made up of a mandatory _label_, an optional list of _arguments_ and an optional _value_. A label may consist of any characters other than ```():``` and spaces. If a line has arguments, they go immediately after the label in parentheses, separated by semicolons. For example, ```label (arg1; arg2)``` or ```label (arg)```. The value, if present, comes last and is separated from the label and arguments by a semicolon. For example, ```label (arg1; arg2): value``` or ```label: value```. There are no restrictions on what may be part of a value.

Lines that come after a particular line but are indented by two spaces are said to be the first line's _children_.

```
line-A (arg): blah
  line-B: bloop
    line-C
  line-D (aha!; zoom!)
line-E
```
In this example, C is a child of B while B and D are both children of A. In the various commands and parameters described in the following sections, the order of distinct children doesn't matter. The behavior of children with identical labels, however, varies by context: In a few instances it is allowed or expected, and these will be marked as such, but in most cases either the first instance or the last will be used. However, neither should be entirely relied upon to behave consistently.

Blank lines, lines composed entirely of space characters, and lines in which the first non-space character is ```#``` are ignored by the parser. At present it is not possible to have a comment after something else on a line.
# Object notation
Almost all arguments and values are interpreted as objects or strings. In object notation, all whitespace is ignored and any character other than ```|[]<>$:(){}=@%~*``` is treated as part of a name. If you want a space within a name, use a plus sign (```+```). Not all of the special characters are described in this document since the objects they produce are not currently used by the program.

## At
When a pattern is applied within another pattern, an at sign (```@```) in the containing pattern is a variable equivalent to the interior pattern.

## Morphemes
Morphemes are either words or parts of words. They have a _part of speech_ (pos), a _root_, and a set of _properties_. Their properties are either defined in ```lexicon.txt``` or given to them by rules in ```lang.txt```. They are written as ```pos=root```.

## Nodes
A node is part of a syntax tree which has a _type_, some number of _children_, and a set of _properties_. There are typically 2 children, and they can be other nodes, variables, morphemes, or nothing. The full notation is ```[type child1 child2 ...]{prop1=val1 prop2=val2 ...}``` with the part in brackets being optional.

Since all the files that currently exist were written with X-bar syntax, there is also an abbreviated form (```X``` standing in for any name):
- ```|[XP A B C D]``` expands to ```[XP A [Xmod B [Xbar C D]]]```
- ```|[XP A B C]``` expands to ```[XP A [Xmod ~ [Xbar B C]]]``` (missing modifier)
- ```|[XP A B]``` expands to ```[XP ~ [Xmod ~ [Xbar A B]]]``` (missing modifier and specifier)
- ```|[XP A]``` expands to ```[XP ~ [Xmod ~ [Xbar A ~]]]``` (missing modifier, specifier, and complement)
- ```|[XP]``` expands to ```[XP ~ [Xmod ~ [Xbar ~ ~]]]``` (missing modifier, specifier, complement, and head)

What goes in the missing slots can be specified by placing a character before the node type:
- ```$XP``` fills the gaps from ```$Xspec, $Xmod, $Xhead, $Xcomp```
- ```?XP``` fills the gaps from ```$Xspec?, $Xmod?, $Xhead?, $Xcomp?```
- ```*XP``` fills the gaps with ```*```
- ```~XP``` fills the gaps with ```~``` (identical to simply ```XP```)

## Nothing
The symbol ```~``` is used to represent that a location exists but doesn't contain anything.

## Unknown
The symbol ```*``` will match literally anything.

## Variables
Variables are used to match parts of trees in patterns and for inserting values into patterns. Variables normally consist of a dollar sign ```$``` followed by the name of the variable. After this comes the optional _condition_. In a pattern context, a variable will match anything that satisfies it's condition. In an insertion context the condition is ignored. Nothing (```~```) does not satisfy a non-existent condition.

If the condition is ```!```, the variable will only match an empty space. If the condition is a colon (```:```) followed by a name, the variable will match any node with that as its type or any morpheme with that as its part of speech. If the name or the type condition ends with a question mark (```?```) the variable will be able to match nothing in addition to whatever it normally matches.

Finally, a further condition can be specified between parentheses. If this condition is of the form ```(property=value)``` then the variable will be restricted to nodes and morphemes which have that property with that value. Otherwise what is between the parentheses will be interpreted as an object and the variable can only match things that are equivalent to that object.

Names which the parser is unable to interpret as part of anything else will be treated as conditions for nameless variables, which is useful in syntax patterns but will probably give unexpected results otherwise.

### Examples
- ```$blah``` - a variable called "blah" which can match anything except ```~```
- ```$s``` - a variable called "s" which can match anything
- ```$subj:noun``` - a variable called "subj" which can only match ```noun```s
- ```$sock!``` - a variable called "sock" which can only match nothing
- ```$x?(h=c)``` - a variable called "x" which can match nothing or something with the property "h" and value "c"
- ```$x:NP([NP $x noun=sock])``` - a variable called "x" which can only match nodes of type ```NP``` which have 2 children, the first being equal to ```$x```, if ```$x``` has a value, or simply not nothing if it doesn't, and the second being the noun "sock"

# lang.txt
```lang.txt``` is the main file of the language directory and currently contains 4 sections: metadata, syntax, lexc, and transform.
## metadata
This may contain any information about the language, but currently the only interpreted properties are ```creator``` and ```name```.

Example:
```
metadata
  creator: Common Honey Tribe
  name
    local: Sajem Tan
    1: Common Honey
```
## syntax
The ```syntax``` section specifies how to generate a syntax tree, both randomly and when trying to parse a text. It has the following children:

```start-with: XP``` specifies that ```XP``` should be the type of the highest node in the tree.

```auto-setlang: AP; BP; CP; bloop``` specifies that when the translator encounters nodes whose types are in the set ```{AP Amod Abar BP Bmod Bbar CP Cmod Cbar bloop}``` it can just copy them over directly. This is generally unnecessary, since the language of syntactic nodes is ignored by default.

The children of ```node-types``` each specify the rules for generating particular type of node. The label of each child is the way it is referred to by the other rules, though not necessarily the name of the node it generates. Each node has 0 or more ```variable``` children, specifying sub-elements to be generated first, and ```option``` children, each a possible final output. If there is only one ```option```, it may be omitted and its children placed directly under the node.

For each ```variable```, an element will be selected which statisfies the condition and will be assigned to the variable. So ```variable: $x:noun``` will assign some noun as the value of ```$x```, and ```variable: $x:noun(gender=m)``` will assign a masculine noun as the value of ```$x```.

Each ```option``` must have either as a child either ```xbar``` or ```structure```.

```structure``` simply specifies the structure of the node. So, for instance ```structure: |[NP $x PP]``` would generate a noun phrase contain the previously declared variable ```$x``` and a prepositional phrase (or whatever else the PP rule might generate). ```structure``` may optionally be accompanied by ```translation (LANG): FORM``` where ```LANG``` is the destination language and ```FORM``` is the result. This automatically creates a translation rule, though this probably best done in the translation files.

```xbar: A; B; C; D``` is equivalent to ```structure: |[XP A B C D]``` with ```XP``` being replaced with the rule name.

Each option can also have one or more ```require``` lines. These tell the generator to ignore the option if a certain type of morpheme is unavailable. For example, ```require: pronoun``` will only allow the option to be selected if it is possible to generate a pronoun. This primarily exists to increase the efficiency of the parser.
## lexc
## transform
# lexicon.txt
# Translation files
# .pdtxt files
.pdtxt files are documents ready to be translated. The file has the same syntax as the language data files. The first line will be ```metadata``` with a child which says ```lang: ID``` with ```ID``` being the language of the document. The metadata section may contain other data about the document, but nothing is currently done with it. Each line after this will have the line number or some other identifier as the label and usually the primary tree for the sentence as the value. Any children will be interpreted as alternative trees (this would happen, for instance, if the translator had multiple outputs) except ```gloss``` which optionally holds the original plaintext.
# .lexc and .twol
