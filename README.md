
# Table of Contents

1.  [Purpose of this Project](#org8617e77)
2.  [Status](#org5536948)
3.  [Features](#org5dd2478)
    1.  [Regex and Grammar Formats](#orgea1587e)
    2.  [Regex](#org2316306)
        1.  [Supported Regex Constructs](#org2d1c62e)
        2.  [Some Implementation Notes](#org2cc7245)
    3.  [Parser](#org69e3e20)
        1.  [Supported Grammar Constructs](#orga7fe614)
        2.  [Grammar Specification](#orgab9e3ad)
        3.  [Parsing](#org7b971f7)
    4.  [Components](#orgbf6f293)
4.  [Prerequisites](#org67a73dd)
5.  [Libraries (Dependencies)](#orgb02734e)
6.  [Installation](#org6ba42e1)
7.  [Usage](#org335a6e3)
    1.  [Regex DFA Generation](#org80b9e18)
        1.  [User Interface](#orge4545c3)
        2.  [Unit Tests ](#org013e4a1)
        3.  [Visualizing the GraphViz Dot Diagrams](#orgaed10f4)
    2.  [Regex Matching](#orgfccced1)
    3.  [Parsing](#org31ef9c6)
8.  [](#orgfe9a2e4)
9.  [Author](#org8b8c3f6)



<a id="org8617e77"></a>

# Purpose of this Project

This project started as an experimental sexp-based regex interpreter, and as of version **0.5.0**, it comprises the following:

-   Regular expression interpreter and DFA generator.
-   Regular expression matcher that could serve as backend for different tools (parsers, grep-like tools etc.).
-   PEG grammar interpreter and grammar IR generator.
-   Customizable recursive-descent parser.
-   Various generic utils that could end up in separate systems.

The syntax for both regular expression and grammar is currently provided in SEXP form, however, the design is flexible enough to introduce different front-ends. The decision to choose Lisp expression to represent such data should be obvious :)

<div class="note" id="orgc254348">

</div>

At this stage, I'm fairly satisfied with the maturity of this project, However, it is not meant to compete with well-established and feature-rich projects, such as **[CL-PPCRE](https://github.com/edicl/cl-ppcre)**.


<a id="org5536948"></a>

# Status

At this stage, I think the project could be useful as a workbench for people interested in Lisp (Common Lisp in particular), regular expressions, or recursive-descent parsers. It's equipped with hundreds of test cases, based on FiveAM, so hopefully the reader should not feel lost.

Regarding code quality, I was experimenting with different Common Lisp features and alternatives along the project. So whenever the reader would ask "why did he use CLOS here but a 'closure factory' there?", well, this could be the answer.


<a id="org5dd2478"></a>

# Features


<a id="orgea1587e"></a>

## Regex and Grammar Formats

I'm convinced that in Lisp, there is little reason to use a non-lisp syntax to define regular expressions or grammars, for different reasons:

-   We get regex or grammar parsing almost for free, thanks to the **Common Lisp Reader**. This also allows adding more features in the future, without the need for complicated regex or grammar parser updates.
-   Ambiguity related to order of evalution is avoided, thanks to the parentheses.

Even so, as mentioned previously, the design takes into consideration separating between representation and processing, for both the regex and grammar specifications . This means that different "front-ends" could be implemented, allowing  different forms (traditional regex, PEG grammar, JSON etc.).


<a id="org2316306"></a>

## Regex


<a id="org2d1c62e"></a>

### Supported Regex Constructs

Regex constructs are either simple (that is, matching a single character), or composed of other constructs.

The following table describes the supported constructs. For brievity, I'm assuming a level of familiarity with regular expressions in general.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Regex Element</th>
<th scope="col" class="org-left">Description</th>
<th scope="col" class="org-left">Example</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">Character</td>
<td class="org-left">Individual characters</td>
<td class="org-left">#\a</td>
</tr>

<tr>
<td class="org-left">Any Character</td>
<td class="org-left">An element matching any single character</td>
<td class="org-left">:any-char</td>
</tr>

<tr>
<td class="org-left">Character Range</td>
<td class="org-left">Character range, based on char-code order</td>
<td class="org-left">(char-range #\A #\F)</td>
</tr>

<tr>
<td class="org-left">Sequence</td>
<td class="org-left">Sequence of one or more elements (of any type)</td>
<td class="org-left">(seq #\x #\y)</td>
</tr>

<tr>
<td class="org-left">String</td>
<td class="org-left">Sequence of characters</td>
<td class="org-left">"xy" (same as previous one)</td>
</tr>

<tr>
<td class="org-left">Choice</td>
<td class="org-left">Choice between multiple elements</td>
<td class="org-left">(or "Hello" "Hi")</td>
</tr>

<tr>
<td class="org-left">Zero or More</td>
<td class="org-left">Zero or more occurrences of a specific element (Kleene closure)</td>
<td class="org-left">(* "a")</td>
</tr>

<tr>
<td class="org-left">One or more</td>
<td class="org-left">One or more occurrences of a specific element</td>
<td class="org-left">(+ "Hello ")</td>
</tr>

<tr>
<td class="org-left">Zero or One</td>
<td class="org-left">Zero or one occurrence of a specific element</td>
<td class="org-left">(? (or "Mr." "Mrs."))</td>
</tr>

<tr>
<td class="org-left">Regex Negation</td>
<td class="org-left">Matches anything other than the specified regex element</td>
<td class="org-left">(not "abc")</td>
</tr>

<tr>
<td class="org-left">Regex Inversion</td>
<td class="org-left">Equivalent to the caret (^) inside square brackets in classical regex format</td>
<td class="org-left">(inv #\a (char-range #\d #\m))</td>
</tr>

<tr>
<td class="org-left">Repetition</td>
<td class="org-left">Equivalent to <code>{m, n}</code> in classical regex format</td>
<td class="org-left">(rep (or "X" "O") 1 3)</td>
</tr>
</tbody>
</table>

Note:

-   Single character and string elements are specified as they would be read by the Common Lisp **reader**.
-   Construct types (**seq**, **or**, etc.) are specified using symbols.
-   Character overlaps are handled decently.
-   The **negation** element is still subject to changes, and its behavior will most probably be controlled using flags (to be added).


<a id="org2cc7245"></a>

### Some Implementation Notes

Here are some assorted implementation details, which need to be addressed by expanding them into architecture document, or that may need to be handled in the code (besides being documented):

1.  Backtracking

    Backtracking in case of no match and in case of *candidate match* are both implemented in the input source. These features are transparent to the regex matching function itself. There are two benefits from this:
    
    1.  The contract between the matching function and the input handling is simple.
    2.  Different behavior can be implemented in different implementations of the input source, without altering the matching function. For example, more efficient source could be implemented for exact matches only. Note that currently, only one implemententation is provided for the input source.


<a id="org69e3e20"></a>

## Parser


<a id="orga7fe614"></a>

### Supported Grammar Constructs

PEG grammar can be specified using the constructs described in the table below.

I'm assuming a level of familiarity with PEG grammars.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Construct</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">Sequence</td>
<td class="org-left">Sequence of one or more constructs</td>
</tr>

<tr>
<td class="org-left">Ordered Choice (Or)</td>
<td class="org-left">Ordered choice between multiple constructs</td>
</tr>

<tr>
<td class="org-left">Zero or More</td>
<td class="org-left">Zero or more occurrences of a specific construct</td>
</tr>

<tr>
<td class="org-left">One or more</td>
<td class="org-left">One or more occurrences of a specific construct</td>
</tr>

<tr>
<td class="org-left">Zero or One</td>
<td class="org-left">Zero or one occurrence of a specific construct</td>
</tr>
</tbody>
</table>

<div class="note" id="orgd32cce4">
<p>
I think this covers most of the PEG features, except maybe for <i>predicates</i>, which are not currently supported. May consider adding them in future update.
</p>

</div>


<a id="orgab9e3ad"></a>

### Grammar Specification

In this section, I'll describe briefly how can grammar be specified in SEXP form.

Grammar in this library may be specified as a list of forms;
each for could be either **token** or **rule**;
a *token* starts with the symbol `token`, and is specified using an *id* and a regex form;
a *rule* starts with the symbol `rule`, and is specified using an *id* and a grammar form, or an alias (reference) to another rule;

Here is an example:

    '((token id (seq
                 #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                 (+ (or #1# (char-range #\0 #\9)))))
      (token int (+ (char-range #\0 #\9)))
      (token *-op #\*)
      (token assign #\=)
      (token semicolon #\;)
      (rule factor (or id int))
      (rule mul-expr (seq factor (? (seq *-op factor))))
      (rule statement (seq id assign mul-expr semicolon))
      (rule statement-block (seq statement (* statement))))

Here is an sample text that complies with the above grammar:

    "id1=id2*3;id11=id22*33;"

Note the following:

-   The order of forms (tokens and rules) is irrelevant; the grammar "parser" is able to handle forward references.
-   White spaces are currently not treated in any special way. In a future update, the tokenizer would be able to handle them (e.g. treat them as token separators).
-   In the above grammar example, I'm using Common Lisp's object label notation (**#n=**, **#n#**) to avoid redundancy. I.e. these are not special features in the grammar itself, and it could be regarded as one of the benefits of relying on Common Lisp's reader.

From the last note, one could see that I leaned towards minimalism and using whatever feature that Common Lisp provides for free. Nevertheless, More features could be added in the future.


<a id="org7b971f7"></a>

### Parsing

The parser uses the IR (Intermediate Representation) generated from the grammar, to parse text provided by a **backtracking tokenizer**.

TODO: expand.


<a id="orgbf6f293"></a>

## Components

TODO, explain the different components:

-   Regex parser tree generator
-   NFA and DFA generators
-   Regex text matcher (and tokenizer)
-   Backtracking tokenizer
-   Parser (including pre and post notifications interface)


<a id="org67a73dd"></a>

# Prerequisites

-   Git
-   A Common Lisp installation, including ASDF (e.g. SBCL).
-   Quicklisp


<a id="orgb02734e"></a>

# Libraries (Dependencies)

Only **alexandria** is used, however, **iterate** is also declared as dependency (I'm content with the standard **loop**, though).


<a id="org6ba42e1"></a>

# Installation

Once the Git repository is cloned, the **ASDF** file (`parsex-cl.asd`) can be compiled and loaded in a REPL session (e.g. Emacs **Slime** REPL).

The project can then be loaded using **Quicklisp**, as follows:

    (ql:quickload 'parsex-cl)  

The project components will be loaded sequentially, as indicated in the following output:

    To load "parsex-cl":
      Load 1 ASDF system:
        parsex-cl
    ; Loading "parsex-cl"
    [package parsex-cl]...............................
    
    <other packages here>
    (PARSEX-CL)

TODO: Enhance this section.


<a id="org335a6e3"></a>

# Usage


<a id="org80b9e18"></a>

## Regex DFA Generation


<a id="orge4545c3"></a>

### User Interface

TODO: so far, I was focusing on test cases, and the only available matching function takes a DFA. This means that before matching, NFA and DFA need to be generated in a separate step (which is useful in case a single regex will be used in many matching operations, for performance). Next, I will focus on the user interface (API / command line), and update this section.


<a id="org013e4a1"></a>

### Unit Tests <a id="org53e5a29"></a>

First, the unit tests system needs to be loaded as follows:

    (ql:quickload "parsex-cl/test")

Then, running test cases can be done by first changing into the regex unit tests package:

    (in-package :parsex-cl.test/regex.test)

The output and updated prompt will indicate the **test** package:

    #<PACKAGE "PARSEX-CL.TEST/REGEX.TEST">
    TEST>

Finally, all defined test cases could be executed as follows:

    TEST> (run! :parsex-cl.regex.test-suite)

Of course, specific test suites or individual test cases could be ran by using the corresponding test name.

The output will provide information about the test cases (controlled by dynamic variables), including the following (depending on
some configuration variables):

-   Text being matched.
-   Regular expression being matched against.
-   Text consumed by the matching process (updated accumulator).
-   GraphViz Dot for the NFA finite state machine diagram.
-   GraphViz Dot for the DFA finite state machine diagram.
-   Test execution status (success/failure).

Here is a sample output for the execution of one of the test cases:

    ...
    Running test BASIC2-REGEX-MATCHING-TEST 
    Matching the text "abcacdaecccaabeadde" against the regex (+
                                                               (OR (CHAR-RANGE a d)
                                                                (CHAR-RANGE b e)))..
    
    Updated accumulator is abcacdaecccaabeadde
    
    Graphviz for NFA:
    digraph {
    rankdir = LR;
    
        0 -> 1 [label="b - e"];
        1 -> 2 [label="ε"];
        2 -> 3 [label="ε"];
        2 -> 4 [label="ε"];
        4 -> 5 [label="b - e"];
        5 -> 6 [label="ε"];
        6 -> 3 [label="ε"];
        6 -> 4 [label="ε"];
        4 -> 7 [label="a - d"];
        7 -> 6 [label="ε"];
        0 -> 8 [label="a - d"];
        8 -> 2 [label="ε"];
    }
    
    
    Graphviz for DFA:
    digraph {
    rankdir = LR;
    
        0 -> 1 [label="e - e"];
        1 -> 2 [label="e - e"];
        2 -> 2 [label="e - e"];
        2 -> 3 [label="b - d"];
        3 -> 2 [label="e - e"];
        3 -> 3 [label="b - d"];
        3 -> 4 [label="a - a"];
        4 -> 2 [label="e - e"];
        4 -> 3 [label="b - d"];
        4 -> 4 [label="a - a"];
        2 -> 4 [label="a - a"];
        1 -> 3 [label="b - d"];
        1 -> 4 [label="a - a"];
        0 -> 5 [label="b - d"];
        5 -> 2 [label="e - e"];
        5 -> 3 [label="b - d"];
        5 -> 4 [label="a - a"];
        0 -> 6 [label="a - a"];
        6 -> 2 [label="e - e"];
        6 -> 3 [label="b - d"];
        6 -> 4 [label="a - a"];
    }


<a id="orgaed10f4"></a>

### Visualizing the GraphViz Dot Diagrams

In order to inspect the NFA or DFA visually, the **dot** utility provided with **Graphviz** may be used to export the Dot output into **SVG**.

**Note**: A Graphviz installation is required for this step.

For example, to generate NFA and DFA for the regex `(or (seq :any-char #\z) "hello")`, and visualize it, you can use the following code:

    (let* ((*print-case* :downcase)
           (regex '(or (seq :any-char #\z) "hello"))
           (regex-obj-tree (parsex-cl/regex/sexp:prepare-regex-tree regex)))
      (multiple-value-bind (start end) (parsex-cl/regex/nfa:produce-nfa regex-obj-tree)
        (let ((dfa (parsex-cl/regex/dfa:nfa-to-dfa start)))
          (print end)
          (parsex-cl/graphviz-util:generate-graphviz-dot-diagram start "/tmp/sample-nfa.svg"
                                                                 :regex regex
                                                                 :dot-output-file "/tmp/sample-nfa.dot")
          (parsex-cl/graphviz-util:generate-graphviz-dot-diagram dfa "/tmp/sample-dfa.svg"
                                                                 :regex regex
                                                                 :dot-output-file "/tmp/sample-dfa.dot"))))

Then, view the generated SVG files with any modern web browser or vector graphics tool that supports it.

![img](./images/sample-nfa.svg "Sample NFA finite state machine diagram")

![img](./images/sample-dfa.svg "Sample DFA finite state machine diagram")


<a id="orgfccced1"></a>

## Regex Matching

TODO - for now, reader could refer to [regex-test.lisp](./test/regex-test.lisp) for examples on matching of text against a regex specified with a DFA.


<a id="org31ef9c6"></a>

## Parsing

TODO - for now, reader could refer to [parser-test.Lisp](./test/parser-test.lisp) for examples on parsing of text against a PEG grammar.


<a id="orgfe9a2e4"></a>

# TODO 

-   Test parser on a real-world grammar and text. This could be accompanied with a practical application (e.g. syntax coloring).
-   Implement **anchors**, via specific matching functions. This won't affect the regex engine itself.
-   There are also some TODOs in the source code that are worth checking out/cleaning up.
-   Optimization, refactoring etc&#x2026;
-   Revise/Complete the implementation of negation (add customizable behavior). Not a priority since the negation use case may not be useful/meaningful.


<a id="org8b8c3f6"></a>

# Author

-   John Kirollos (johnkirollos@gmail.com)

