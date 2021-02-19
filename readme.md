Metalinguistic abstraction -> the process of establishing new languages.

The process is important in computer programming because not only can we formulate
new languages but we can also implement them by constructing evaluators(a procedure that 
when applied to an expression of the language, performs actions required to evaluate that 
expression).

Most any program may be regarded as the evaluator for some language

Metacircular evaluator -> an evaluator written in the same language that it evaluates.

The job of an evaluator is not to specify the primitives but rather to provide the connective 
tissue, in essence, a means of combination and a means of abstraction that binds a collection 
of primitives to form a language.

The use of true? in eval-if highlights the issue of the conectino between an implemented language and the implementing language.
The if predicate is being evaluated in the language being implemented 
and thus yields a value in that language.
The evaluator predicate true? then transforms that value into a value 
that can be tested by the if in the implementation language

## How to Run
1. Install [MIT Scheme](https://www.gnu.org/software/mit-scheme/)
2. Run the Scheme REPL.
3. Compile the files by typing in the REPL `(cf "<file-name>")`
4. Load the driver loop by typing in the REPL `(load "driver_loop")`
