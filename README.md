# Welcome to tea-script!

I was jointly inspired by CoffeeScript, work I've done with Prof. David Van
Horn, and frustration with JS's C-like syntax to write tea-script.
Essentially, tea-script is a scheme-like language which compiles down to
JavaScript.  I envision it replacing JavaScript for those of us who would
rather program in SExps.

Initially, I want a simple translation from a small subset of Scheme to vanilla
(human readable) JavaScript.  With that done, I'd like to transition to
implementing cool features of Scheme such as `call/cc' and macros--I'll likely
branch tea-script to work on these new features.  I'd like to keep the generated
JavaScript reasonably faithful to the original TeaScript because I find it
infuriating when a library has minimzed, illegible JavaScript which mutilate
stack traces.  That said, it is beneficial to provide a "production" option
which produces highly optimized output.

Overall, this is a fun project for me to explore the world of compiler writing.

System Requirements:
This is all developed in Racket (`http://racket-lang.org`), a Scheme derivative.
The `eval.rkt` test file requires rhino to be installed on your system.
