# Compound Indexing Calculator #

A quick hack in scheme that calculates compound (and simple) indexing movements for a dividing head of a specific ratio and with a given set of plates.

In order to run this, you will need:

- An r6rs scheme.  I developed this using Chez Scheme, https://www.scheme.com, but others will probably work.
- An installation of the r6rs SRFI libraries, at least SRFI-1 and SRFI-26.  For Chez Scheme, https://github.com/arcfide/chez-srfi will do the trick.  The SRFI installation will need to be either on scheme's seach path, or passed in as a command line parameter.

- A TeX installation.  TeX is a bastard to install, try https://miktex.org/

We can then run the application, including the relevant dividing head definitions before the application itself.  Yeah, I should make this easier to run, but I can't be buggered.  Something like this:

> scheme hbm.scm compound-division.scm

It will take a while, and hopefully spit out a nicely formatted PDF.  To do this for your "special" dividing head, or for your own parameters, duplicate or modify a set of dividing head definitions.

The provided files are definitions for the dividing heads listed in image dividing-heads.jpg

The default allowable error percentage produces results which are certainly respectable, but perhaps not ptimum.  Decreasing the allowable error will increase precision, but at the cost of run time.  If you want the absolute summum of precision, edit compound-division.scm, function acceptable-solutions-all-targets-for, commenting out or removing the following line : 

               ((>= (length results) 3) (take results 3)) ;; Return top 3 approximations

This will have the effect of traversing the entire search space, returning *all* solutions meeting the error criteria.  This guarantees the best approximations possible using additive compounding, but the cost in terms of processing time is massive - on my machine, calculating divisions 1 to 400 for the Browne & Sharpe divider takes around 10 seconds of CPU time if we cut at 3 results, more than 2 hours if we don't.  Sure, my code's not optimal, but the search space is enormous.
