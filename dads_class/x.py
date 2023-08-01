#!/usr/bin/env python

# If you can't run Python programs yourself, try one of the following websites:
# http://repl.it/
# http://codepad.org/
# http://labs.codecademy.com/


def x(k):
    """Numerically investigate the continued fraction x =
    1 +         1
         ________________
          1 +      1
               __________
                1 +   1
                    _____
                      ...
    We won't be able to go out to infinity, so we'll use a counter to take only
    a (large) finite number of steps down this fraction.
    This is a recursive function; to find its answer, it will continue to call
    itself (with a different argument) until a certain stopping criterion is
    met.
    If our counter k is less than 1, we'll return 1; this is the stopping
    criterion---or base case---of our recursion.
    If not, we'll return 1 + 1 / x(k - 1) (our recursive step).
    """
    if k < 1:
        return 1.0
    else:
        return 1.0 + 1.0 / x(k - 1)


# Let's see x() in action
if __name__ == "__main__":
    # Print the outcome of stopping our continued fraction after 200 steps
    print(x(200))  # Does the output look familiar?
