#!/usr/bin/env python
from random import randrange
WHEEL = ["BAR", "BELL", "ORANGE", "LEMON", "PLUM", "CHERRY"]


def display_instructions():
    print "WELCOME TO THE CASINO"
    print "BET IN INCREMENTS OF $1 FROM $1 TO $100"
    print "BET $0 WHEN YOU ARE FINISHED"
    return


def pull(n):

    def _win(msg, d):
        print "***{0}***".format(msg)
        print "YOU WIN ${0}".format(n * d)
        return n * d

    x, y, z = randrange(6), randrange(6), randrange(6)
    print WHEEL[x], WHEEL[y], WHEEL[z]
    if x == y == z == 0:
        return _win("JACKPOT", 101)
    elif x == y == z:
        return _win("TOP DOLLAR", 11)
    elif x == y == 0 or x == z == 0 or y == z == 0:
        return _win("DOUBLE BAR", 6)
    elif x == y or x == z or y == z:
        return _win("DOUBLE", 3)
    else:
        print "YOU LOSE ${0}".format(n)
        return -n


def play():
    display_instructions()
    bet, purse = 1, 0
    while bet != 0:
        bet = raw_input("ENTER YOUR BET:\t")
        try:
            bet = int(bet)
        except ValueError:
            continue
        if bet > 100:
            print "HOUSE LIMIT $100"
            continue
        elif 1 <= bet <= 100:
            purse += pull(bet)
            if purse > 0:
                print "YOU HAVE ${0}".format(purse)
            elif purse < 0:
                print "YOU OWE ${0}".format(-purse)
            else:
                print "YOU ARE EVEN"
    if purse < 0:
        print "PLACE ${0} ON THE KEYBOARD".format(-purse)
    elif purse > 0:
        print "COLLECT ${0} FROM THE CASHIER".format(purse)
    else:
        print "YOU BROKE EVEN"
    return


if __name__ == "__main__":
    play()

"""
My Python solution. Like the Haskell above, I factored out the <code>_win</code>
procedure. I'm not terribly happy with the maze of ifs and elifs my code grew
into, but it'll do.
[sourcecode lang=python]
#!/usr/bin/env python
from random import randrange
WHEEL = ["BAR", "BELL", "ORANGE", "LEMON", "PLUM", "CHERRY"]


def display_instructions():
    print "WELCOME TO THE CASINO"
    print "BET IN INCREMENTS OF $1 FROM $1 TO $100"
    print "BET $0 WHEN YOU ARE FINISHED"
    return


def pull(n):

    def _win(msg, d):
        print "***{0}***".format(msg)
        print "YOU WIN ${0}".format(n * d)
        return n * d

    x, y, z = randrange(6), randrange(6), randrange(6)
    print WHEEL[x], WHEEL[y], WHEEL[z]
    if x == y == z == 0:
        return _win("JACKPOT", 101)
    elif x == y == z:
        return _win("TOP DOLLAR", 11)
    elif x == y == 0 or x == z == 0 or y == z == 0:
        return _win("DOUBLE BAR", 6)
    elif x == y or x == z or y == z:
        return _win("DOUBLE", 3)
    else:
        print "YOU LOSE ${0}".format(n)
        return -n


def play():
    display_instructions()
    bet, purse = 1, 0
    while bet != 0:
        bet = raw_input("ENTER YOUR BET:\t")
        try:
            bet = int(bet)
        except ValueError:
            continue
        if bet > 100:
            print "HOUSE LIMIT $100"
            continue
        elif 1 <= bet <= 100:
            purse += pull(bet)
            if purse > 0:
                print "YOU HAVE ${0}".format(purse)
            elif purse < 0:
                print "YOU OWE ${0}".format(-purse)
            else:
                print "YOU ARE EVEN"
    if purse < 0:
        print "PLACE ${0} ON THE KEYBOARD".format(-purse)
    elif purse > 0:
        print "COLLECT ${0} FROM THE CASHIER".format(purse)
    else:
        print "YOU BROKE EVEN"
    return


if __name__ == "__main__":
    play()


[/sourcecode]
"""
