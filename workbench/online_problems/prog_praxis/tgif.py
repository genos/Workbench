#!/usr/bin/env python

from fractions import Fraction

DAYS = {0: "Sunday", 1: "Monday", 2: "Tuesday", 3: "Wednesday", 4: "Thursday",
        5: "Friday", 6: "Saturday"}


def gauss(year, month, day):
    yr = (year - 1) if (month < 3) else year
    m = (month + 10) if (month < 3) else (month - 2)
    c, y = divmod(yr, 100)
    return DAYS[sum((day, int((Fraction('13/5') * m) - Fraction('1/5')), y,
                 (y / 4), (c / 4), - 2 * c)) % 7]


def sakamoto(year, month, day):
    t = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
    y = (year - 1) if (month < 3) else year
    return DAYS[sum((day, y, (y / 4), - (y / 100), (y / 400), t[month - 1]))
                % 7]


def doomsday(year):
    return DAYS[sum((2, year, year / 4, - (year / 100), year / 400)) % 7]


if __name__ == "__main__":
    print gauss(2011, 6, 24)
    print sakamoto(2011, 6, 24)
    print doomsday(2011)
