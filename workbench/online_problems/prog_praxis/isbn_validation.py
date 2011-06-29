#!/usr/bin/env python
"""isbn_validation.py

My submission to http://programmingpraxis.com/2011/05/20/isbn-validation/
Various ISBN and EAN utilities, including internet search.

GRE, 5/20/11
"""

from ast import literal_eval
from urllib2 import urlopen


def clean_isbn(isbn):
    """List of digits in ISBN"""
    digits = set("0123456789")
    return [int(x if x in digits else 10) for x in isbn.translate(None, " -")]


def isbn_check_digit(isbn):
    """Check digit for (9 digit long) ISBN"""
    return (11 - (sum(x * y for (x, y) in enumerate(reversed(isbn), start=2))
            % 11)) % 11


def is_valid_isbn(isbn):
    """Validate check digit against rest of ISBN"""
    clean = clean_isbn(isbn)
    return clean[-1] == isbn_check_digit(clean[:-1])


def clean_ean(ean):
    """List of digits in EAN"""
    return [int(x) for x in ean.translate(None, " -")]


def ean_check_digit(ean):
    """Check digits for (12 digit long) EAN"""
    return (10 - (sum((2 + (-1) ** x) * y for (x, y) in enumerate(ean,
        start=1)) % 10)) % 10


def is_valid_ean(ean):
    """Validate check digit against rest of EAN"""
    clean = clean_ean(ean)
    return clean[-1] == ean_check_digit(clean[:-1])


def to_ean(isbn):
    """Convert ISBN to EAN"""
    clean = clean_isbn(isbn)
    ean = [9, 7, 8]
    ean.extend(clean[:-1])
    ean.append(ean_check_digit(ean))
    return ''.join(str(d) for d in ean)


def to_isbn(ean):
    """Convert EAN to ISBN"""
    clean = clean_isbn(ean)
    isbn = clean[3:-1]
    isbn.append(isbn_check_digit(isbn))
    return ''.join(str(d) for d in isbn)


def isbn_lookup(isbn):
    """Search Google Books for ISBN"""
    base = "https://www.googleapis.com/books/v1/volumes?q=isbn="
# Unfortunately we can't use the superior "with spam as eggs" syntax here...
    search = urlopen(base + isbn + "&prettyprint=false")
    lines = search.read()
    search.close()
    for bool_pair in [("false", "False"), ("true", "True")]:
        lines = lines.replace(*bool_pair)
    volume_info = literal_eval(lines)["items"][0]["volumeInfo"]
    title = volume_info["title"]
    authors = ', '.join(a for a in volume_info["authors"])
    return "Title:\t\t%s\nAuthor(s):\t%s" % (title, authors)


if __name__ == "__main__":
    ISBN = "0495014281"
    EAN = "9780495014287"
    print "Is %s a valid ISBN?\t%s" % (ISBN, "Yes" if is_valid_isbn(ISBN) else
            "No")
    print "Is %s a valid EAN?\t%s" % (EAN, "Yes" if is_valid_ean(EAN) else
            "No")
    print isbn_lookup(to_isbn("9780262011532"))
