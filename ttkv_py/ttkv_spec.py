#!/usr/bin/env python3
"""Property tests for TTKV"""


from typing import Tuple
from ttkv import TTKV
from hypothesis import assume, given
from hypothesis.strategies import composite, floats, integers, text
from pytest import raises


SQLITE_MIN_INT = -(1 << 63)
SQLITE_MAX_INT = (1 << 63) - 1
STR = text(min_size=1)
INT = integers(min_value=SQLITE_MIN_INT, max_value=SQLITE_MAX_INT)


@composite
def distinct_keys(draw) -> Tuple[str, str]:
    a, b = draw(STR), draw(STR)
    assume(a != b)
    return (a, b)


@given(STR)
def test_initially_empty(a: str) -> None:
    with raises(KeyError):
        TTKV().get(a)


@given(STR, INT)
def test_single_get(a: str, x: int) -> None:
    ttkv = TTKV()
    ttkv.put(a, x)
    assert ttkv.get(a) == x


@given(distinct_keys(), INT, INT)
def test_two_gets_different_keys(ab: Tuple[str, str], x: int, y: int) -> None:
    a, b = ab
    ttkv = TTKV()
    ttkv.put(a, x)
    ttkv.put(b, y)
    assert ttkv.get(a) == x
    assert ttkv.get(b) == y


@given(STR, INT, INT)
def test_two_puts_same_key(a: str, x: int, y: int) -> None:
    ttkv = TTKV()
    ttkv.put(a, x)
    ttkv.put(a, y)
    assert ttkv.get(a) == y


@given(STR, INT, INT)
def test_two_puts_same_key_two_times(a: str, x: int, y: int) -> None:
    ttkv = TTKV()
    ttkv.put(a, x)
    ttkv.put(a, y)
    assert len(ttkv.times(a)) == 2


@given(STR, STR, INT, INT)
def test_two_puts_two_times(a: str, b: str, x: int, y: int) -> None:
    ttkv = TTKV()
    ttkv.put(a, x)
    ttkv.put(b, y)
    assert len(ttkv.times()) == 2


@given(STR, INT, INT, floats(min_value=0, max_value=1 - 1e-16))
def test_middle_get(a: str, x: int, y: int, d: float) -> None:
    ttkv = TTKV()
    ttkv.put(a, x)
    ttkv.put(a, y)
    [t1, t0] = ttkv.times(a)
    δ = (t1 - t0) * d
    assert ttkv.get(a, t0 + int(δ)) == x


@given(STR, INT, integers(min_value=0, max_value=SQLITE_MAX_INT))
def test_get_before_time(a: str, x: int, t: int) -> None:
    ttkv = TTKV()
    ttkv.put(a, x)
    t0 = ttkv.times(a)[0]
    with raises(KeyError):
        TTKV().get(a, t0 - t)
