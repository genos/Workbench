#!/usr/bin/env python3
"""Time Traveling Key-Value Store"""


from pickle import dumps, loads
from sqlite3 import Row, connect
from time import perf_counter
from typing import Any, List, Optional


def perf_counter_ns() -> int:
    """`time.perf_counter_ns()` was introduced in 3.7"""
    return int(perf_counter() * 1e9)


class TTKV:
    """Time Traveling Key-Value Store

    Uses `sqlite3` and `pickle` to store things
    Could raise weird `pickle` errors with strange input
    """

    def __init__(self) -> None:
        self.conn = connect(":memory:")
        self.conn.row_factory = Row
        self.cursor = self.conn.cursor()
        self.cursor.execute(
            "create table ttkv (timestamp integer, key blob, value blob)"
        )
        self.cursor.execute("create index timestamp_index on ttkv(timestamp)")
        self.cursor.execute("create index key_index on ttkv(key)")
        self.conn.commit()

    def put(self, key: Any, value: Any) -> None:
        """Insert a (key ↦ value) pair"""
        self.cursor.execute(
            "insert into ttkv values (?, ?, ?)",
            (perf_counter_ns(), dumps(key), dumps(value)),
        )

    def get(self, key: Any, timestamp: Optional[int] = None) -> Any:
        """Retrieve a value for a given key

        If `timestamp` is `None`, return the latest value.
        If not, give the value at that time (up to & including it).

        Raises
        ------
        KeyError
            If `key` isn't present (or wasn't, if using a timestamp)

        ValueError
            If timestamp is not `None` and not an integer
        """
        select = "select * from ttkv where key = ?"
        order = "order by timestamp desc"
        if timestamp is None:
            self.cursor.execute(f"{select} {order}", (dumps(key),))
        elif isinstance(timestamp, int):
            self.cursor.execute(
                f"{select} and timestamp <= ? {order}", (dumps(key), timestamp)
            )
        else:
            raise ValueError(timestamp)
        result = self.cursor.fetchone()
        if result is None:
            raise KeyError(key)
        return loads(result["value"])

    def times(self, key: Any = None) -> List[int]:
        """All times (or just for a given key) in our TTKV"""
        select = "select timestamp from ttkv"
        order = "order by timestamp desc"
        if key is None:
            self.cursor.execute(f"{select} {order}")
        else:
            self.cursor.execute(f"{select} where key = ? {order}", (dumps(key),))
        return [row["timestamp"] for row in self.cursor.fetchall()]
