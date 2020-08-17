#!/usr/bin/env python

from datetime import datetime, timedelta
from itertools import accumulate
from operator import add
import random

random.seed(1729)


NAMES = ["Alice", "Bob", "Carol", "Dave"]
_ACTIVITIES = ["Reading", "Writing", "Arithmetic"]
_DATA = [
    {
        "timestamp": ts.time().isoformat(timespec="minutes"),
        "person": random.choice(NAMES),
        "activity": random.choice(_ACTIVITIES),
    }
    for ts in accumulate(
        (timedelta(minutes=random.randrange(17)) for _ in range(42)),
        func=add,
        initial=datetime.today().replace(hour=9),
    )
]
SELECT = {name: [x for x in _DATA if x["person"] == name] for name in NAMES}
