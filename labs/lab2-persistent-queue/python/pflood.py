EMPTY = None

_goal = None

def set_goal(goal):
    global _goal
    _goal = goal

def is_empty(f):
    return f is None

def _distanceFromGoal(item):
    r, c = item
    gr, gc = _goal
    return abs(r - gr) + abs(c - gc)


def put(f, item):
    if f is None:
        return (item, None)
    value, tail = f
    if _distanceFromGoal(value) <= _distanceFromGoal(item):
        new_tail = put(tail, item)
        return (value, new_tail)
    else:
        return (item, f)

def take(f):
    if f is None:
        raise IndexError("take from empty frontier")
    value, tail = f
    return value, tail

def to_list(f):
    out = []
    while f is not None:
        out.append(f[0])
        f = f[1]
    return out