"""A persistent FIFO queue built from two cons-lists.  THE TASK IS HERE.

Cons-lists are exactly the ones from the lecture:
    None                 the empty list
    (head, tail)         a node whose tail is another cons-list

A queue is a pair (front, back):
    - dequeue takes from the head of `front`
    - enqueue pushes onto the head of `back`
    - abstraction:  queue as a sequence  =  front ++ reverse(back)
    - invariant:    if front is empty, back is empty too
                    (so peek/dequeue only ever look at front)

All operations must return NEW queues; never modify an argument.
Fill in the four TODOs, in order. Check progress with:
    python3 -m unittest test_pqueue -v
"""

EMPTY = (None, None)


def is_empty(q):
    front, _back = q
    return front is None


def peek(q):
    """The element that dequeue would return, without removing it."""
    front, _back = q
    if front is None:
        raise IndexError("peek at empty queue")
    return front[0]


def _rev(lst):
    running_lst = lst
    new_lst = None
    while running_lst is not None:
        head, tail = running_lst
        new_lst = (head, new_lst)
        running_lst = tail

    return new_lst


def _make(front, back):
    if front is None:
        frontnew = _rev(back)
        backnew = None
        return (frontnew,backnew)
    else:
        return front, back


def enqueue(q, x):
    front, _back = q
    """TODO 3: return a new queue with x added at the back. O(1).

    Do not rebuild anything: cons x onto back, keep front AS IS (shared!).
    """
    return _make(front, (x, _back))
    raise NotImplementedError("TODO 3: enqueue")


def dequeue(q):
    front, _back = q
    if front is None:
            raise IndexError
    value, _front = front
    
    """TODO 4: return (value, new_queue); raise IndexError if empty.

    Take the head of front; rebuild the rest through _make.
    """
    return (value, _make(_front, _back))
    raise NotImplementedError("TODO 4: dequeue")


# ---------------------------------------------------------------- helpers
# Provided. to_list is the ABSTRACTION FUNCTION: the sequence this
# queue represents. The tests compare the implementation against it.

def to_list(q):
    front, back = q
    out = []
    while front is not None:
        out.append(front[0])
        front = front[1]
    tail = []
    while back is not None:
        tail.append(back[0])
        back = back[1]
    return out + tail[::-1]


def from_iter(xs):
    q = EMPTY
    for x in xs:
        q = enqueue(q, x)
    return q
