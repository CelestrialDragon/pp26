def sum_list(xs):
    if not xs:
        return 0
    return xs[0] + sum_list(xs[1:])

print(sum_list(list(range(100))))
print(sum_list(list(range(10_000))))

# Session 4, block 1: when the stack runs out.       python3 recursion_limit.py
#
# The code is at the top of the file so that the traceback says "line 4", as on
# the slide. The sum of 100 numbers needs 100 frames: it prints 4950. The sum of
# 10 000 needs 10 000 frames, and Python stops at 1 000: RecursionError. The
# recursion is right; the machine is finite. Python's limit is a counter, not the
# memory: sys.setrecursionlimit changes it.
