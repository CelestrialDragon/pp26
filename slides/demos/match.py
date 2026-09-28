class Circle:
    def __init__(self, r): self.r = r
class Square:
    def __init__(self, s): self.s = s
class Tri:
    def __init__(self, b, h): self.b, self.h = b, h

def area(sh):
    match sh:
        case Circle(): return 3.14159 * sh.r ** 2
        case Square(): return sh.s ** 2
        # Tri deliberately missing

print("square:", area(Square(3)))
print("triangle:", area(Tri(3, 4)))
