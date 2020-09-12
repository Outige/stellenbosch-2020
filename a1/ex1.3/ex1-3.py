class CMYKColour:
    # constructor
    def __init__(self, c, m, y, k):
        self.c = float(c)
        self.m = float(m)
        self.y = float(y)
        self.k = float(k)
    
    # getters
    def cyan(self):
        return self.c
    def magenta(self):
        return self.m
    def yellow(self):
        return self.y
    def black(self):
        return self.k

    # equivalence
    def __eq__(self, other):
        if isinstance(other, CMYKColour):
            return self.c == other.c and self.m == other.m and self.y == other.y and self.k == other.k
        return False
    
    # toString
    def __str__(self):
        return '(%.2f, %.2f, %.2f, %.2f)'%(self.cyan(), self.magenta(), self.yellow(), self.black())

if __name__ == '__main__':
    # testing functions
    d = CMYKColour(0, 0.4, 0.4452123, 1)
    print(d)

    # equivalence
    d1 = CMYKColour(0, 0.4, 0.445212, 1)
    d2 = CMYKColour(0, 0.4, 0.4452123, 1)
    print(d1 == d2)