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

class RGBColour:
    def __init__(self, r, g, b):
        if r > 255 or r < 0 or g > 255 or g < 0 or b > 255 or b < 0:
            raise ValueError("out of range [0,255]")
        self.r = r
        self.g = g
        self.b = b
    
    # equality
    def __eq__(self, other):
        if isinstance(other, RGBColour):
            return self.r == other.r and self.g == other.g and self.b == other.b
        return False
    
    # getters
    def red(self):
        return self.r
    def green(self):
        return self.g
    def blue(self):
        return self.b
    
    # functions
    def luminance(self):
        return 0.299*self.r + 0.587*self.g + 0.114*self.b
    
    def as_hex(self):
        r = hex(self.r)[2:]
        if len(r) < 2:
            r = (2-len(r)) * '0' + r

        g = hex(self.g)[2:]
        if len(g) < 2:
            g = (2-len(g)) * '0' + g

        b = hex(self.b)[2:]
        if len(b) < 2:
            b = (2-len(b)) * '0' + b

        return r + g + b

    def to_cmyk(self):
        w = max(self.r/255, self.g/255, self.b/255)
        c = (w - (self.r/255))/w
        m = (w - (self.g/255))/w
        y = (w - (self.b/255))/w
        k = 1 - w
        return CMYKColour(c, m, y, k)

    
    # toString
    def __str__(self):
        return '(%s, %s, %s) = %s' % (str(self.red()), str(self.green()), str(self.blue()), str(self.luminance()))

if __name__ == '__main__':
    # testing functions
    c = RGBColour(140, 15, 0)
    print(c)
    print(c.as_hex())
    print(c.to_cmyk())

    # testing equality
    c1 = RGBColour(122, 200, 171)
    c2 = RGBColour(122, 200, 170)
    print(c1 == c2)