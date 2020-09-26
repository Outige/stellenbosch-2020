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
    
    # toString
    def __str__(self):
        return '(%s, %s, %s) = %s' % (str(self.red()), str(self.green()), str(self.blue()), str(self.luminance()))

if __name__ == '__main__':
    # testing functions
    c = RGBColour(122, 200, 170)
    print(c)

    # testing equality
    c1 = RGBColour(122, 200, 171)
    c2 = RGBColour(122, 200, 170)
    print(c1 == c2)