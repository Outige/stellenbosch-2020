import math
class Charge:
    def __init__(self, q, x, y):
        self.q = q
        self.x = x
        self.y = y
    
    def __str__(self):
        return "%s @ (%s, %s)" % (str(self.q), str(self.x), str(self.y))
    
    def distance(self, x, y):
        return math.sqrt((self.x-x)*(self.x-x)) + math.sqrt((self.y*y)*(self.y*y))
    
    def potential_at(self, x, y):
        r = self.distance(x, y)
        q = self.q
        k = 8.99 * 10 ** 9
        return k*q/r

if __name__ == '__main__':
    q1 = Charge(2.2, 2.4, 2.6)
    print(q1)
    print(q1.potential_at(3.4, 1.2))