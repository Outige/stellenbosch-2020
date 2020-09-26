class Employee:

    def __init__(self, first, last, email):
        self.first = first
        self.last = last
        self.email = email

    def foo(self):
        print('foo')

    def __str__(self):
        return "first: %-30slast: %-30semail: %-30s" % (self.first, self.last, self.email)

e1 = Employee('Amy', 'Smith', '4myS@gmail.com')
print(e1)
e1.foo()