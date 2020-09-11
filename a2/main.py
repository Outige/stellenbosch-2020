def neville(x_data, y_data, x):
    # base case: P0[xi] = yi
    if len(x_data) == 1 or len(y_data) == 1:
        return y_data[0]
    
    #
    top = (x - x_data[-1])*neville(x_data[0:len(x_data)-1], y_data[0:len(x_data)-1], x) + (x_data[0]-x)*neville(x_data[1:], y_data[1:], x)
    bot = x_data[0] - x_data[-1]
    return top/bot

if __name__ == '__main__':
    x_data = [0, 1, 2, 3, 4, 5, 6]
    y_data = [0, 1, 4, 9, 16, 25, 36]
    for x in x_data:
        result = neville(x_data, y_data, x=x)
        print(result)