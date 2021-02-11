
list_of_lists = [["Apple", 120], ["Amazon", 150], ["Tesla", 100], ["Apple", 50], ["Facebook", 95],
                 ["Apple", 50]]


def portfolio():
    stock_dict = {}

    for key, value in list_of_lists:
        if key in stock_dict:
            stock_dict[key] += value
        else:
            stock_dict[key] = value
    return stock_dict


portfolio()
