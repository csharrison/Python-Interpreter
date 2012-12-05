def nested_reraise():
    raise 5
def reraise():
    try:
        raise TypeError("foo")
    except:
        nested_reraise()
___assertRaises(TypeError, reraise)
