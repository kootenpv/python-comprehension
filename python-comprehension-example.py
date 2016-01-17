# simple for dict
options = {}
for i in range(10):
    options[i] = i + 1

# if for list
options = []
for i in range(10):
    if i > 5:
        options.append(i)

# if else for set
options = {}
for i in range(10):
    if i > 5:
        options.add(i)
    else:
        options.add(None)

# if else for dict
options = {}
for i in range(10):
    if i > 5:
        options[i] = i + 1
    else:
        options[i] = None

# if elif else: should not work!
options = []
for i in range(10):
    if i > 5:
        options.append(i)
    elif i < 3:
        options.append(0)
    else:
        options.append(None)
