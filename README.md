# python-comprehend.el

The goal is to offer a single shortcut to make comprehensions from for loops.

Use the function `python-comprehend` to search back for a `for` loop and try to write it as a comprehension:

```python
# if else for dict
options = {}
for i in range(10):
    if i > 5:
        options[i] = i + 1
    else:
        options[i] = None


# becomes
options = {i: i + 1 if i > 5 else None for i in range(10)}
```

It currently handles:

- set, list, dict
- no ifs (a simple for loop), ifs, and if..elses
