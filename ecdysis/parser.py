
# based on "implementing read" from:
# http://www.cs.utexas.edu/users/wilson/schintro/schintro_115.html



def tokenize(str):
    current = []
    expecting = "any"

    for ch in str:
        endOfToken = False
        nextToken = None

        # strings can contain anything:
        if expecting == "string":
            if ch =='"':
                endOfToken=True
            current.append(ch)

        # split on whitespace:
        elif ch in " \t\n\r":
            if expecting == "any":
                pass
            else:
                endOfToken = True

        # split on parentheses
        elif ch in "()":
            endOfToken = True
            nextToken = ch

        # collect chars into numbers:
        elif ch in "0123456789":
            if expecting == "any":
                expecting = "num"
            current.append(ch)

        # " starts a string:
        elif ch == '"':
            if expecting=="any":
                expecting = "string"
            else:
                raise SyntaxError("unexpected quote")
            current.append(ch)
                
        # alpha or underline starts an identifier
        elif ch in ":_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ":
            if expecting == "any":
                expecting = "sym"
            elif expecting == "num":
                raise SyntaxError("unexpected char in number: %s" % ch)
            current.append(ch)

        # close off tokens:
        if endOfToken:
            expecting = "any"
            if current:
                yield "".join(current)
                current = []

        if nextToken:
            yield nextToken


print [t for t in tokenize('(cat in23 (the "ho(()2t (little")  04 hat)')]

print [t for t in tokenize(
    """
    (set (:int foo) 5)
    (set (:int bar) 4)
    (:int (add (:int a) (:int b))
        (+ a b))
    """)]
