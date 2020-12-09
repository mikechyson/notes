# Although Python does not need an emtry point,
# it is quite common in Python programs to create a function called main() and
# to call it to start off processing.

# Since no function can be called before it has been created,
# we must make sure we call main() after the function it relies on
# have be defined.

# The order in which the functions appear in the file
# (i.e., the order in which they are created)
# does not matter.

def main():
    maxwidth = 100  # the number of characters in a cell
    print_start()
    count = 0
    while True:
        try:
            line = input()
            if count == 0:  # head line labels
                color = 'lightgreen'
            elif count % 2:  # odd number
                color = 'while'
            else:
                color = 'lightyellow'
            print_line(line, color, maxwidth)
            count += 1
        except EOFError:
            break
    print_end()


# separate out the logic
def print_start():
    print("<table border='1'")


def print_end():
    print("</table>")


def print_line(line, color, maxwidth):
    print("<tr bgcolor='{}'>".format(color))
    # we cannot use str.split(",") to split each line into fields
    # because commas can occur inside quoted strings.
    fields = extract_fields(line)
    for field in fields:
        if not field:
            print("<td></td>")
        else:
            # use a new variable 'number', the field is not affected
            number = field.replace(",", "")
            try:
                x = float(number)
                print("<td align='right'>{:d}</td>".format(round(x)))
            except ValueError:
                field = field.title()
                field = field.replace(" And ", " and ")
                if len(field) <= maxwidth:
                    field = escape_html(field)
                else:
                    field = "{} ...".format(escape_html(field[:maxwidth]))
                print("<td>{}</td>".format(field))
    print('</td>')


def extract_fields(line):
    # learn this thought
    fields = []
    field = ""
    quote = None
    for c in line:
        if c in "\"'":
            if quote is None:  # start of quoted string
                quote = c
            elif quote == c:  # end of quoted string
                quote = None
            else:
                field += c  # other quote inside quoted string
            continue

        if quote is None and c == ",":  # end of a field
            fields.append(field)
            field = ""
        else:
            field += c  # accumulating a field
    if field:
        fields.append(field)  # adding the last field

    return fields


def escape_html(text):
    text = text.replace("&", "&amp;")
    text = text.replace("<", '&lt;')
    text = text.replace('>', '&gt;')
    return text


main()
