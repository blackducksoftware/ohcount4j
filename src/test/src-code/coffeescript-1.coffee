# A CoffeeScript parser test file

simple_code = true

###
A multi-line block comment
begins and ends with three hash marks
###

multi_line_string = '''
                    A multi-line string constant ("here doc")
                    begins and ends with three quote marks
                    '''

foo = "A string can wrap across multiple lines
  and may contain #{interpolated_values}"


###
A clever parser can use Ohcount's "Polyglot" feature treat the
following as embedded JavaScript.
###
embedded_js = `function() {
  return [document.title, "Hello JavaScript"].join(": ");
}`

