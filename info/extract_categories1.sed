s/^@deffn  *{[^}]*}  *\([^[:blank:]]*\).*/items = ["\1"]/
s/^@defvr  *{[^}]*}  *\([^[:blank:]]*\).*/items = ["\1"]/
s/^@deffnx  *{[^}]*}  *\([^[:blank:]]*\).*/if not "\1" in items: items.append ("\1")/
s/^@defvrx  *{[^}]*}  *\([^[:blank:]]*\).*/if not "\1" in items: items.append ("\1")/
s/^@end deffn/items = []/
s/^@end defvr/items = []/
s/^@node  *\([^,]*\).*/items = ["\1"] # extracted from node/
s/@opencatbox//
s/@closecatbox//
s/@category{\([^}]*\)}\s*/\$foo = []\$for x in items: foo.append ([items[0], x])\$try: categories ["\1"] . extend (foo)\$except KeyError: categories ["\1"] = foo\$/g
