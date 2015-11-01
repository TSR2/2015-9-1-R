v=deriv(expression(x^3+y),'x')
is(v)
v
y=1
x=1
eval(v)

v=deriv(expression(x^3),"x",function.arg = T)
p=v(4)
p
p[1]
attr(p, "gradient")[1]

