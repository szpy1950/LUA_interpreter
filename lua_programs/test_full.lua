-- variables
local a = 5
local b = 10
print("a + b =", a + b)

-- if
if a < b then
    print("a is less than b")
end

-- while
local i = 1
while i <= 3 do
    print("i =", i)
    i = i + 1
end

-- for
for j = 1, 3 do
    print("j =", j)
end

-- function
local function add(x, y)
    return x + y
end
print("add(2,3) =", add(2, 3))

-- table
local t = {10, 20, 30}
print("t[1] =", t[1])

-- table with keys
local person = {name = "Alice", age = 25}
print("name =", person.name)

print("Done!")
