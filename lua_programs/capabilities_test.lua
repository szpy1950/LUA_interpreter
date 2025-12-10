-- ============================================================================
-- CAPABILITIES TEST FILE
-- This file demonstrates all features our Lua interpreter can handle
-- ============================================================================

print("=== SECTION 1: WHAT OUR INTERPRETER CAN DO ===")
print("")

-- ----------------------------------------------------------------------------
-- 1.1 LITERALS AND BASIC TYPES
-- ----------------------------------------------------------------------------
print("--- 1.1 Literals and Types ---")

local nil_val = nil
local bool_true = true
local bool_false = false
local num_int = 42
local num_float = 3.14159
local num_negative = -100
local str = "Hello, World!"

print("nil:", nil_val)
print("boolean true:", bool_true)
print("boolean false:", bool_false)
print("integer:", num_int)
print("float:", num_float)
print("negative:", num_negative)
print("string:", str)
print("")

-- ----------------------------------------------------------------------------
-- 1.2 TYPE FUNCTION
-- ----------------------------------------------------------------------------
print("--- 1.2 Type Function ---")

print("type(nil):", type(nil))
print("type(true):", type(true))
print("type(42):", type(42))
print("type('hello'):", type("hello"))
print("type({}):", type({}))
print("type(print):", type(print))
print("")

-- ----------------------------------------------------------------------------
-- 1.3 ARITHMETIC OPERATORS
-- ----------------------------------------------------------------------------
print("--- 1.3 Arithmetic Operators ---")

local a, b = 10, 3
print("a =", a, ", b =", b)
print("a + b =", a + b)
print("a - b =", a - b)
print("a * b =", a * b)
print("a / b =", a / b)
print("a // b (floor div) =", a // b)
print("a % b (modulo) =", a % b)
print("a ^ b (power) =", a ^ b)
print("-a (unary minus) =", -a)
print("")

-- ----------------------------------------------------------------------------
-- 1.4 COMPARISON OPERATORS
-- ----------------------------------------------------------------------------
print("--- 1.4 Comparison Operators ---")

print("5 < 10:", 5 < 10)
print("5 > 10:", 5 > 10)
print("5 <= 5:", 5 <= 5)
print("5 >= 5:", 5 >= 5)
print("5 == 5:", 5 == 5)
print("5 ~= 10:", 5 ~= 10)
print("'abc' < 'abd':", "abc" < "abd")
print("")

-- ----------------------------------------------------------------------------
-- 1.5 LOGICAL OPERATORS
-- ----------------------------------------------------------------------------
print("--- 1.5 Logical Operators ---")

print("true and false:", true and false)
print("true or false:", true or false)
print("not true:", not true)
print("not false:", not false)
print("not nil:", not nil)

-- Short-circuit evaluation
print("nil and 'hello':", nil and "hello")
print("'hello' and 'world':", "hello" and "world")
print("nil or 'default':", nil or "default")
print("'value' or 'default':", "value" or "default")
print("")

-- ----------------------------------------------------------------------------
-- 1.6 STRING CONCATENATION
-- ----------------------------------------------------------------------------
print("--- 1.6 String Concatenation ---")

local first = "Hello"
local second = "World"
print(first .. " " .. second .. "!")
print("Number concat: " .. 42 .. " is the answer")
print("")

-- ----------------------------------------------------------------------------
-- 1.7 LENGTH OPERATOR
-- ----------------------------------------------------------------------------
print("--- 1.7 Length Operator (#) ---")

print("#'hello' =", #"hello")
local arr = {10, 20, 30, 40, 50}
print("#array =", #arr)
print("")

-- ----------------------------------------------------------------------------
-- 1.8 BITWISE OPERATORS - UNARY ONLY
-- ----------------------------------------------------------------------------
print("--- 1.8 Bitwise Operators (unary only) ---")

-- Only unary ~ (bitwise NOT) is supported by parser
print("~0 (NOT) =", ~0)
print("~15 (NOT) =", ~15)
print("(Binary bitwise operators &, |, ^, <<, >> are NOT parsed)")
print("")

-- ----------------------------------------------------------------------------
-- 1.9 VARIABLES AND ASSIGNMENT
-- ----------------------------------------------------------------------------
print("--- 1.9 Variables and Assignment ---")

-- Local variables
local x = 100
local y, z = 200, 300
print("local x:", x)
print("local y, z:", y, z)

-- Global variables
globalVar = "I am global"
print("global:", globalVar)

-- Multiple assignment
local p, q, r = 1, 2, 3
print("p, q, r:", p, q, r)

-- Swap
p, q = q, p
print("after swap p, q:", p, q)

-- Less values than variables
local m, n = 10
print("m:", m, "n:", n)
print("")

-- ----------------------------------------------------------------------------
-- 1.10 TABLES - ARRAYS
-- ----------------------------------------------------------------------------
print("--- 1.10 Tables - Arrays ---")

local numbers = {10, 20, 30, 40, 50}
print("numbers[1]:", numbers[1])
print("numbers[3]:", numbers[3])
print("numbers[5]:", numbers[5])

-- Modify array
numbers[2] = 25
print("after numbers[2] = 25:", numbers[2])

-- Add element
numbers[6] = 60
print("numbers[6]:", numbers[6])
print("")

-- ----------------------------------------------------------------------------
-- 1.11 TABLES - DICTIONARIES
-- ----------------------------------------------------------------------------
print("--- 1.11 Tables - Dictionaries ---")

local person = {
    name = "Alice",
    age = 30,
    city = "Paris"
}

print("person.name:", person.name)
print("person['age']:", person["age"])
print("person.city:", person.city)

-- Modify
person.age = 31
print("after birthday, age:", person.age)

-- Add new key
person.job = "Engineer"
print("person.job:", person.job)
print("")

-- ----------------------------------------------------------------------------
-- 1.12 TABLES - MIXED AND COMPUTED KEYS
-- ----------------------------------------------------------------------------
print("--- 1.12 Tables - Mixed and Computed Keys ---")

local mixed = {
    100,                    -- [1] = 100
    200,                    -- [2] = 200
    name = "mixed table",   -- ["name"] = ...
    [3+3] = "computed",     -- [6] = "computed"
    ["key"] = "value"       -- explicit string key
}

print("mixed[1]:", mixed[1])
print("mixed[2]:", mixed[2])
print("mixed.name:", mixed.name)
print("mixed[6]:", mixed[6])
print("mixed['key']:", mixed["key"])
print("")

-- ----------------------------------------------------------------------------
-- 1.13 NESTED TABLES
-- ----------------------------------------------------------------------------
print("--- 1.13 Nested Tables ---")

local nested = {
    level1 = {
        level2 = {
            value = "deep value"
        }
    }
}

print("nested.level1.level2.value:", nested.level1.level2.value)
print("")

-- ----------------------------------------------------------------------------
-- 1.14 IF STATEMENTS
-- ----------------------------------------------------------------------------
print("--- 1.14 If Statements ---")

local score = 85

if score >= 90 then
    print("Grade: A")
elseif score >= 80 then
    print("Grade: B")
elseif score >= 70 then
    print("Grade: C")
else
    print("Grade: F")
end

-- Nested if
local num = 15
if num > 0 then
    if num % 2 == 0 then
        print(num, "is positive and even")
    else
        print(num, "is positive and odd")
    end
end
print("")

-- ----------------------------------------------------------------------------
-- 1.15 WHILE LOOPS
-- ----------------------------------------------------------------------------
print("--- 1.15 While Loops ---")

local i = 1
while i <= 5 do
    print("while i =", i)
    i = i + 1
end

-- While with break
local j = 0
while true do
    j = j + 1
    if j > 3 then
        break
    end
    print("while-break j =", j)
end
print("")

-- ----------------------------------------------------------------------------
-- 1.16 REPEAT-UNTIL LOOPS
-- ----------------------------------------------------------------------------
print("--- 1.16 Repeat-Until Loops ---")

local k = 1
repeat
    print("repeat k =", k)
    k = k + 1
until k > 4
print("")

-- ----------------------------------------------------------------------------
-- 1.17 NUMERIC FOR LOOPS
-- ----------------------------------------------------------------------------
print("--- 1.17 Numeric For Loops ---")

-- Basic for
for i = 1, 5 do
    print("for i =", i)
end

-- For with step
print("counting by 2:")
for i = 0, 10, 2 do
    print("  i =", i)
end

-- Descending for
print("counting down:")
for i = 5, 1, -1 do
    print("  i =", i)
end
print("")

-- ----------------------------------------------------------------------------
-- 1.18 GENERIC FOR LOOPS (FOR-IN)
-- ----------------------------------------------------------------------------
print("--- 1.18 Generic For Loops ---")

-- pairs - iterate all keys
local tbl = {a = 1, b = 2, c = 3}
print("pairs iteration:")
for k, v in pairs(tbl) do
    print("  key:", k, "value:", v)
end

-- ipairs - iterate array part
local arr2 = {10, 20, 30}
print("ipairs iteration:")
for i, v in ipairs(arr2) do
    print("  index:", i, "value:", v)
end
print("")

-- ----------------------------------------------------------------------------
-- 1.19 DO BLOCKS (SCOPING)
-- ----------------------------------------------------------------------------
print("--- 1.19 Do Blocks ---")

local outer = "outer"
do
    local inner = "inner"
    print("inside do: outer =", outer)
    print("inside do: inner =", inner)
end
print("outside do: outer =", outer)
-- inner is not accessible here
print("")

-- ----------------------------------------------------------------------------
-- 1.20 FUNCTIONS - BASIC
-- ----------------------------------------------------------------------------
print("--- 1.20 Functions - Basic ---")

-- Local function
local function add(x, y)
    return x + y
end

print("add(3, 4) =", add(3, 4))

-- Global function
function multiply(x, y)
    return x * y
end

print("multiply(3, 4) =", multiply(3, 4))

-- Function with no return (returns nil)
local function greet(name)
    print("Hello, " .. name .. "!")
end

greet("World")
print("")

-- ----------------------------------------------------------------------------
-- 1.21 FUNCTIONS - MULTIPLE RETURN VALUES
-- ----------------------------------------------------------------------------
print("--- 1.21 Multiple Return Values ---")

local function minmax(a, b, c)
    local min = a
    local max = a
    if b < min then min = b end
    if c < min then min = c end
    if b > max then max = b end
    if c > max then max = c end
    return min, max
end

local minimum, maximum = minmax(5, 2, 8)
print("min:", minimum, "max:", maximum)
print("")

-- ----------------------------------------------------------------------------
-- 1.22 FUNCTIONS - ANONYMOUS (LAMBDA)
-- ----------------------------------------------------------------------------
print("--- 1.22 Anonymous Functions ---")

local square = function(x)
    return x * x
end

print("square(7) =", square(7))

-- Immediately invoked
local result = (function(x) return x * 2 end)(21)
print("immediately invoked:", result)
print("")

-- ----------------------------------------------------------------------------
-- 1.23 FUNCTIONS - CLOSURES
-- ----------------------------------------------------------------------------
print("--- 1.23 Closures ---")

local function makeCounter()
    local count = 0
    return function()
        count = count + 1
        return count
    end
end

local counter = makeCounter()
print("counter():", counter())
print("counter():", counter())
print("counter():", counter())

-- Another closure example
local function makeAdder(x)
    return function(y)
        return x + y
    end
end

local add10 = makeAdder(10)
print("add10(5) =", add10(5))
print("add10(20) =", add10(20))
print("")

-- ----------------------------------------------------------------------------
-- 1.24 FUNCTIONS - VARARGS
-- ----------------------------------------------------------------------------
print("--- 1.24 Varargs (...) ---")

local function printAll(...)
    local args = {...}
    for i, v in ipairs(args) do
        print("  arg " .. i .. ":", v)
    end
end

print("printAll with 3 args:")
printAll("one", "two", "three")
print("")

-- ----------------------------------------------------------------------------
-- 1.25 FUNCTIONS - METHODS (COLON SYNTAX)
-- ----------------------------------------------------------------------------
print("--- 1.25 Methods (: syntax) ---")

local obj = {
    value = 10
}

function obj:increment(amount)
    self.value = self.value + amount
    return self.value
end

function obj:getValue()
    return self.value
end

print("initial value:", obj:getValue())
print("after increment(5):", obj:increment(5))
print("after increment(3):", obj:increment(3))
print("")

-- ----------------------------------------------------------------------------
-- 1.26 RECURSION
-- ----------------------------------------------------------------------------
print("--- 1.26 Recursion ---")

local function factorial(n)
    if n <= 1 then
        return 1
    else
        return n * factorial(n - 1)
    end
end

print("factorial(5) =", factorial(5))
print("factorial(10) =", factorial(10))

local function fibonacci(n)
    if n < 2 then
        return n
    end
    return fibonacci(n - 1) + fibonacci(n - 2)
end

print("fibonacci(10) =", fibonacci(10))
print("")

-- ----------------------------------------------------------------------------
-- 1.27 HIGHER-ORDER FUNCTIONS
-- ----------------------------------------------------------------------------
print("--- 1.27 Higher-Order Functions ---")

local function applyTwice(f, x)
    return f(f(x))
end

local function double(x)
    return x * 2
end

print("applyTwice(double, 3) =", applyTwice(double, 3))

-- Map-like function
local function map(tbl, f)
    local result = {}
    for i, v in ipairs(tbl) do
        result[i] = f(v)
    end
    return result
end

local nums = {1, 2, 3, 4, 5}
local squared = map(nums, function(x) return x * x end)
print("map square [1,2,3,4,5]:")
for i, v in ipairs(squared) do
    print("  ", v)
end
print("")

-- ----------------------------------------------------------------------------
-- 1.28 BUILT-IN FUNCTIONS
-- ----------------------------------------------------------------------------
print("--- 1.28 Built-in Functions ---")

-- tonumber
print("tonumber('123') =", tonumber("123"))
print("tonumber('3.14') =", tonumber("3.14"))
print("tonumber('hello') =", tonumber("hello"))

-- tostring
print("tostring(123) =", tostring(123))
print("tostring(true) =", tostring(true))
print("tostring(nil) =", tostring(nil))

-- select
print("select(2, 'a', 'b', 'c') =", select(2, "a", "b", "c"))
print("select('#', 'a', 'b', 'c') =", select("#", "a", "b", "c"))

-- next
local t = {a = 1, b = 2}
local k, v = next(t)
print("next(t) =", k, v)
print("")

-- ----------------------------------------------------------------------------
-- 1.29 ERROR HANDLING
-- ----------------------------------------------------------------------------
print("--- 1.29 Error Handling ---")

-- pcall - protected call
local function riskyFunction(x)
    if x < 0 then
        error("negative value not allowed")
    end
    return x * 2
end

local ok, result = pcall(riskyFunction, 10)
print("pcall with 10: ok =", ok, "result =", result)

local ok2, err = pcall(riskyFunction, -5)
print("pcall with -5: ok =", ok2, "error =", err)

-- assert
local function safeDivide(a, b)
    assert(b ~= 0, "division by zero!")
    return a / b
end

print("safeDivide(10, 2) =", safeDivide(10, 2))
print("")

-- ----------------------------------------------------------------------------
-- 1.30 RAWGET AND RAWSET
-- ----------------------------------------------------------------------------
print("--- 1.30 rawget and rawset ---")

local tbl2 = {x = 10, y = 20}
print("rawget(tbl2, 'x') =", rawget(tbl2, "x"))
print("rawget(tbl2, 'z') =", rawget(tbl2, "z"))

rawset(tbl2, "z", 30)
print("after rawset, tbl2.z =", tbl2.z)
print("")

-- ----------------------------------------------------------------------------
-- 1.31 TRUTHINESS
-- ----------------------------------------------------------------------------
print("--- 1.31 Truthiness Rules ---")

-- Only nil and false are falsy in Lua
if 0 then print("0 is truthy") end
if "" then print("empty string is truthy") end
if {} then print("empty table is truthy") end

if not nil then print("nil is falsy") end
if not false then print("false is falsy") end
print("")

print("=== END OF SUPPORTED FEATURES ===")
print("")
print("")

-- ============================================================================
-- SECTION 2: WHAT OUR INTERPRETER CANNOT DO YET
-- ============================================================================

print("=== SECTION 2: FEATURES NOT YET SUPPORTED ===")
print("")
print("The following code examples are COMMENTED OUT because they will fail.")
print("Uncomment them to see the errors.")
print("")

-- ----------------------------------------------------------------------------
-- 2.1 TABLE LIBRARY (not implemented)
-- ----------------------------------------------------------------------------
print("--- 2.1 Table Library (NOT SUPPORTED) ---")
print("  table.insert(t, value)")
print("  table.remove(t, index)")
print("  table.concat(t, separator)")
print("  table.sort(t)")
print("  table.unpack(t)")
print("")

-- WOULD FAIL:
-- local t = {1, 2, 3}
-- table.insert(t, 4)
-- print(table.concat(t, ", "))

-- ----------------------------------------------------------------------------
-- 2.2 STRING LIBRARY (not implemented)
-- ----------------------------------------------------------------------------
print("--- 2.2 String Library (NOT SUPPORTED) ---")
print("  string.sub(s, i, j)")
print("  string.len(s)")
print("  string.upper(s)")
print("  string.lower(s)")
print("  string.find(s, pattern)")
print("  string.gsub(s, pattern, replacement)")
print("  string.format(format, ...)")
print("  string.match(s, pattern)")
print("  string.byte(s)")
print("  string.char(n)")
print("")

-- WOULD FAIL:
-- local s = "Hello World"
-- print(string.upper(s))
-- print(string.sub(s, 1, 5))

-- ----------------------------------------------------------------------------
-- 2.3 MATH LIBRARY (not implemented)
-- ----------------------------------------------------------------------------
print("--- 2.3 Math Library (NOT SUPPORTED) ---")
print("  math.floor(x)")
print("  math.ceil(x)")
print("  math.abs(x)")
print("  math.sqrt(x)")
print("  math.sin(x), math.cos(x), math.tan(x)")
print("  math.random(), math.randomseed()")
print("  math.min(...), math.max(...)")
print("  math.pi")
print("")

-- WOULD FAIL:
-- print(math.floor(3.7))
-- print(math.sqrt(16))
-- print(math.random())

-- ----------------------------------------------------------------------------
-- 2.4 IO LIBRARY (not implemented)
-- ----------------------------------------------------------------------------
print("--- 2.4 IO Library (NOT SUPPORTED) ---")
print("  io.open(filename, mode)")
print("  io.read()")
print("  io.write()")
print("  io.lines(filename)")
print("  file:read(), file:write(), file:close()")
print("")

-- WOULD FAIL:
-- local file = io.open("test.txt", "r")
-- local content = file:read("*a")
-- file:close()

-- ----------------------------------------------------------------------------
-- 2.5 OS LIBRARY (not implemented)
-- ----------------------------------------------------------------------------
print("--- 2.5 OS Library (NOT SUPPORTED) ---")
print("  os.time()")
print("  os.date()")
print("  os.clock()")
print("  os.execute(command)")
print("  os.getenv(varname)")
print("  os.exit()")
print("")

-- WOULD FAIL:
-- print(os.time())
-- print(os.date("%Y-%m-%d"))

-- ----------------------------------------------------------------------------
-- 2.6 REQUIRE AND MODULES (not implemented)
-- ----------------------------------------------------------------------------
print("--- 2.6 Require/Modules (NOT SUPPORTED) ---")
print("  require('modulename')")
print("  dofile('filename.lua')")
print("  loadfile('filename.lua')")
print("  load(string)")
print("  package.path, package.loaded")
print("")

-- WOULD FAIL:
-- local json = require("json")
-- dofile("other_script.lua")

-- ----------------------------------------------------------------------------
-- 2.7 COROUTINES (not implemented)
-- ----------------------------------------------------------------------------
print("--- 2.7 Coroutines (NOT SUPPORTED) ---")
print("  coroutine.create(f)")
print("  coroutine.resume(co)")
print("  coroutine.yield(...)")
print("  coroutine.status(co)")
print("  coroutine.wrap(f)")
print("")

-- WOULD FAIL:
-- local co = coroutine.create(function()
--     print("hello from coroutine")
--     coroutine.yield()
--     print("resumed!")
-- end)
-- coroutine.resume(co)

-- ----------------------------------------------------------------------------
-- 2.8 METATABLES (partially implemented - stubs only)
-- ----------------------------------------------------------------------------
print("--- 2.8 Metatables (STUBS ONLY - no actual functionality) ---")
print("  setmetatable(t, mt) -- exists but doesn't work")
print("  getmetatable(t) -- exists but always returns nil")
print("  __index, __newindex, __add, __sub, etc.")
print("  __tostring, __call, __len")
print("")

-- setmetatable exists but does nothing useful:
-- local t = {}
-- local mt = {
--     __index = function(t, k) return "default" end
-- }
-- setmetatable(t, mt)
-- print(t.anything)  -- would print nil, not "default"

-- ----------------------------------------------------------------------------
-- 2.9 GOTO (parsed but not executed)
-- ----------------------------------------------------------------------------
print("--- 2.9 Goto (PARSED BUT NOT EXECUTED) ---")
print("  goto label")
print("  ::label::")
print("  (The parser accepts it, but evaluator ignores it)")
print("")

-- This won't work as expected:
-- goto skip
-- print("this should be skipped")
-- ::skip::
-- print("jumped here")

-- ----------------------------------------------------------------------------
-- 2.10 TOP-LEVEL RETURN (causes error)
-- ----------------------------------------------------------------------------
print("--- 2.10 Top-Level Return (CAUSES ERROR) ---")
print("  return value  -- at module level")
print("  (throws uncaught ReturnException)")
print("")

-- WOULD FAIL:
-- return 42

-- ----------------------------------------------------------------------------
-- 2.11 _G AND _ENV (not implemented)
-- ----------------------------------------------------------------------------
print("--- 2.11 _G and _ENV (NOT SUPPORTED) ---")
print("  _G -- global table")
print("  _ENV -- environment table")
print("  _G['varname'] = value")
print("")

-- WOULD FAIL:
-- print(_G)
-- _G["newGlobal"] = 123

-- ----------------------------------------------------------------------------
-- 2.12 DEBUG LIBRARY (not implemented)
-- ----------------------------------------------------------------------------
print("--- 2.12 Debug Library (NOT SUPPORTED) ---")
print("  debug.traceback()")
print("  debug.getinfo(f)")
print("  debug.getlocal()")
print("  debug.setlocal()")
print("")

-- ----------------------------------------------------------------------------
-- 2.13 BITWISE BINARY OPERATORS (not parsed)
-- ----------------------------------------------------------------------------
print("--- 2.13 Bitwise Binary Operators (NOT PARSED) ---")
print("  a & b   (bitwise AND)")
print("  a | b   (bitwise OR)")
print("  a ~ b   (bitwise XOR) -- note: unary ~ works")
print("  a << b  (left shift)")
print("  a >> b  (right shift)")
print("  (Evaluator supports them, but parser doesn't parse them)")
print("")

-- WOULD FAIL TO PARSE:
-- local x = 5 & 3
-- local y = 5 | 3
-- local z = 1 << 4

-- ----------------------------------------------------------------------------
-- 2.14 OTHER MISSING FUNCTIONS
-- ----------------------------------------------------------------------------
print("--- 2.14 Other Missing Functions ---")
print("  collectgarbage()")
print("  xpcall(f, errhandler)")
print("  setfenv(), getfenv() -- Lua 5.1 only")
print("  unpack() -- use table.unpack in 5.2+")
print("  loadstring() -- deprecated")
print("")

print("=== END OF UNSUPPORTED FEATURES ===")
print("")
print("Total: Our interpreter handles core Lua well!")
print("Missing: Standard libraries (table, string, math, io, os)")
print("         Advanced features (coroutines, metatables, require)")