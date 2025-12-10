-- ============================================================================
-- ADAPTED OFFICIAL LUA TESTS
-- These tests are simplified versions of the official Lua 5.4 test suite,
-- adapted to work with our interpreter (no standard libraries required)
-- ============================================================================

print("=== ADAPTED OFFICIAL TESTS ===")
print("")

local passed = 0
local failed = 0

-- Simple test helper (since we don't have string.format)
local function test(name, condition)
    if condition then
        print("[PASS] " .. name)
        passed = passed + 1
    else
        print("[FAIL] " .. name)
        failed = failed + 1
    end
end

-- ============================================================================
-- FROM constructs.lua: OPERATOR PRIORITIES
-- ============================================================================
print("--- Operator Priorities (from constructs.lua) ---")

test("power right-associative: 2^3^2 == 512", 2^3^2 == 512)
test("power before mult: 2^3*4 == 32", 2^3*4 == 32)
test("negative power: 2.0^-2 == 0.25", 2.0^-2 == 0.25)
test("logical: not nil and 2", not nil and 2 == 2)
test("comparison: not(2>3 or 3<2)", not(2>3 or 3<2))
test("subtraction chain: -3-1-5 == -9", -3-1-5 == -9)
test("unary minus with power: -2^2 == -4", -2^2 == -4)
test("parenthesized: (-2)^2 == 4", (-2)^2 == 4)
test("mixed arithmetic: 2*2-3-1 == 0", 2*2-3-1 == 0)
test("modulo negative: -3%5 == 2", -3%5 == 2)
test("concat with arith: 1+2 .. 3*1 == '33'", 1+2 .. 3*1 == "33")

-- Old bugs from official tests
test("old bug 1: (((1 or false) and true) or false) == true",
     (((1 or false) and true) or false) == true)
test("old bug 2: (((nil and true) or false) and true) == false",
     (((nil and true) or false) and true) == false)

-- Short-circuit and/or
local a, b = 1, nil
test("short-circuit 1: -(1 or 2) == -1", -(1 or 2) == -1)
test("short-circuit 2: (1 and 2)+(-1.25 or -4) == 0.75",
     (1 and 2)+(-1.25 or -4) == 0.75)

test("tonumber: 1234567890", 1234567890 == tonumber('1234567890'))
test("large number +1", 1234567890+1 == 1234567891)
print("")

-- ============================================================================
-- FROM constructs.lua: CONTROL STRUCTURES
-- ============================================================================
print("--- Control Structures (from constructs.lua) ---")

-- Testing while loops
local a = 0
while a < 10 do a = a + 1 end
test("while loop counts to 10", a == 10)

-- Testing repeat-until
a = 0
repeat a = a + 1 until a >= 5
test("repeat-until counts to 5", a == 5)

-- Testing for loop
local sum = 0
for i = 1, 10 do sum = sum + i end
test("for 1 to 10 sum = 55", sum == 55)

-- For with step
sum = 0
for i = 10, 1, -1 do sum = sum + i end
test("for 10 to 1 step -1 sum = 55", sum == 55)

-- Nested loops
local count = 0
for i = 1, 3 do
    for j = 1, 3 do
        count = count + 1
    end
end
test("nested for 3x3 = 9", count == 9)

-- While with break
a = 0
while true do
    a = a + 1
    if a >= 7 then break end
end
test("while with break at 7", a == 7)

-- For with break
sum = 0
for i = 1, 100 do
    if i > 5 then break end
    sum = sum + i
end
test("for with break: 1+2+3+4+5 = 15", sum == 15)
print("")

-- ============================================================================
-- FROM calls.lua: FUNCTION CALLS
-- ============================================================================
print("--- Function Calls (from calls.lua) ---")

-- Basic function
local function f(a, b)
    return a + b
end
test("basic function f(1,2) = 3", f(1, 2) == 3)

-- Function with more args than params
test("extra args ignored: f(1,2,3) = 3", f(1, 2, 3) == 3)

-- Function with fewer args than params (nil)
local function g(a, b, c)
    return a, b, c
end
local x, y, z = g(1, 2)
test("missing arg is nil", x == 1 and y == 2 and z == nil)

-- Recursive function
local function fac(n)
    if n <= 1 then return 1 end
    return n * fac(n - 1)
end
test("factorial(5) = 120", fac(5) == 120)
test("factorial(0) = 1", fac(0) == 1)

-- Mutual recursion
local odd, even
function even(n)
    if n == 0 then return true end
    return odd(n - 1)
end
function odd(n)
    if n == 0 then return false end
    return even(n - 1)
end
test("even(10) = true", even(10) == true)
test("odd(10) = false", odd(10) == false)
test("even(7) = false", even(7) == false)
test("odd(7) = true", odd(7) == true)
print("")

-- ============================================================================
-- FROM closure.lua: CLOSURES
-- ============================================================================
print("--- Closures (from closure.lua) ---")

-- Basic closure
local function counter()
    local n = 0
    return function()
        n = n + 1
        return n
    end
end

local c = counter()
test("closure counter() = 1", c() == 1)
test("closure counter() = 2", c() == 2)
test("closure counter() = 3", c() == 3)

-- Independent closures
local c1 = counter()
local c2 = counter()
test("independent closure c1 = 1", c1() == 1)
test("independent closure c2 = 1", c2() == 1)
test("independent closure c1 = 2", c1() == 2)
test("independent closure c2 = 2", c2() == 2)

-- Closure capturing loop variable
local funcs = {}
for i = 1, 3 do
    funcs[i] = function() return i end
end
-- Note: In Lua 5.x, loop var is local to each iteration
-- Our interpreter should handle this
print("")

-- ============================================================================
-- FROM locals.lua: LOCAL VARIABLES
-- ============================================================================
print("--- Local Variables (from locals.lua) ---")

-- Basic local scoping
local x = 10
do
    local x = 20
    test("inner scope x = 20", x == 20)
end
test("outer scope x = 10", x == 10)

-- Multiple locals
local a, b, c = 1, 2, 3
test("multiple locals", a == 1 and b == 2 and c == 3)

-- Shadowing
local shadow = "outer"
do
    local shadow = "inner"
    test("shadow inner", shadow == "inner")
end
test("shadow outer restored", shadow == "outer")

-- Local function
local function localfn()
    return "local"
end
test("local function", localfn() == "local")
print("")

-- ============================================================================
-- FROM nextvar.lua: TABLES
-- ============================================================================
print("--- Tables (from nextvar.lua) ---")

-- Empty table
local t = {}
test("empty table length = 0", #t == 0)

-- Array-like table
t = {10, 20, 30}
test("array t[1] = 10", t[1] == 10)
test("array t[2] = 20", t[2] == 20)
test("array t[3] = 30", t[3] == 30)
test("array length = 3", #t == 3)

-- Dictionary table
local d = {x = 1, y = 2, z = 3}
test("dict d.x = 1", d.x == 1)
test("dict d['y'] = 2", d["y"] == 2)

-- Mixed table
local m = {100, x = "hello", 200}
test("mixed m[1] = 100", m[1] == 100)
test("mixed m[2] = 200", m[2] == 200)
test("mixed m.x = 'hello'", m.x == "hello")

-- Computed keys
local k = {[1+1] = "two", [2+1] = "three"}
test("computed key k[2] = 'two'", k[2] == "two")
test("computed key k[3] = 'three'", k[3] == "three")

-- Nested tables
local n = {inner = {value = 42}}
test("nested n.inner.value = 42", n.inner.value == 42)

-- Table modification
t = {1, 2, 3}
t[4] = 4
test("table add element", t[4] == 4)
t[2] = 20
test("table modify element", t[2] == 20)

-- Table as function arg
local function sumtable(tbl)
    local s = 0
    for i = 1, #tbl do
        s = s + tbl[i]
    end
    return s
end
test("sum table {1,2,3,4} = 10", sumtable({1, 2, 3, 4}) == 10)
print("")

-- ============================================================================
-- FROM vararg.lua: VARARGS
-- ============================================================================
print("--- Varargs (from vararg.lua) ---")

local function countargs(...)
    local args = {...}
    return #args
end
test("countargs(1,2,3) = 3", countargs(1, 2, 3) == 3)
test("countargs() = 0", countargs() == 0)
test("countargs(1,2,3,4,5) = 5", countargs(1, 2, 3, 4, 5) == 5)

local function sumargs(...)
    local args = {...}
    local s = 0
    for i = 1, #args do
        s = s + args[i]
    end
    return s
end
test("sumargs(1,2,3) = 6", sumargs(1, 2, 3) == 6)
test("sumargs(10,20,30,40) = 100", sumargs(10, 20, 30, 40) == 100)

-- Test varargs forwarding
local function forward(...)
    return countargs(...)
end
test("forward(1,2,3,4) = 4", forward(1, 2, 3, 4) == 4)
print("")

-- ============================================================================
-- ARITHMETIC EDGE CASES
-- ============================================================================
print("--- Arithmetic Edge Cases ---")

test("floor div: 7 // 3 = 2", 7 // 3 == 2)
test("floor div negative: -7 // 3 = -3", -7 // 3 == -3)
test("modulo: 7 % 3 = 1", 7 % 3 == 1)
test("modulo negative: -7 % 3 = 2", -7 % 3 == 2)
test("power: 2^10 = 1024", 2^10 == 1024)
test("power of 0: 0^0 = 1", 0^0 == 1)
test("power negative exp: 2^-1 = 0.5", 2^-1 == 0.5)
print("")

-- ============================================================================
-- COMPARISON EDGE CASES
-- ============================================================================
print("--- Comparison Edge Cases ---")

test("equal numbers", 1 == 1.0)
test("string comparison lt", "a" < "b")
test("string comparison gt", "b" > "a")
test("string comparison eq", "hello" == "hello")
test("nil == nil", nil == nil)
test("false ~= nil", false ~= nil)
test("0 ~= false", 0 ~= false)
test("'' ~= false", "" ~= false)
print("")

-- ============================================================================
-- LOGICAL OPERATOR EDGE CASES
-- ============================================================================
print("--- Logical Operators Edge Cases ---")

test("and returns first falsy", (nil and 1) == nil)
test("and returns first falsy 2", (false and 1) == false)
test("and returns second if first truthy", (1 and 2) == 2)
test("or returns first truthy", (1 or 2) == 1)
test("or returns second if first falsy", (nil or 2) == 2)
test("or returns second if first falsy 2", (false or "x") == "x")
test("not nil = true", not nil == true)
test("not false = true", not false == true)
test("not 0 = false", not 0 == false)
test("not '' = false", not "" == false)
print("")

-- ============================================================================
-- SUMMARY
-- ============================================================================
print("===========================================")
print("RESULTS: " .. passed .. " passed, " .. failed .. " failed")
print("===========================================")

if failed == 0 then
    print("All tests passed!")
else
    print("Some tests failed - check implementation")
end