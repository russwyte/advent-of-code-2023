package aoc.solutions

extension (a: BigInt) def lcm(b: BigInt): BigInt = a * b / a.gcd(b)
