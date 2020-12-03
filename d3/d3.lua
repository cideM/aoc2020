#!/usr/bin/env nix-shell
--[[
#! nix-shell -p "lua53Packages.lua" -i lua
]]

local input = io.read("a")

function toboggan(dx, dy)
	local y = 1
	local trees = 0

	for line in input:gmatch("[^\r\n]+") do
		local x = y == 1 and 1 or 1 + (y - 1) / dy * dx
		local remainder = x % string.len(line)
		local x_wrap = remainder == 0 and string.len(line) or remainder
		local i = 1
		for c in string.gmatch(line, "[#.]") do
			if i == x_wrap and c == "#" then 
				trees = trees + 1 
			end
			i = i + 1
		end
		y = y + 1
	end

	return trees
end

print("Part 1: ", toboggan(3, 1))
print("Part 2:",
	toboggan(1, 1) *
	toboggan(3, 1) *
	toboggan(5, 1) *
	toboggan(7, 1) *
	toboggan(1, 2)
)
