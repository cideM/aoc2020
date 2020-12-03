#!/usr/bin/env nix-shell
--[[
#! nix-shell -p "lua53Packages.lua" -i lua
]]

local input = io.read("a")

local function toboggan(dx, dy)
	local trees = 0
	local step = 0
	local line_number = 0

	for line in input:gmatch("[^\r\n]+") do
		if line_number % dy == 0 then
			-- You can calculate how far right we should be by
			-- multiplying the right shift by the step we're
			-- currently in, not the line number!
			-- Wrap the right shift by using modulo
			local seek = (step * dx) % string.len(line) + 1
			if line:sub(seek, seek) == "#" then
				trees = trees + 1
			end
			step = step + 1
		end
		line_number = line_number + 1
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
