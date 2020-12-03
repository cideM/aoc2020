#!/usr/bin/env nix-shell
--[[
#! nix-shell -p "lua53Packages.lua" -i lua
]]

local s = io.read("*a")
local valid = 0
local valid_part_two = 0
-- Iterate linewise
for line in s:gmatch("[^\r\n]+") do
	local lower, upper, seek, input = line:match("(%d+)-(%d+) (%l): (%l+)")
	lower = tonumber(lower)
	upper = tonumber(upper)

	local count = 0
	for _ in string.gmatch(input, string.format("%s", seek)) do
		count = count + 1
	end

	if count >= lower and count <= upper then
		valid = valid + 1
	end

	-- No string indexing in lua, get substring from START to FINISH with
	-- both set to same index
	local pos1 = input:sub(lower, lower) == seek
	local pos2 = input:sub(upper, upper) == seek

	if (pos1 and not pos2) or (pos2 and not pos1) then
		valid_part_two = valid_part_two + 1
	end
end
print("Part One: ", valid)
print("Part Two: ", valid_part_two)
