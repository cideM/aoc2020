#!/usr/bin/env nix-shell
--[[
#! nix-shell -p "lua53Packages.lua" -i lua
]]

local s = io.read("*a")

local lines = {}

for line in s:gmatch("[^\r\n]+") do
	table.insert(lines, tonumber(line))
end

for _, a in ipairs(lines) do
	for _, b in ipairs(lines) do
		if a + b == 2020 then
			print(a * b)
			goto done_one
		end
	end
end
::done_one::

for _, a in ipairs(lines) do
	for _, b in ipairs(lines) do
		for _, c in ipairs(lines) do
			if a + b + c == 2020 then
				print(a * b * c)
				goto done_two
			end
		end
	end
end
::done_two::
