#!/usr/bin/env nix-shell
--[[
#! nix-shell -p "lua53Packages.lua" -i lua
]]

function build_grid()
	local y = 0
	local grid = {bounds = { y = 0, x = 0 }}

	for line in io.lines() do
		y = y + 1
		local x = 0

		if not grid[y] then grid[y] = {} end
		setmetatable(grid[y], {
			__index = function (table, key)
				local k = key % grid.bounds.x
				if k == 0 then return table[grid.bounds.x] else return table[k] end
			end
		})

		for c in string.gmatch(line, "[#.]") do
			x = x + 1
			grid[y][x] = c
		end

		if x > grid.bounds.x then grid.bounds.x = x end
	end
	if y > grid.bounds.y then grid.bounds.y = y end

	return grid
end

local grid = build_grid()

function go(grid, dx, dy)
	local trees = 0
	local pos = { x = 1, y = 1 }
	while grid[pos.y] do
		if grid[pos.y][pos.x] == "#" then trees = trees + 1 end

		pos.x = pos.x + dx
		pos.y = pos.y + dy
	end

	return trees
end

print("Part 1:", go(grid, 3, 1))
print("Part 2:",
	go(grid, 1, 1) *
	go(grid, 3, 1) *
	go(grid, 5, 1) *
	go(grid, 7, 1) *
	go(grid, 1, 2)
)
